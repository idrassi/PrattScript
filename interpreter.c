/*
 *  PrattScript - Dynamic Scripting Language & Pratt Parser Core
 *  -----------------------------------------------------------
 *  PrattScript is a lightweight, dynamically typed scripting language
 *  featuring first-class functions, arrays, objects, and garbage collection.
 *  Its core is a robust C99 implementation of Pratt's Top-Down Operator-Precedence
 *  parsing algorithm, supporting explicit binding-power rules, extensible syntax,
 *  and pluggable lexers and parse tables. The interpreter includes an arena allocator,
 *  string interning, and a mark-sweep garbage collector, making it suitable for
 *  embedding, rapid prototyping, and language research.
 *
 *  Homepage : https://github.com/idrassi/PrattScript
 *  License  : MIT (c) see LICENSE or <https://opensource.org/licenses/MIT>
 *
 *  Copyright (c) 2025 Mounir IDRASSI <mounir.idrassi@amcrypto.jp>
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *      The above copyright notice and this permission notice shall be included
 *      in all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

#include "pratt_default.h"
#include "pratt_lexer.h"
#include "interpreter_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define DEFAULT_INITIAL_ARENA_SIZE 8192

// --- Dynamic Array for Statements ---
// A simple vector to hold a dynamic list of Statement pointers
typedef struct {
    Statement** statements;
    int count;
    int capacity;
} StatementVector;

static void statement_vector_init(StatementVector* vec) {
    vec->statements = NULL;
    vec->count = 0;
    vec->capacity = 0;
}

static bool statement_vector_append(StatementVector* vec, Statement* stmt, Interpreter* interp) {
    if (vec->capacity < vec->count + 1) {
        int old_capacity = vec->capacity;
        int new_capacity = old_capacity < 8 ? 8 : old_capacity * 2;
        Statement** new_statements =
            realloc(vec->statements, sizeof(Statement*) * (size_t)new_capacity);
        if (new_statements == NULL) {
            runtime_error(interp, "Out of memory growing statement vector.");
            return false;
        }
        vec->statements = new_statements;
        vec->capacity = new_capacity;
    }
    vec->statements[vec->count++] = stmt;
    return true;
}

static void statement_vector_free(StatementVector* vec) {
    free(vec->statements);
    statement_vector_init(vec);
}
// --- End Dynamic Array ---

static bool ast_contains_function(ASTNode* node) {
    if (node == NULL) return false;

    switch (node->type) {
        case AST_FUNCTION:
            return true;
        case AST_BINARY:
            return ast_contains_function(node->as.binary.left) ||
                   ast_contains_function(node->as.binary.right);
        case AST_UNARY:
            return ast_contains_function(node->as.unary.child);
        case AST_TERNARY:
            return ast_contains_function(node->as.ternary.cond) ||
                   ast_contains_function(node->as.ternary.then_branch) ||
                   ast_contains_function(node->as.ternary.else_branch);
        case AST_ASSIGN:
            return ast_contains_function(node->as.assign.target) ||
                   ast_contains_function(node->as.assign.value);
        case AST_CALL:
            if (ast_contains_function(node->as.call.callee)) return true;
            for (size_t i = 0; i < node->as.call.argc; ++i) {
                if (ast_contains_function(node->as.call.args[i])) return true;
            }
            return false;
        case AST_ARRAY:
            for (size_t i = 0; i < node->as.array.count; ++i) {
                if (ast_contains_function(node->as.array.elements[i])) return true;
            }
            return false;
        case AST_OBJECT:
            for (size_t i = 0; i < node->as.object.count; ++i) {
                if (ast_contains_function(node->as.object.values[i])) return true;
            }
            return false;
        case AST_INDEX:
            return ast_contains_function(node->as.index.object) ||
                   ast_contains_function(node->as.index.index);
        case AST_NUMBER:
        case AST_STRING:
        case AST_IDENT:
        case AST_BOOL:
        case AST_NIL:
            return false;
    }

    return false;
}

static bool statement_contains_function(Statement* stmt) {
    if (stmt == NULL) return false;

    switch (stmt->type) {
        case ST_FUNCTION:
            return true;
        case ST_EXPR:
            return ast_contains_function(stmt->as.expr.expr);
        case ST_VAR:
            return ast_contains_function(stmt->as.var.initializer);
        case ST_BLOCK:
            for (size_t i = 0; i < stmt->as.block.count; ++i) {
                if (statement_contains_function(stmt->as.block.list[i])) return true;
            }
            return false;
        case ST_IF:
            return ast_contains_function(stmt->as.if_s.condition) ||
                   statement_contains_function(stmt->as.if_s.then_branch) ||
                   statement_contains_function(stmt->as.if_s.else_branch);
        case ST_WHILE:
            return ast_contains_function(stmt->as.while_s.condition) ||
                   statement_contains_function(stmt->as.while_s.body);
        case ST_FOR:
            return statement_contains_function(stmt->as.for_s.initializer) ||
                   ast_contains_function(stmt->as.for_s.condition) ||
                   ast_contains_function(stmt->as.for_s.increment) ||
                   statement_contains_function(stmt->as.for_s.body);
        case ST_RETURN:
            return ast_contains_function(stmt->as.ret.value);
        case ST_BREAK:
        case ST_CONTINUE:
            return false;
    }

    return false;
}

static bool program_contains_function(const StatementVector* program) {
    for (int i = 0; i < program->count; ++i) {
        if (statement_contains_function(program->statements[i])) return true;
    }
    return false;
}

static bool parse_program(Interpreter* interp,
                          const char* source,
                          Arena* arena,
                          StatementVector* program,
                          const char** error_message) {
    PrattLexer lex;
    pratt_lexer_init(&lex, source);

    Parser parser;
    parser_init(&parser,
                pratt_lexer_next,
                &lex,
                interp,
                default_rules,
                default_rule_count,
                default_token_name,
                arena);

    while (parser.next.type != T_EOF) {
        Statement* stmt = parse_statement(&parser);
        if (parser.had_error) {
            if (error_message != NULL) {
                *error_message = parser.last_error.message;
            }
            parser_destroy(&parser);
            return false;
        }
        if (stmt && !statement_vector_append(program, stmt, interp)) {
            if (error_message != NULL) {
                *error_message = interp->error_message;
            }
            parser_destroy(&parser);
            return false;
        }
    }

    parser_destroy(&parser);
    return true;
}

static int execute_program(Interpreter* interp, const StatementVector* program) {
    for (int i = 0; i < program->count; i++) {
        ExecResult res = execute(interp, program->statements[i]);
        if (interp->had_error) {
            fprintf(stderr, "Runtime Error: %s\n", interp->error_message);
            return 1;
        }
        if (res.status == EXEC_BREAK) {
            runtime_error(interp, "Cannot 'break' outside of a loop.");
            fprintf(stderr, "Runtime Error: %s\n", interp->error_message);
            return 1;
        }
        if (res.status == EXEC_CONTINUE) {
            runtime_error(interp, "Cannot 'continue' outside of a loop.");
            fprintf(stderr, "Runtime Error: %s\n", interp->error_message);
            return 1;
        }
        if (res.status == EXEC_RETURN) {
            runtime_error(interp, "Cannot 'return' from top-level code.");
            fprintf(stderr, "Runtime Error: %s\n", interp->error_message);
            return 1;
        }
    }

    return 0;
}

// Top-level function to execute a block of source code.
// It handles the entire lifecycle: init, parse, execute, destroy.
// Returns an exit code: 0 for success, 1 for failure.
static int execute_source(const char* source) {
    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);

    if (interp.had_error) {
        fprintf(stderr, "Initialization Error: %s\n", interp.error_message);
        interpreter_destroy(&interp);
        return 1;
    }

    StatementVector program;
    statement_vector_init(&program);

    const char* parse_error = NULL;
    if (!parse_program(&interp, source, &interp.arena, &program, &parse_error)) {
        if (interp.had_error) {
            fprintf(stderr, "Error: %s\n", interp.error_message);
        } else {
            fprintf(stderr, "Parse Error: %s\n", parse_error);
        }
        statement_vector_free(&program);
        interpreter_destroy(&interp);
        return 1;
    }

    int result = execute_program(&interp, &program);
    statement_vector_free(&program);
    interpreter_destroy(&interp);
    return result;
}

static void run_repl() {
    printf("PrattScript Interpreter REPL. Enter 'exit' or press Ctrl+D to quit.\n");
    char line[2048];

    // For a REPL, we create one long-lived interpreter instance.
    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);
    if (interp.had_error) {
        fprintf(stderr, "Initialization Error: %s\n", interp.error_message);
        interpreter_destroy(&interp);
        return;
    }

    for (;;) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n"); // Handle Ctrl+D
            break;
        }

        if (strncmp(line, "exit", 4) == 0) {
            break;
        }

        // Reset error state for this line
        interp.had_error = 0;
        interp.error_message[0] = '\0';

        Arena line_arena;
        arena_init(&line_arena, DEFAULT_INITIAL_ARENA_SIZE);

        StatementVector program;
        statement_vector_init(&program);

        const char* parse_error = NULL;
        if (!parse_program(&interp, line, &line_arena, &program, &parse_error)) {
            if (interp.had_error) {
                fprintf(stderr, "Error: %s\n", interp.error_message);
            } else {
                fprintf(stderr, "Parse Error: %s\n", parse_error);
            }
            statement_vector_free(&program);
            arena_free(&line_arena);
            continue;
        }

        if (program_contains_function(&program)) {
            statement_vector_free(&program);
            arena_free(&line_arena);

            statement_vector_init(&program);
            parse_error = NULL;
            if (!parse_program(&interp, line, &interp.arena, &program, &parse_error)) {
                if (interp.had_error) {
                    fprintf(stderr, "Error: %s\n", interp.error_message);
                } else {
                    fprintf(stderr, "Parse Error: %s\n", parse_error);
                }
                statement_vector_free(&program);
                continue;
            }

            (void)execute_program(&interp, &program);
            statement_vector_free(&program);
            continue;
        }

        (void)execute_program(&interp, &program);
        statement_vector_free(&program);
        arena_free(&line_arena);
    }
    
    interpreter_destroy(&interp);
}


static char* read_file(const char* path) {
    struct stat statbuffer;
    size_t file_size;
    if (stat(path, &statbuffer) == 0) {
        file_size = (size_t)statbuffer.st_size;
    } else {
        fprintf(stderr, "Error: Could not get file size for \"%s\".\n", path);
        return NULL;
    }
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error: Could not open file \"%s\".\n", path);
        return NULL;
    }

    char* buffer = (char*)malloc(file_size + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Error: Not enough memory to read \"%s\".\n", path);
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
    int read_error = ferror(file);
    fclose(file);
    if (read_error || bytes_read != file_size) {
        fprintf(stderr, "Error: Could not read the file \"%s\".\n", path);
        free(buffer);
        return NULL;
    }
    
    buffer[bytes_read] = '\0';
    return buffer;
}

static int run_file(const char* path) {
    char* source = read_file(path);
    if (source == NULL) {
        return 1; // read_file already printed the error
    }

    int result = execute_source(source);
    free(source);
    return result;
}

int main(int argc, const char* argv[]) {
    if (argc == 1) {
        // No arguments, run the REPL
        run_repl();
    } else if (argc == 2) {
        // One argument, assume it's a file path
        return run_file(argv[1]);
    } else if (argc == 3 && strcmp(argv[1], "-c") == 0) {
        // -c flag, execute the following string
        return execute_source(argv[2]);
    } else {
        fprintf(stderr, "Usage: %s [path]\n", argv[0]);
        fprintf(stderr, "       %s -c \"source code\"\n", argv[0]);
        fprintf(stderr, "       %s\n", argv[0]);
        return 64; // EX_USAGE
    }

    return 0;
}
