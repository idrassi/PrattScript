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

static void statement_vector_append(StatementVector* vec, Statement* stmt) {
    if (vec->capacity < vec->count + 1) {
        int old_capacity = vec->capacity;
        vec->capacity = old_capacity < 8 ? 8 : old_capacity * 2;
        vec->statements = realloc(vec->statements, sizeof(Statement*) * vec->capacity);
        if (vec->statements == NULL) {
            fprintf(stderr, "Fatal: out of memory growing statement vector.\n");
            exit(1);
        }
    }
    vec->statements[vec->count++] = stmt;
}

static void statement_vector_free(StatementVector* vec) {
    free(vec->statements);
    statement_vector_init(vec);
}
// --- End Dynamic Array ---


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

    // 1. Initialize Parser
    PrattLexer lex;
    pratt_lexer_init(&lex, source);
    Parser parser;
    parser_init(&parser, pratt_lexer_next, &lex, &interp, default_rules, default_rule_count, default_token_name, &interp.arena);

    // 2. Parse all statements into our dynamic vector.
    StatementVector program;
    statement_vector_init(&program);

    while (parser.next.type != T_EOF) {
        Statement* stmt = parse_statement(&parser);
        if (parser.had_error) {
            // A parse error occurred. Report it and stop.
            fprintf(stderr, "Parse Error: %s\n", parser.last_error.message);
            parser_destroy(&parser);
            statement_vector_free(&program);
            interpreter_destroy(&interp);
            return 1;
        }
        if (stmt) {
            statement_vector_append(&program, stmt);
        }
    }
    
    // The parser is no longer needed; its arena-allocated error message
    // is now invalid, but the AST lives on in the interpreter's arena.
    parser_destroy(&parser);

    // 3. Interpret the program
    for (int i = 0; i < program.count; i++) {
        execute(&interp, program.statements[i]);
        if (interp.had_error) {
            fprintf(stderr, "Runtime Error: %s\n", interp.error_message);
            statement_vector_free(&program);
            interpreter_destroy(&interp);
            return 1;
        }
    }

    // 4. Clean up
    statement_vector_free(&program);
    interpreter_destroy(&interp);
    return 0; // Success
}

static void run_repl() {
    printf("PrattScript Interpreter REPL. Enter 'exit' or press Ctrl+D to quit.\n");
    char line[2048];

    // For a REPL, we create one long-lived interpreter instance.
    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);

    for (;;) {
        printf("> ");
        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n"); // Handle Ctrl+D
            break;
        }

        if (strncmp(line, "exit", 4) == 0) {
            break;
        }

        PrattLexer lex;
        pratt_lexer_init(&lex, line);
        Parser parser;
        parser_init(&parser, pratt_lexer_next, &lex, &interp, default_rules, default_rule_count, default_token_name, &interp.arena);
        
        // Reset error state for this line
        interp.had_error = 0;
        
        while (parser.next.type != T_EOF) {
            Statement* stmt = parse_statement(&parser);
            if (parser.had_error) {
                fprintf(stderr, "Parse Error: %s\n", parser.last_error.message);
                break; // Stop processing this line
            }
            if (stmt) {
                ExecResult res = execute(&interp, stmt);
                // For a REPL, it's conventional to print the value of the last expression statement.
                if (stmt->type == ST_EXPR) {
                   // Note: `eval` would be more appropriate here, but execute() is what we have.
                   // A proper REPL might `eval` and `print` if the statement is an EXPR_STMT.
                }

                if (interp.had_error) {
                    fprintf(stderr, "Runtime Error: %s\n", interp.error_message);
                    break;
                }
                 if (res.status == EXEC_RETURN) {
                    printf("=> ");
                    print_value(res.value);
                    printf("\n");
                }
            }
        }
        parser_destroy(&parser);
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
    FILE* file = fopen(path, "r");
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
    fclose(file);
    if (bytes_read == 0 || ferror(file)) {
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
