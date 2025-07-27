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

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_DEPRECATE
#include <io.h>
#include <fcntl.h>
#include <direct.h>
#else
#include <unistd.h>
#include <sys/stat.h>
#endif
#include "pratt_default.h"
#include "pratt_lexer.h"
#include "interpreter_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <inttypes.h>

// For date.parse test
#if !defined(_WIN32) && !defined(_MSC_VER)
#define HAVE_STRPTIME
#endif

#define DEFAULT_INITIAL_ARENA_SIZE 8192

/*── Test framework helpers ──────────────────────────────────────────────*/
static int test_count = 0;
static int test_passed = 0;
static int test_failed = 0;

#define TEST_START(name) \
    do { \
        test_count++; \
        printf("Test %d: %s... ", test_count, name); \
        fflush(stdout); \
    } while(0)

#define TEST_PASS() \
    do { \
        test_passed++; \
        printf("PASS\n"); \
    } while(0)

#define TEST_FAIL(msg, ...) \
    do { \
        test_failed++; \
        printf("FAIL - " msg "\n", ##__VA_ARGS__); \
    } while(0)

#define ASSERT(condition, msg, ...) \
    do { \
        if (!(condition)) { \
            TEST_FAIL(msg, ##__VA_ARGS__); \
            return; \
        } \
    } while(0)

/* ── Lifetime-management helpers ─────────────────────────────────────── */
/*  We collect every Parser that parse_expression_test() creates so that
    we can destroy them *after* the AST has been inspected.              */
typedef struct {
    Parser parser;
    Interpreter interpreter;
} TestHarness;

#define MAX_TRACKED_HARNESSES 1024
static TestHarness *tracked_harnesses[MAX_TRACKED_HARNESSES];
static int          tracked_count = 0;

static PrattToken bad_token_lexer(void *ctx) {
    // The context `ctx` is a pointer to the token ID we want to emit.
    int token_id = *(int*)ctx;
    // Return a token with the bad ID. Other fields are dummy values.
    return (PrattToken){.type = token_id, .start = "X", .length = 1, .line=1, .col=1};
}

static void track_harness(TestHarness *h) {
    if (tracked_count < MAX_TRACKED_HARNESSES) tracked_harnesses[tracked_count++] = h;
}

static void destroy_tracked_harnesses(void) {
    for (int i = 0; i < tracked_count; ++i) {
        parser_destroy(&tracked_harnesses[i]->parser);
        interpreter_destroy(&tracked_harnesses[i]->interpreter);
        free(tracked_harnesses[i]);
    }
    tracked_count = 0;
}

/*── AST printing for debugging ──────────────────────────────────────────*/
static void print_statement_ast(Statement *stmt, int depth);

static void print_ast_indent(ASTNode *node, int depth) {
    if (!node) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("NULL\n");
        return;
    }
    
    for (int i = 0; i < depth; i++) printf("  ");
    
    switch (node->type) {
        case AST_NUMBER:
            if (node->as.number.is_double) {
                printf("NUMBER(double: %g)\n", node->as.number.as.d_val);
            } else {
                printf("NUMBER(int: %" PRId64 ")\n", node->as.number.as.i_val);
            }
            break;
        case AST_STRING:
            printf("STRING(\"%.*s\")\n", (int)node->as.string.length, node->as.string.value);
            break;
        case AST_IDENT:
            printf("IDENT(%s)\n", node->as.ident.name);
            break;
        case AST_BOOL:
            printf("BOOL(%s)\n", node->as.boolean.value ? "true" : "false");
            break;
        case AST_NIL:
            printf("NIL\n");
            break;
        case AST_BINARY:
            printf("BINARY(%.*s)\n", 
                   (int)node->as.binary.op.length, 
                   node->as.binary.op.start);
            print_ast_indent(node->as.binary.left, depth + 1);
            print_ast_indent(node->as.binary.right, depth + 1);
            break;
        case AST_UNARY:
            printf("UNARY(%.*s)\n", 
                   (int)node->as.unary.op.length, 
                   node->as.unary.op.start);
            print_ast_indent(node->as.unary.child, depth + 1);
            break;
        case AST_TERNARY:
            printf("TERNARY(?)\n");
            print_ast_indent(node->as.ternary.cond, depth + 1);
            print_ast_indent(node->as.ternary.then_branch, depth + 1);
            print_ast_indent(node->as.ternary.else_branch, depth + 1);
            break;
        case AST_CALL:
            printf("CALL\n");
            for (int i=0; i<depth+1; ++i) printf("  "); printf("CALLEE:\n");
            print_ast_indent(node->as.call.callee, depth + 2);
            if (node->as.call.argc > 0) {
                for (int i=0; i<depth+1; ++i) printf("  "); printf("ARGS[%zu]:\n", node->as.call.argc);
                for (size_t i = 0; i < node->as.call.argc; ++i) {
                    print_ast_indent(node->as.call.args[i], depth + 2);
                }
            }
            break;
        case AST_ARRAY:
            printf("ARRAY[%zu]\n", node->as.array.count);
            for (size_t i = 0; i < node->as.array.count; ++i) {
                print_ast_indent(node->as.array.elements[i], depth + 1);
            }
            break;
        case AST_OBJECT:
            printf("OBJECT[%zu]\n", node->as.object.count);
            for (size_t i = 0; i < node->as.object.count; ++i) {
                for (int j=0; j<depth+1; ++j) printf("  ");
                printf("KEY: \"%s\"\n", node->as.object.keys[i]);
                print_ast_indent(node->as.object.values[i], depth + 2);
            }
            break;
        case AST_INDEX:
            printf("INDEX\n");
            for (int i=0; i<depth+1; ++i) printf("  "); printf("OBJECT:\n");
            print_ast_indent(node->as.index.object, depth + 2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("INDEX:\n");
            print_ast_indent(node->as.index.index, depth + 2);
            break;
        case AST_FUNCTION:
            printf("FUNCTION_EXPR(%s)\n", node->as.function.name ? node->as.function.name : "<anonymous>");
            if (node->as.function.param_count > 0) {
                for (int i = 0; i < depth + 1; i++) printf("  ");
                printf("PARAMS[%zu]:", node->as.function.param_count);
                for (size_t i = 0; i < node->as.function.param_count; ++i) {
                    printf(" %s", node->as.function.params[i]);
               }
                printf("\n");
            }
            if (node->as.function.body) {
                for (int i = 0; i < depth + 1; i++) printf("  ");
                printf("BODY:\n");
               print_statement_ast(node->as.function.body, depth + 2);
            }
            break;
        case AST_ASSIGN:
            printf("ASSIGN(%.*s)\n", (int)node->as.assign.op.length, node->as.assign.op.start);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("TARGET:\n");
            print_ast_indent(node->as.assign.target, depth + 2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("VALUE:\n");
            print_ast_indent(node->as.assign.value, depth + 2);
            break;
    }
}

static void print_ast(ASTNode *node) {
    print_ast_indent(node, 0);
}

// --- Statement AST printer for debugging ---
static void print_statement_list(Statement **list, size_t count, int depth) {
    for (size_t i = 0; i < count; ++i) {
        print_statement_ast(list[i], depth);
    }
}

static void print_statement_ast(Statement *stmt, int depth) {
    if (!stmt) {
        for (int i = 0; i < depth; i++) printf("  ");
        printf("NULL_STMT\n");
        return;
    }
    for (int i = 0; i < depth; i++) printf("  ");
    switch(stmt->type) {
        case ST_EXPR:
            printf("EXPR_STMT:\n");
            print_ast_indent(stmt->as.expr.expr, depth + 1);
            break;
        case ST_VAR:
            printf("VAR_STMT(%s)\n", stmt->as.var.name);
            if (stmt->as.var.initializer) {
                print_ast_indent(stmt->as.var.initializer, depth + 1);
            }
            break;
        case ST_BLOCK:
            printf("BLOCK_STMT:\n");
            print_statement_list(stmt->as.block.list, stmt->as.block.count, depth + 1);
            break;
        case ST_IF:
            printf("IF_STMT:\n");
            for (int i=0; i<depth+1; ++i) printf("  "); printf("COND:\n");
            print_ast_indent(stmt->as.if_s.condition, depth+2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("THEN:\n");
            print_statement_ast(stmt->as.if_s.then_branch, depth+2);
            if (stmt->as.if_s.else_branch) {
                for (int i=0; i<depth+1; ++i) printf("  "); printf("ELSE:\n");
                print_statement_ast(stmt->as.if_s.else_branch, depth+2);
            }
            break;
        case ST_WHILE:
            printf("WHILE_STMT:\n");
            for (int i=0; i<depth+1; ++i) printf("  "); printf("COND:\n");
            print_ast_indent(stmt->as.while_s.condition, depth+2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("BODY:\n");
            print_statement_ast(stmt->as.while_s.body, depth+2);
            break;
        case ST_FOR:
            printf("FOR_STMT:\n");
            for (int i=0; i<depth+1; ++i) printf("  "); printf("INIT:\n");
            print_statement_ast(stmt->as.for_s.initializer, depth+2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("COND:\n");
            print_ast_indent(stmt->as.for_s.condition, depth+2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("INCR:\n");
            print_ast_indent(stmt->as.for_s.increment, depth+2);
            for (int i=0; i<depth+1; ++i) printf("  "); printf("BODY:\n");
            print_statement_ast(stmt->as.for_s.body, depth+2);
            break;
        case ST_BREAK:
            printf("BREAK_STMT\n");
            break;
        case ST_CONTINUE:
            printf("CONTINUE_STMT\n");
            break;
        case ST_RETURN:
            printf("RETURN_STMT:\n");
            if(stmt->as.ret.value) print_ast_indent(stmt->as.ret.value, depth+1);
            break;
        case ST_FUNCTION:
             printf("FUNCTION_STMT(%s)\n", stmt->as.func.name);
             // TODO: Print params
             print_statement_ast(stmt->as.func.body, depth+1);
             break;
    }
}

/*── Test execution helper ───────────────────────────────────────────────*/
static ASTNode *parse_expression_test(const char *source) {
    PrattLexer lex;
    pratt_lexer_init(&lex, source);
    
    /* Allocate a harness (Parser + Interpreter + Arena) on the heap. */
    TestHarness *harness = malloc(sizeof(TestHarness));
    if (!harness) { fputs("Out of memory\n", stderr); exit(EXIT_FAILURE); }
    
    interpreter_init(&harness->interpreter, 1024 * 4);
    parser_init(&harness->parser, pratt_lexer_next, &lex, &harness->interpreter,
                default_rules, default_rule_count, default_token_name, &harness->interpreter.arena);

    ASTNode *result = parse_expression(&harness->parser);

    if (harness->parser.had_error) {
        printf("\nParser error: %s\n", harness->parser.last_error.message ? harness->parser.last_error.message : "Unknown error");
        if (result) print_ast(result);
    }
    
    /* Do NOT destroy the harness yet – the AST is still in use. */
    track_harness(harness);
    return result;
}

// --- Test helper for statements ---
static Statement *parse_program_test(const char *source, Parser* out_parser, Interpreter* out_interp) {
    PrattLexer lex;

    // To test parsing of multiple statements, we wrap them in a block
    char block_source[4096];
    snprintf(block_source, sizeof(block_source), "{ %s }", source);
    pratt_lexer_init(&lex, block_source);

    interpreter_init(out_interp, 1024 * 4);
    parser_init(out_parser, pratt_lexer_next, &lex, out_interp,
                default_rules, default_rule_count, default_token_name, &out_interp->arena);

    Statement *result = parse_statement(out_parser);

    if (out_parser->had_error) {
        printf("\nParser error: %s\n", out_parser->last_error.message ? out_parser->last_error.message : "Unknown error");
        if (result) print_statement_ast(result, 0);
    }
    
    return result;
}

/*── Basic functionality tests ───────────────────────────────────────────*/
static void test_simple_number() {
    TEST_START("Simple integer parsing");
    ASTNode *ast = parse_expression_test("42");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_NUMBER, "Expected number node");
    ASSERT(!ast->as.number.is_double, "Expected integer, not double");
    ASSERT(ast->as.number.as.i_val == 42, "Expected value 42, got %" PRId64, ast->as.number.as.i_val);
    TEST_PASS();
}

static void test_simple_string() {
    TEST_START("Simple string parsing");
    ASTNode *ast = parse_expression_test("\"hello world\"");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_STRING, "Expected string node, got %d", ast->type);
    ASSERT(strcmp(ast->as.string.value, "hello world") == 0,
           "Expected value 'hello world', got '%s'", ast->as.string.value);
    TEST_PASS();
}

static void test_decimal_number() {
    TEST_START("Decimal number parsing");
    ASTNode *ast = parse_expression_test("3.14159");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_NUMBER, "Expected number node");
    ASSERT(ast->as.number.is_double, "Expected double, not integer");
    ASSERT(ast->as.number.as.d_val > 3.14 && ast->as.number.as.d_val < 3.15, 
           "Expected value ~3.14159, got %.5f", ast->as.number.as.d_val);
    TEST_PASS();
}

static void test_identifier() {
    TEST_START("Identifier parsing");
    ASTNode *ast = parse_expression_test("variable");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_IDENT, "Expected identifier node");
    ASSERT(strcmp(ast->as.ident.name, "variable") == 0, 
           "Expected 'variable', got '%s'", ast->as.ident.name);
    TEST_PASS();
}

/*── Arithmetic expression tests ─────────────────────────────────────────*/
static void test_simple_addition() {
    TEST_START("Simple addition");
    ASTNode *ast = parse_expression_test("2 + 3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus operator");
    ASSERT(ast->as.binary.left->type == AST_NUMBER, "Expected number on left");
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Expected number on right");
    ASSERT(!ast->as.binary.left->as.number.is_double && ast->as.binary.left->as.number.as.i_val == 2, "Expected left value 2");
    ASSERT(!ast->as.binary.right->as.number.is_double && ast->as.binary.right->as.number.as.i_val == 3, "Expected right value 3");
    TEST_PASS();
}

static void test_simple_subtraction() {
    TEST_START("Simple subtraction");
    ASTNode *ast = parse_expression_test("10 - 4");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_MINUS, "Expected minus operator");
    TEST_PASS();
}

static void test_multiplication() {
    TEST_START("Multiplication");
    ASTNode *ast = parse_expression_test("6 * 7");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_STAR, "Expected star operator");
    TEST_PASS();
}

static void test_division() {
    TEST_START("Division");
    ASTNode *ast = parse_expression_test("15 / 3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_SLASH, "Expected slash operator");
    TEST_PASS();
}

/*── Modulo Operator Tests ───────────────────────────────────────────────*/
static void test_modulo_operator() {
    TEST_START("Parsing: simple modulo");
    ASTNode *ast = parse_expression_test("10 % 3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PERCENT, "Expected percent operator");
    ASSERT(ast->as.binary.left->type == AST_NUMBER, "Expected number on left");
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Expected number on right");
    TEST_PASS();

    TEST_START("Parsing: modulo precedence");
    ast = parse_expression_test("5 + 10 % 3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus at root");
    ASTNode *right = ast->as.binary.right;
    ASSERT(right->type == AST_BINARY, "Expected binary on right");
    ASSERT(right->as.binary.op.type == T_PERCENT, "Expected percent on right");
    TEST_PASS();

    TEST_START("Parsing: modulo left-associativity");
    ast = parse_expression_test("20 % 7 % 4");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PERCENT, "Expected percent at root");
    ASTNode *left = ast->as.binary.left;
    ASSERT(left->type == AST_BINARY, "Expected binary on left");
    ASSERT(left->as.binary.op.type == T_PERCENT, "Expected percent on left");
    TEST_PASS();
}


/*── Precedence tests ────────────────────────────────────────────────────*/
static void test_precedence_multiplication_first() {
    TEST_START("Precedence: multiplication before addition");
    ASTNode *ast = parse_expression_test("2 + 3 * 4");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus at root");
    
    // Left should be 2
    ASSERT(ast->as.binary.left->type == AST_NUMBER, "Expected number on left");
    ASSERT(!ast->as.binary.left->as.number.is_double && ast->as.binary.left->as.number.as.i_val == 2, "Expected left value 2");
    
    // Right should be (3 * 4)
    ASSERT(ast->as.binary.right->type == AST_BINARY, "Expected binary on right");
    ASSERT(ast->as.binary.right->as.binary.op.type == T_STAR, "Expected multiplication on right");
    TEST_PASS();
}

static void test_precedence_with_parentheses() {
    TEST_START("Precedence: parentheses override");
    ASTNode *ast = parse_expression_test("(2 + 3) * 4");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_STAR, "Expected multiplication at root");
    
    // Left should be (2 + 3)
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Expected binary on left");
    ASSERT(ast->as.binary.left->as.binary.op.type == T_PLUS, "Expected addition on left");
    
    // Right should be 4
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Expected number on right");
    ASSERT(!ast->as.binary.right->as.number.is_double && ast->as.binary.right->as.number.as.i_val == 4, "Expected right value 4");
    TEST_PASS();
}

static void test_complex_precedence() {
    TEST_START("Complex precedence expression");
    ASTNode *ast = parse_expression_test("2 * 3 + 4 * 5");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus at root");
    
    // Both sides should be multiplications
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Expected binary on left");
    ASSERT(ast->as.binary.left->as.binary.op.type == T_STAR, "Expected multiplication on left");
    ASSERT(ast->as.binary.right->type == AST_BINARY, "Expected binary on right");
    ASSERT(ast->as.binary.right->as.binary.op.type == T_STAR, "Expected multiplication on right");
    TEST_PASS();
}

/*── Exponentiation tests ───────────────────────────────────────────────*/
static void test_right_associativity_exponent() {
    TEST_START("Right associativity: exponentiation");
    // Use the new operator for exponentiation
    ASTNode *ast = parse_expression_test("2 ** 3 ** 2"); 
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    // The root operator should now be T_STAR_STAR
    ASSERT(ast->as.binary.op.type == T_STAR_STAR,
           "Root should be star-star operator");

    /*  (2 ** 3) ** 2  would put '+' on the left branch,
        right-associativity means the RIGHT child is another star-star node. */
    ASSERT(ast->as.binary.right->type == AST_BINARY,
           "Right child should be binary");
    // The right child should also be T_STAR_STAR
    ASSERT(ast->as.binary.right->as.binary.op.type == T_STAR_STAR,
           "Right child should also be '**', proving right-assoc");
    TEST_PASS();
}

/*── Unary operator tests ────────────────────────────────────────────────*/
static void test_unary_minus() {
    TEST_START("Unary minus");
    ASTNode *ast = parse_expression_test("-5");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_UNARY, "Expected unary node");
    ASSERT(ast->as.unary.op.type == T_MINUS, "Expected minus operator");
    ASSERT(ast->as.unary.child->type == AST_NUMBER, "Expected number child");
    ASSERT(!ast->as.unary.child->as.number.is_double && ast->as.unary.child->as.number.as.i_val == 5, "Expected child value 5");
    TEST_PASS();
}

static void test_unary_precedence() {
    TEST_START("Unary precedence");
    ASTNode *ast = parse_expression_test("-2 * 3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_STAR, "Expected multiplication at root");
    
    // Left should be (-2)
    ASSERT(ast->as.binary.left->type == AST_UNARY, "Expected unary on left");
    ASSERT(ast->as.binary.left->as.unary.op.type == T_MINUS, "Expected minus on left");
    TEST_PASS();
}

static void test_double_unary() {
    TEST_START("Double unary minus");
    ASTNode *ast = parse_expression_test("--3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_UNARY, "Expected unary node");
    ASSERT(ast->as.unary.op.type == T_MINUS, "Expected minus operator");
    ASSERT(ast->as.unary.child->type == AST_UNARY, "Expected unary child");
    ASSERT(ast->as.unary.child->as.unary.op.type == T_MINUS, "Expected minus child");
    TEST_PASS();
}

/*── Associativity tests ─────────────────────────────────────────────────*/
static void test_left_associativity_addition() {
    TEST_START("Left associativity: addition");
    ASTNode *ast = parse_expression_test("1 + 2 + 3");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus at root");
    
    // Should be ((1 + 2) + 3), so left side is binary
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Expected binary on left");
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Expected number on right");
    ASSERT(!ast->as.binary.right->as.number.is_double && ast->as.binary.right->as.number.as.i_val == 3, "Expected right value 3");
    TEST_PASS();
}

static void test_left_associativity_subtraction() {
    TEST_START("Left associativity: subtraction");
    ASTNode *ast = parse_expression_test("10 - 3 - 2");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_MINUS, "Expected minus at root");
    
    // Should be ((10 - 3) - 2), so left side is binary
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Expected binary on left");
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Expected number on right");
    TEST_PASS();
}

/*── Grouping tests ──────────────────────────────────────────────────────*/
static void test_nested_parentheses_1() {
    TEST_START("Nested parentheses - 1");
    ASTNode *ast = parse_expression_test("((2 + 3) * (4 - 1))");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_STAR, "Expected multiplication at root");
    
    // Both sides should be binary operations
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Expected binary on left");
    ASSERT(ast->as.binary.right->type == AST_BINARY, "Expected binary on right");
    TEST_PASS();
}

static void test_nested_parentheses_2() {
    TEST_START("Nested parentheses - 2");
    ASTNode *ast = parse_expression_test("((((((1 + 2) * 3) - 4) / 5) + 6) * 7)");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_STAR, "Expected multiplication at root");
    
    // Both sides should be binary operations
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Expected binary on left");
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Expected number on right");
    TEST_PASS();
}

static void test_multiple_groupings() {
    TEST_START("Multiple groupings");
    ASTNode *ast = parse_expression_test("(1 + 2) * (3 + 4) / (5 - 2)");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    // The parser should handle the precedence correctly
    TEST_PASS();
}

/*── Complex expression tests ────────────────────────────────────────────*/
static void test_complex_arithmetic() {
    TEST_START("Complex arithmetic expression");
    ASTNode *ast = parse_expression_test("2 * (3 + 4) - 5 / (1 + 1)");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

static void test_identifier_arithmetic() {
    TEST_START("Arithmetic with identifiers");
    ASTNode *ast = parse_expression_test("x + y * z");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus at root");
    
    // Left should be identifier 'x'
    ASSERT(ast->as.binary.left->type == AST_IDENT, "Expected identifier on left");
    ASSERT(strcmp(ast->as.binary.left->as.ident.name, "x") == 0, "Expected 'x' on left");
    
    // Right should be (y * z)
    ASSERT(ast->as.binary.right->type == AST_BINARY, "Expected binary on right");
    TEST_PASS();
}

static void test_mixed_expressions() {
    TEST_START("Mixed numbers and identifiers");
    ASTNode *ast = parse_expression_test("a * 2 + b / 3.14");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

/*── Edge case tests ─────────────────────────────────────────────────────*/
static void test_single_character_identifier() {
    TEST_START("Single character identifier");
    ASTNode *ast = parse_expression_test("x");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_IDENT, "Expected identifier node");
    ASSERT(strcmp(ast->as.ident.name, "x") == 0, "Expected 'x'");
    TEST_PASS();
}

static void test_zero() {
    TEST_START("Zero value");
    ASTNode *ast = parse_expression_test("0");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_NUMBER, "Expected number node");
    ASSERT(!ast->as.number.is_double && ast->as.number.as.i_val == 0, "Expected value 0");
    TEST_PASS();
}

static void test_decimal_zero() {
    TEST_START("Decimal zero");
    ASTNode *ast = parse_expression_test("0.0");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_NUMBER, "Expected number node");
    ASSERT(ast->as.number.is_double && ast->as.number.as.d_val == 0.0, "Expected value 0.0");
    TEST_PASS();
}

static void test_large_number() {
    TEST_START("Large number");
    ASTNode *ast = parse_expression_test("123456789.987654321");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_NUMBER && ast->as.number.is_double, "Expected double node");
    TEST_PASS();
}

static void test_long_identifier() {
    TEST_START("Long identifier");
    ASTNode *ast = parse_expression_test("very_long_variable_name_with_underscores");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_IDENT, "Expected identifier node");
    ASSERT(strcmp(ast->as.ident.name, "very_long_variable_name_with_underscores") == 0, 
           "Expected long identifier name");
    TEST_PASS();
}

/*── Whitespace handling tests ───────────────────────────────────────────*/
static void test_whitespace_handling() {
    TEST_START("Whitespace handling");
    ASTNode *ast = parse_expression_test("  2   +   3  ");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_PLUS, "Expected plus operator");
    TEST_PASS();
}

static void test_no_whitespace() {
    TEST_START("No whitespace");
    ASTNode *ast = parse_expression_test("2+3*4");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

/*── Error handling tests ────────────────────────────────────────────────*/
static void test_empty_input() {
    TEST_START("Empty input");
    PrattLexer lex;
    pratt_lexer_init(&lex, "");

    Interpreter interpreter;
    interpreter_init(&interpreter, 256);
    Parser parser;
    parser_init(&parser, pratt_lexer_next, &lex, &interpreter,
                default_rules, default_rule_count, default_token_name, &interpreter.arena);

    ASTNode *result = parse_expression(&parser);

    ASSERT(result == NULL, "Expected NULL AST for empty input");
    ASSERT(parser.had_error, "Expected error flag to be set for empty input");
    ASSERT(parser.last_error.message && strstr(parser.last_error.message, "No parse-rule"), "Expected clearer EOF error message, got '%s'", parser.last_error.message);
    parser_destroy(&parser);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_negative_token_id_error() {
    TEST_START("Negative token ID error (UB fix)");
    int bad_id = -1;
    Parser p;
    Interpreter interpreter;
    interpreter_init(&interpreter, 256);
    parser_init(&p, bad_token_lexer, &bad_id, &interpreter, default_rules, default_rule_count, default_token_name, &interpreter.arena);
    ASTNode *ast = parse_expression(&p);
    ASSERT(ast == NULL, "Expected NULL AST on negative ID error");
    ASSERT(p.had_error, "Parser should report error on negative ID");
    ASSERT(p.last_error.message && strstr(p.last_error.message, "PrattToken ID"), "Error should complain about PrattToken ID, got: %s", p.last_error.message);
    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_recursion_limit() {
    TEST_START("Recursion depth limit");
    char source[2001];
    memset(source, '(', 1000);
    strcpy(source + 1000, "1");
    memset(source + 1001, ')', 999);
    source[2000] = '\0';
    
    PrattLexer lex; pratt_lexer_init(&lex, source);
    Parser p;
    Interpreter interpreter;
    interpreter_init(&interpreter, 4096);
    parser_init(&p, pratt_lexer_next, &lex, &interpreter, default_rules, default_rule_count, default_token_name, &interpreter.arena);
    parser_set_max_recursion(&p, 500);

    ASTNode *ast = parse_expression(&p);
    ASSERT(ast == NULL, "Expected NULL AST due to recursion limit");
    ASSERT(p.had_error, "Parser should report an error");
    ASSERT(p.last_error.message && strstr(p.last_error.message, "recursion depth"), "Error message should mention recursion limit, got: '%s'", p.last_error.message);
    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_error_recovery() {
    TEST_START("Error recovery with synchronization");
    PrattLexer lex;
    pratt_lexer_init(&lex, "1 + ; 2");

    Interpreter interpreter;
    interpreter_init(&interpreter, 1024);
    Parser p;
    parser_init(&p, pratt_lexer_next, &lex, &interpreter, default_rules, default_rule_count, default_token_name, &interpreter.arena);
    p.recover_errors = 1;
    PrattTokenType sync_set[] = { T_SEMICOLON };
    parser_set_sync_tokens(&p, sync_set, 1);

    ASTNode *ast1 = parse_expression_until(&p, sync_set, 1);
    ASSERT(ast1 == NULL, "First expression should fail to parse");
    ASSERT(p.had_error, "Error flag should be set after first parse");
    
    p.had_error = 0; // Reset error for next parse attempt
    
    ASTNode *ast2 = parse_expression(&p);
    ASSERT(ast2 != NULL, "Second expression should parse correctly after sync");
    ASSERT(ast2->type == AST_NUMBER && !ast2->as.number.is_double && ast2->as.number.as.i_val == 2, "AST should be number 2");
    ASSERT(!p.had_error, "Second parse should succeed without error");

    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_incomplete_expression() {
    TEST_START("Incomplete expression");
    ASTNode *ast = parse_expression_test("2 +");
    // Should handle incomplete expressions gracefully
    if (ast == NULL) {
        TEST_PASS();
    } else {
        TEST_FAIL("Expected NULL for incomplete expression");
    }
}

static bool parse_and_check_error(const char *source, ASTNode **out_ast, Interpreter *out_interpreter, Parser *out_parser) {
    PrattLexer lex;
    pratt_lexer_init(&lex, source);
    interpreter_init(out_interpreter, 1024);
    parser_init(out_parser, pratt_lexer_next, &lex, out_interpreter, default_rules, default_rule_count, default_token_name, &out_interpreter->arena);
    *out_ast = parse_expression(out_parser);
    return out_parser->had_error;
}

static void test_unmatched_parentheses() {
    TEST_START("Unmatched parentheses");
    ASTNode *ast;
    Interpreter interpreter;
    Parser p;
    bool had_error = parse_and_check_error("(2 + 3", &ast, &interpreter, &p);

    ASSERT(ast == NULL, "Expected NULL AST on error");
    ASSERT(had_error, "Expected parser to report an error");
    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

/*── error-branch tests ──────────────────────────────────────────────*/
static void test_invalid_character_error() {
    TEST_START("Invalid character error");

    ASTNode *ast;
    Interpreter interpreter;
    Parser p;
    bool had_error = parse_and_check_error("2 $ 3", &ast, &interpreter, &p);

    ASSERT(ast == NULL, "Expected NULL AST on error");
    ASSERT(had_error,   "Parser should report error on '$'");
    /* Optional: check that the diagnostic mentions the unexpected char */
    ASSERT(p.last_error.message && strstr(p.last_error.message, "$") != NULL,
           "Error message should reference '$', got: %s", p.last_error.message);

    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_sparse_token_id_error() {
    TEST_START("Sparse/invalid token ID error");

    // 1. Define the out-of-bounds token ID
    enum { T_WAY_TOO_BIG = 9999 };
    int bad_id = T_WAY_TOO_BIG;

    // 2. Initialize the parser with our special lexer that emits the bad ID.
    //    We pass a pointer to `bad_id` as the lexer context.
    Parser p;
    Interpreter interpreter;
    interpreter_init(&interpreter, 256);
    parser_init(&p, bad_token_lexer, &bad_id, &interpreter,
                default_rules, default_rule_count,
                default_token_name, &interpreter.arena);

    // 3. Run the parser
    ASTNode *ast = parse_expression(&p);

    // 4. Assert the expected error state
    ASSERT(ast == NULL, "Expected NULL AST on sparse ID error");
    ASSERT(p.had_error, "Parser should report error on sparse ID");
    ASSERT(p.last_error.message && strstr(p.last_error.message, "PrattToken ID"),
           "Error message should complain about invalid token ID, got: %s", p.last_error.message);

    // 5. Clean up
    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_number_out_of_range_error() {
    TEST_START("Out-of-range number error");

    ASTNode *ast;
    Interpreter interpreter;
    Parser   p;
    bool had_error = parse_and_check_error("1e4000", &ast, &interpreter, &p);

    ASSERT(ast == NULL, "Expected NULL AST on ERANGE error");
    ASSERT(p.had_error,   "Parser should report ERANGE");
    ASSERT(strstr(p.last_error.message, "out of range") != NULL,
           "Error message should say 'out of range', got: %s", p.last_error.message);

    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

static void test_forced_oom_error() {
    TEST_START("Forced out-of-memory error");

    /* 1. Prepare tiny arena and pass it to parser. Then mark it OOM. */
    PrattLexer lex;
    pratt_lexer_init(&lex, "1 + 2");

    Interpreter interpreter;
    interpreter_init(&interpreter, 8); // Absurdly small arena
    Parser p;
    parser_init(&p, pratt_lexer_next, &lex, &interpreter, default_rules, default_rule_count, default_token_name, &interpreter.arena);
    p.arena->oom = 1; /* Manually trigger the OOM state */

    ASTNode *ast = parse_expression(&p);

    ASSERT(ast == NULL,              "Expected NULL AST on OOM");
    ASSERT(p.had_error,              "Parser should flag error");
    ASSERT(strstr(p.last_error.message, "Out of memory") != NULL,
           "Error message should reference OOM, got: %s", p.last_error.message);

    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

/*── Performance/stress tests ────────────────────────────────────────────*/
static void test_deeply_nested_expression() {
    TEST_START("Deeply nested expression");
    ASTNode *ast = parse_expression_test("((((((1 + 2) * 3) - 4) / 5) + 6) * 7)");
    ASSERT(ast != NULL, "Expected AST node");
    // Just verify it parses without crashing
    TEST_PASS();
}

static void test_long_chain_expression() {
    TEST_START("Long chain expression");
    ASTNode *ast = parse_expression_test("1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10");
    ASSERT(ast != NULL, "Expected AST node");
    // Verify it's properly left-associative
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

/*── Real-world inspired tests ───────────────────────────────────────────*/
static void test_formula_like_expression() {
    TEST_START("Formula-like expression");
    ASTNode *ast = parse_expression_test("a * x * x + b * x + c");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

static void test_distance_formula() {
    TEST_START("Distance formula components");
    ASTNode *ast = parse_expression_test("(x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

static void test_physics_formula() {
    TEST_START("Physics-like formula");
    ASTNode *ast = parse_expression_test("m * g * h + 0.5 * k * x * x");
    ASSERT(ast != NULL, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    TEST_PASS();
}

static void test_call_no_args() {
    TEST_START("Postfix: call with no arguments");
    ASTNode *ast = parse_expression_test("foo()");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_CALL, "Expected call node, got %d", ast->type);
    ASSERT(ast->as.call.callee->type == AST_IDENT, "Callee should be identifier");
    ASSERT(strcmp(ast->as.call.callee->as.ident.name, "foo") == 0, "Callee name mismatch");
    ASSERT(ast->as.call.argc == 0, "Expected 0 arguments, got %zu", ast->as.call.argc);
    ASSERT(ast->as.call.args == NULL, "args pointer should be NULL for 0 args");
    TEST_PASS();
}

static void test_call_one_arg() {
    TEST_START("Postfix: call with one argument");
    ASTNode *ast = parse_expression_test("foo(123)");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_CALL, "Expected call node");
    ASSERT(ast->as.call.argc == 1, "Expected 1 argument, got %zu", ast->as.call.argc);
    ASSERT(ast->as.call.args != NULL, "args pointer should not be NULL");
    ASSERT(ast->as.call.args[0]->type == AST_NUMBER, "Argument should be a number");
    ASSERT(!ast->as.call.args[0]->as.number.is_double && ast->as.call.args[0]->as.number.as.i_val == 123, "Argument value mismatch");
    TEST_PASS();
}

static void test_call_multiple_args() {
    TEST_START("Postfix: call with multiple arguments");
    ASTNode *ast = parse_expression_test("func(a, 2, b*c)");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_CALL, "Expected call node");
    ASSERT(ast->as.call.argc == 3, "Expected 3 arguments, got %zu", ast->as.call.argc);
    ASSERT(ast->as.call.args[0]->type == AST_IDENT, "Arg 1 should be ident");
    ASSERT(ast->as.call.args[1]->type == AST_NUMBER, "Arg 2 should be number");
    ASSERT(ast->as.call.args[2]->type == AST_BINARY, "Arg 3 should be binary expr");
    TEST_PASS();
}

static void test_call_nested() {
    TEST_START("Postfix: nested calls");
    ASTNode *ast = parse_expression_test("one(two(three()))");
    ASSERT(ast, "Expected AST node");
    // one(...)
    ASSERT(ast->type == AST_CALL, "Outer call node type mismatch");
    ASSERT(ast->as.call.argc == 1, "Outer call should have 1 arg");
    // two(...)
    ASTNode *inner_call = ast->as.call.args[0];
    ASSERT(inner_call->type == AST_CALL, "Inner call node type mismatch");
    ASSERT(inner_call->as.call.argc == 1, "Inner call should have 1 arg");
    // three()
    ASTNode *innermost_call = inner_call->as.call.args[0];
    ASSERT(innermost_call->type == AST_CALL, "Innermost call node type mismatch");
    ASSERT(innermost_call->as.call.argc == 0, "Innermost call should have 0 args");
    TEST_PASS();
}

static void test_call_precedence() {
    TEST_START("Postfix: call precedence");
    ASTNode *ast = parse_expression_test("a + foo(b) * c");
    // Should parse as a + (foo(b) * c)
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_BINARY && ast->as.binary.op.type == T_PLUS, "Root should be '+'");
    ASTNode *rhs = ast->as.binary.right;
    ASSERT(rhs->type == AST_BINARY && rhs->as.binary.op.type == T_STAR, "RHS should be '*'");
    ASTNode *lhs_of_mul = rhs->as.binary.left;
    ASSERT(lhs_of_mul->type == AST_CALL, "LHS of '*' should be a call");
    ASSERT(lhs_of_mul->as.call.argc == 1, "Call should have 1 arg");
    TEST_PASS();
}

static void test_call_error_missing_rparen() {
    TEST_START("Postfix: error on missing ')'");
    ASTNode *ast;
    Interpreter interpreter;
    Parser p;
    bool had_error = parse_and_check_error("foo(1, 2", &ast, &interpreter, &p);
    ASSERT(ast == NULL, "Expected NULL AST on error");
    ASSERT(had_error, "Expected parser to report an error");
    ASSERT(p.last_error.message && strstr(p.last_error.message, "')' after arguments"), "Wrong error message: %s", p.last_error.message);
    parser_destroy(&p);
    interpreter_destroy(&interpreter);
    TEST_PASS();
}

/*── Comparison, Logical & Ternary Tests ─────────────────────────────────*/
static void test_comparison_simple() {
    TEST_START("Simple comparison");
    ASTNode *ast = parse_expression_test("a > b");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_GREATER, "Expected > operator");
    ASSERT(ast->as.binary.left->type == AST_IDENT, "Expected ident on left");
    ASSERT(ast->as.binary.right->type == AST_IDENT, "Expected ident on right");
    TEST_PASS();
}

static void test_comparison_precedence() {
    TEST_START("Comparison precedence vs. terms");
    ASTNode *ast = parse_expression_test("a + 5 == b - 2");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_BINARY, "Expected binary node");
    ASSERT(ast->as.binary.op.type == T_EQUAL_EQUAL, "Expected == at root");
    ASSERT(ast->as.binary.left->type == AST_BINARY, "Left side should be binary '+'");
    ASSERT(ast->as.binary.left->as.binary.op.type == T_PLUS, "Expected '+' on left");
    ASSERT(ast->as.binary.right->type == AST_BINARY, "Right side should be binary '-'");
    ASSERT(ast->as.binary.right->as.binary.op.type == T_MINUS, "Expected '-' on right");
    TEST_PASS();
}

static void test_comparison_chaining() {
    TEST_START("Comparison chaining (left-associative)");
    ASTNode *ast = parse_expression_test("1 < 2 == 1");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_BINARY && ast->as.binary.op.type == T_EQUAL_EQUAL, "Root should be '=='");
    ASSERT(ast->as.binary.left->type == AST_BINARY && ast->as.binary.left->as.binary.op.type == T_LESS, "Left should be '<'");
    ASSERT(ast->as.binary.right->type == AST_NUMBER, "Right should be a number");
    TEST_PASS();
}

static void test_logical_precedence() {
    TEST_START("Logical operator precedence (&& > ||)");
    ASTNode *ast = parse_expression_test("a || b && c");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_BINARY && ast->as.binary.op.type == T_PIPE_PIPE, "Root should be '||'");
    ASSERT(ast->as.binary.right->type == AST_BINARY && ast->as.binary.right->as.binary.op.type == T_AMP_AMP, "Right should be '&&'");
    TEST_PASS();
}

static void test_logical_short_circuit_ast() {
    TEST_START("Logical AST for short-circuiting");
    ASTNode *ast = parse_expression_test("x != 0 && y / x > 2");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_BINARY && ast->as.binary.op.type == T_AMP_AMP, "Root should be '&&'");
    ASSERT(ast->as.binary.left->type == AST_BINARY && ast->as.binary.left->as.binary.op.type == T_BANG_EQUAL, "Left should be '!='");
    TEST_PASS();
}

static void test_ternary_simple() {
    TEST_START("Simple ternary operator");
    ASTNode *ast = parse_expression_test("c ? t : f");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_TERNARY, "Expected ternary node");
    ASSERT(ast->as.ternary.cond->type == AST_IDENT, "Condition should be ident 'c'");
    ASSERT(ast->as.ternary.then_branch->type == AST_IDENT, "Then branch should be ident 't'");
    ASSERT(ast->as.ternary.else_branch->type == AST_IDENT, "Else branch should be ident 'f'");
    TEST_PASS();
}

static void test_ternary_precedence() {
    TEST_START("Ternary operator precedence");
    ASTNode *ast = parse_expression_test("a > b ? x + y : x - y");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_TERNARY, "Expected ternary node at root");
    ASSERT(ast->as.ternary.cond->type == AST_BINARY && ast->as.ternary.cond->as.binary.op.type == T_GREATER, "Condition should be a > b");
    ASSERT(ast->as.ternary.then_branch->type == AST_BINARY && ast->as.ternary.then_branch->as.binary.op.type == T_PLUS, "Then branch should be x + y");
    ASSERT(ast->as.ternary.else_branch->type == AST_BINARY && ast->as.ternary.else_branch->as.binary.op.type == T_MINUS, "Else branch should be x - y");
    TEST_PASS();
}

static void test_ternary_right_associativity() {
    TEST_START("Ternary operator right-associativity");
    ASTNode *ast = parse_expression_test("a ? b : c ? d : e");
    ASSERT(ast, "Expected AST node");
    ASSERT(ast->type == AST_TERNARY, "Root should be ternary");
    ASSERT(ast->as.ternary.cond->type == AST_IDENT, "Root condition should be 'a'");
    ASSERT(ast->as.ternary.then_branch->type == AST_IDENT, "Root 'then' should be 'b'");
    ASSERT(ast->as.ternary.else_branch->type == AST_TERNARY, "Root 'else' should be another ternary");

    ASTNode *nested = ast->as.ternary.else_branch;
    ASSERT(nested->as.ternary.cond->type == AST_IDENT, "Nested condition should be 'c'");
    ASSERT(nested->as.ternary.then_branch->type == AST_IDENT, "Nested 'then' should be 'd'");
    ASSERT(nested->as.ternary.else_branch->type == AST_IDENT, "Nested 'else' should be 'e'");
    TEST_PASS();
}

/*── Statement tests ────────────────────────────────────────────────────*/
static void test_declaration_and_use() {
    TEST_START("Declaration and use");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("var x = 2 + 3; var y = x * 2;", &p, &interp);
    
    ASSERT(ast, "Expected AST");
    ASSERT(ast->type == ST_BLOCK, "Expected block statement");
    ASSERT(ast->as.block.count == 2, "Expected 2 statements in block");
    
    Statement *var_x = ast->as.block.list[0];
    ASSERT(var_x->type == ST_VAR, "Expected var declaration for x");
    ASSERT(strcmp(var_x->as.var.name, "x") == 0, "Expected var name 'x'");
    ASSERT(var_x->as.var.initializer, "Expected initializer for x");

    Statement *var_y = ast->as.block.list[1];
    ASSERT(var_y->type == ST_VAR, "Expected var declaration for y");
    ASSERT(var_y->as.var.initializer, "Expected initializer for y");
    
    ASTNode *y_init = var_y->as.var.initializer;
    ASSERT(y_init->type == AST_BINARY, "Expected binary expr for y initializer");
    ASSERT(y_init->as.binary.op.type == T_STAR, "Expected '*' operator");
    ASSERT(y_init->as.binary.left->type == AST_IDENT, "Left operand of '*' should be ident");
    ASSERT(strcmp(y_init->as.binary.left->as.ident.name, "x") == 0, "Left operand should be 'x'");

    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

static void test_shadowing_in_blocks() {
    TEST_START("Shadowing in blocks");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("var x = 1; { var x = 2; }", &p, &interp);
    ASSERT(ast, "Expected AST");
    ASSERT(ast->type == ST_BLOCK, "Expected outer block");
    ASSERT(ast->as.block.count == 2, "Expected 2 statements in outer block");

    Statement *outer_var = ast->as.block.list[0];
    ASSERT(outer_var->type == ST_VAR, "Expected outer var decl");
    PrattToken outer_x_tok = outer_var->as.var.name_tok;

    Statement *inner_block = ast->as.block.list[1];
    ASSERT(inner_block->type == ST_BLOCK, "Expected inner block");
    ASSERT(inner_block->as.block.count == 1, "Expected 1 statement in inner block");

    Statement *inner_var = inner_block->as.block.list[0];
    ASSERT(inner_var->type == ST_VAR, "Expected inner var decl");
    PrattToken inner_x_tok = inner_var->as.var.name_tok;
    
    ASSERT(outer_x_tok.start != inner_x_tok.start, "Tokens for shadowed variables should be different");
    
    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

static void test_if_else_nesting() {
    TEST_START("If/else nesting");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("if (a < b) x = 1; else x = 2;", &p, &interp);
    ASSERT(ast && ast->type == ST_BLOCK && ast->as.block.count == 1, "Expected single if stmt in block");

    Statement *if_stmt = ast->as.block.list[0];
    ASSERT(if_stmt->type == ST_IF, "Expected if statement");
    ASSERT(if_stmt->as.if_s.condition, "If should have a condition");
    ASSERT(if_stmt->as.if_s.condition->type == AST_BINARY, "Condition should be binary expr");

    ASSERT(if_stmt->as.if_s.then_branch, "If should have a then branch");
    ASSERT(if_stmt->as.if_s.then_branch->type == ST_EXPR, "Then branch should be an expression statement");

    ASSERT(if_stmt->as.if_s.else_branch, "If should have an else branch");
    ASSERT(if_stmt->as.if_s.else_branch->type == ST_EXPR, "Else branch should be an expression statement");

    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

static void test_if_without_else() {
    TEST_START("If without else");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("if (a) b=1;", &p, &interp);
    ASSERT(ast && ast->type == ST_BLOCK && ast->as.block.count == 1, "Expected single if stmt in block");
    Statement *if_stmt = ast->as.block.list[0];
    ASSERT(if_stmt->type == ST_IF, "Expected if statement");
    ASSERT(if_stmt->as.if_s.then_branch, "If should have a then branch");
    ASSERT(if_stmt->as.if_s.else_branch == NULL, "Else branch should be NULL");
    
    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

/* A structure to hold the outcome of a full parse-and-execute run. */
typedef struct {
    bool success;       // True if no parse or runtime errors occurred.
    bool parse_error;   // True if the parser failed.
    char parse_error_message[256]; // The parser's error message, if any.
} RunResult;

/* Centralized helper to parse and execute a source string. */
static RunResult parse_and_execute(Interpreter* interp, const char* source) {
    RunResult result = { .success = false, .parse_error = false };
    result.parse_error_message[0] = '\0';

    // --- 1. Setup Parser ---
    PrattLexer lex;
    pratt_lexer_init(&lex, source);
    Parser parser;
    parser_init(&parser, pratt_lexer_next, &lex, interp, default_rules, default_rule_count, default_token_name, &interp->arena);

    // --- 2. Parse Loop ---
    Statement* program_statements[128];
    size_t num_statements = 0;
    while (peek(&parser).type != T_EOF && !parser.had_error && num_statements < 128) {
        Statement *stmt = parse_statement(&parser);
        if (stmt) {
            program_statements[num_statements++] = stmt;
        } else if (parser.had_error) {
            // Error already flagged by the parser
            break;
        }
    }

    // --- 3. Check for Parse Error ---
    if (parser.had_error) {
        result.parse_error = true;
        if (parser.last_error.message) {
            strncpy(result.parse_error_message, parser.last_error.message, sizeof(result.parse_error_message) - 1);
        }
        parser_destroy(&parser);
        return result;
    }

    // The parser is no longer needed; the AST lives on in the interpreter's arena.
    parser_destroy(&parser);

    // --- 4. Execute Loop ---
    for (size_t i = 0; i < num_statements; ++i) {
        ExecResult res = execute(interp, program_statements[i]);
        if (interp->had_error) break; // A runtime error occurred.

        // Handle top-level control flow errors
        if (res.status == EXEC_BREAK) {
            runtime_error(interp, "Cannot 'break' outside of a loop.");
            break;
        } else if (res.status == EXEC_CONTINUE) {
            runtime_error(interp, "Cannot 'continue' outside of a loop.");
            break;
        } else if (res.status == EXEC_RETURN) {
            runtime_error(interp, "Cannot 'return' from top-level code.");
            break;
        }
    }

    // --- 5. Finalize Result ---
    if (!interp->had_error) {
        result.success = true;
    }

    return result;
}

/*── Interpreter Tests ──────────────────────────────────────────────────*/
static char test_output_buffer[4096];

static void run_interpreter_test(const char* name, const char* source, const char* expected) {
    TEST_START(name);

    // --- Redirect stdout ---
    fflush(stdout);
    int stdout_pipe[2];
    int saved_stdout = dup(fileno(stdout));
#ifdef _WIN32
    ASSERT(_pipe(stdout_pipe, sizeof(test_output_buffer), _O_TEXT) != -1, "pipe failed");
#else
    ASSERT(pipe(stdout_pipe) == 0, "pipe failed");
#endif
    dup2(stdout_pipe[1], fileno(stdout));
    close(stdout_pipe[1]);

    // --- Set up Interpreter and run ---
    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);

    RunResult result = parse_and_execute(&interp, source);

    // --- Restore stdout and read from pipe ---
    fflush(stdout);
    dup2(saved_stdout, fileno(stdout));
    close(saved_stdout);
    
    int bytes_read = read(stdout_pipe[0], test_output_buffer, sizeof(test_output_buffer) - 1);
    close(stdout_pipe[0]);


    if (bytes_read >= 0) {
        test_output_buffer[bytes_read] = '\0';
    } else {
        test_output_buffer[0] = '\0';
    }

    // --- Assert ---
    if (result.parse_error) {
        ASSERT(false, "Parse error: %s", result.parse_error_message);
    } else if (interp.had_error) {
        ASSERT(false, "Runtime error: %s", interp.error_message);
    } else {
        ASSERT(strcmp(test_output_buffer, expected) == 0,
               "Output mismatch.\nExpected:\n--\n%s\n--\nGot:\n--\n%s\n--",
               expected, test_output_buffer);
    }
    
    // --- Cleanup ---
    interpreter_destroy(&interp);

    TEST_PASS();
}


// Test helper for expecting runtime errors
static void run_interpreter_error_test(const char* name, const char* source, const char* expected_error_substr) {
    TEST_START(name);

    // --- Set up Interpreter and run ---
    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);

    RunResult result = parse_and_execute(&interp, source);

    // --- Assert ---
    if (result.parse_error) {
        ASSERT(false, "Parse error: %s", result.parse_error_message);
    } else if (!interp.had_error) {
        ASSERT(false, "Expected a runtime error, but none occurred.");
    } else {
        ASSERT(strstr(interp.error_message, expected_error_substr) != NULL,
               "Error message mismatch.\nExpected substring: '%s'\nGot: '%s'",
               expected_error_substr, interp.error_message);
    }
    
    // --- Cleanup ---
    interpreter_destroy(&interp);
    
    TEST_PASS();
}

// Helper to test for expected parse errors
static void run_parser_error_test(const char* name, const char* source, const char* expected_error_substr) {
    TEST_START(name);

    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);

    RunResult result = parse_and_execute(&interp, source);

    if (!result.parse_error) {
        TEST_FAIL("Expected a parse error, but none occurred.");
    } else {
        ASSERT(strstr(result.parse_error_message, expected_error_substr) != NULL,
               "Parse error message mismatch.\nExpected substring: '%s'\nGot: '%s'",
               expected_error_substr, result.parse_error_message);
        TEST_PASS();
    }
    
    interpreter_destroy(&interp);
}


// Helper for GC tests that need to inspect memory state
static void run_gc_test(const char* name, const char* source, bool (*check_fn)(Interpreter*)) {
    TEST_START(name);

    // --- Set up Interpreter and run ---
    Interpreter interp;
    interpreter_init(&interp, DEFAULT_INITIAL_ARENA_SIZE);
    
    RunResult result = parse_and_execute(&interp, source);
    
    // --- Assert ---
    if (result.parse_error) {
        ASSERT(false, "Parse error: %s", result.parse_error_message);
    } else if (interp.had_error) {
        ASSERT(false, "Runtime error: %s", interp.error_message);
    } else {
        bool check_ok = (check_fn == NULL) ? true : check_fn(&interp);
        ASSERT(check_ok, "GC check function returned false.");
    }
    
    // --- Cleanup ---
    interpreter_destroy(&interp);
    
    TEST_PASS();
}


static void test_comparison_logic_primitives() {
    const char* source =
        "println(nil == nil);"
        "println(true == true);"
        "println(false == false);"
        "println(true != false);"
        "println(123 == 123);"
        "println(123 != 456);"
        "println(\"hello\" == \"hello\");"
        "println(\"hello\" != \"world\");";
    const char* expected =
        "true\n"
        "true\n"
        "true\n"
        "true\n"
        "true\n"
        "true\n"
        "true\n"
        "true\n";
    run_interpreter_test("Comparison: Primitives", source, expected);
}

static void test_comparison_logic_arrays() {
    const char* source =
        "var a1 = [1, 2];"
        "var a2 = [1, 2];" // Different object, same content
        "var a3 = a1;"      // Same object reference
        "println(a1 == a2);"
        "println(a1 != a2);"
        "println(a1 == a3);"
        "println([] == []);";
    const char* expected =
        "false\n"
        "true\n"
        "true\n"
        "false\n";
    run_interpreter_test("Comparison: Array reference equality", source, expected);
}

static void test_comparison_logic_objects() {
    const char* source =
        "var o1 = {a: 1};"
        "var o2 = {a: 1};" // Different object, same content
        "var o3 = o1;"      // Same object reference
        "println(o1 == o2);"
        "println(o1 != o2);"
        "println(o1 == o3);"
        "println({} == {});";
    const char* expected =
        "false\n"
        "true\n"
        "true\n"
        "false\n";
    run_interpreter_test("Comparison: Object reference equality", source, expected);
}

static void test_assignment_expression() {
    const char* source =
        "var x; var y; println(x = (y = 5)); println(x); println(y);";
    const char* expected = "5\n5\n5\n";
    run_interpreter_test("Interpreter: Assignment is an expression", source, expected);
}

static void test_comparison_logic_strict_typing() {
    const char* source =
        "println(1 == \"1\");"
        "println(true == 1);"
        "println(nil == false);"
        "println([1] == {a:1});"
        "println(nil == 0);"
        "function foo() {}"
        "println(foo == nil);";
    const char* expected =
        "false\n"
        "false\n"
        "false\n"
        "false\n"
        "false\n"
        "false\n";
    run_interpreter_test("Comparison: Strict typing", source, expected);
}

static void test_relational_operators_on_composites() {
    run_interpreter_error_test("Comparison: Relational operators on arrays", "[1] > 0;", "Operands must be numbers for comparison.");
    run_interpreter_error_test("Comparison: Relational operators on objects", "({} < 1);", "Operands must be numbers for comparison.");
    run_interpreter_error_test("Comparison: Relational operators on booleans", "true > false;", "Operands must be numbers for comparison.");
}

static void test_variable_shadowing() {
    const char* source = "var x = 1; { var x = 2; println(x); } println(x);";
    const char* expected = "2\n1\n";
    run_interpreter_test("Interpreter: shadowing", source, expected);
}

static void test_while_loop() {
    const char* source = "var i = 0; while (i < 3) { println(i); i = i + 1; }";
    const char* expected = "0\n1\n2\n";
    run_interpreter_test("Interpreter: while loop", source, expected);
}

static void test_break_statement_parsing() {
    TEST_START("Parsing: break statement");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("while(true) { break; }", &p, &interp);
    ASSERT(ast && ast->type == ST_BLOCK && ast->as.block.count == 1, "Expected single while stmt in block");

    Statement *while_stmt = ast->as.block.list[0];
    ASSERT(while_stmt->type == ST_WHILE, "Expected while statement");
    ASSERT(while_stmt->as.while_s.body, "While should have a body");
    ASSERT(while_stmt->as.while_s.body->type == ST_BLOCK, "While body should be a block");

    Statement *block_body = while_stmt->as.while_s.body;
    ASSERT(block_body->as.block.count == 1, "Block should have one statement");

    Statement *break_stmt = block_body->as.block.list[0];
    ASSERT(break_stmt->type == ST_BREAK, "Expected break statement inside loop body");

    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

static void test_continue_statement_parsing() {
    TEST_START("Parsing: continue statement");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("while(true) { continue; }", &p, &interp);
    ASSERT(ast && ast->type == ST_BLOCK && ast->as.block.count == 1, "Expected single while stmt in block");

    Statement *while_stmt = ast->as.block.list[0];
    ASSERT(while_stmt->type == ST_WHILE, "Expected while statement");
    ASSERT(while_stmt->as.while_s.body, "While should have a body");
    ASSERT(while_stmt->as.while_s.body->type == ST_BLOCK, "While body should be a block");

    Statement *block_body = while_stmt->as.while_s.body;
    ASSERT(block_body->as.block.count == 1, "Block should have one statement");

    Statement *continue_stmt = block_body->as.block.list[0];
    ASSERT(continue_stmt->type == ST_CONTINUE, "Expected continue statement inside loop body");

    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

static void test_simple_continue() {
    const char* source =
        "var i = 0;"
        "var sum = 0;"
        "while (i < 5) {"
        "  i = i + 1;"
        "  if (i == 3) continue;"
        "  sum = sum + i;"
        "}"
        "println(sum);"; // Should be 1 + 2 + 4 + 5 = 12
    const char* expected = "12\n";
    run_interpreter_test("Control Flow: simple continue", source, expected);
}

static void test_nested_continue() {
    const char* source =
        "var i = 0;"
        "while (i < 2) {"
        "  println(\"i=\" + i);"
        "  var j = 0;"
        "  while (j < 4) {"
        "    j = j + 1;"
        "    if (j == 2) continue;"
        "    print(\"  j=\" + j);"
        "  }"
        "  println(\"\");"
        "  i = i + 1;"
        "}"
        "println(\"finished\");";
    const char* expected = "i=0\n  j=1  j=3  j=4\ni=1\n  j=1  j=3  j=4\nfinished\n";
    run_interpreter_test("Control Flow: nested continue only affects inner loop", source, expected);
}

static void test_continue_and_break() {
    const char* source =
        "var i = 0;"
        "while (i < 10) {"
        "  i = i + 1;"
        "  if (i < 5) continue;"
        "  if (i == 8) break;"
        "  print(i);"
        "}"
        "println(\"\");"; // should print 567
    const char* expected = "567\n";
    run_interpreter_test("Control Flow: continue and break together", source, expected);
}

static void test_control_flow_error() {
    run_interpreter_error_test("Control Flow Error: break outside loop", "break;", "Cannot 'break' outside of a loop.");
    run_interpreter_error_test("Control Flow Error: continue outside loop", "continue;", "Cannot 'continue' outside of a loop.");
    run_interpreter_error_test("Control Flow Error: break in function outside loop", "function f() { break; } f();", "Cannot 'break' outside of a loop.");
    run_interpreter_error_test("Control Flow Error: continue in function outside loop", "function f() { continue; } f();", "Cannot 'continue' outside of a loop.");
}

// Test suite for functions and return
static void test_function_and_return() {
    const char* source =
        "function sayHi(name) { return \"hi, \" + name; }"
        "println(sayHi(\"world\"));";
    const char* expected = "hi, world\n";
    run_interpreter_test("Functions: simple return", source, expected);
}

static void test_function_closure() {
    const char* source =
        "function make_counter() {"
        "  var i = 0;"
        "  function count() {"
        "    i = i + 1;"
        "    return i;"
        "  }"
        "  return count;"
        "}"
        "var counter = make_counter();"
        "println(counter());"
        "println(counter());";
    const char* expected = "1\n2\n";
    run_interpreter_test("Functions: lexical closure", source, expected);
}

static void test_function_recursion() {
    const char* source =
        "function fib(n) {"
        "  if (n < 2) return n;"
        "  return fib(n - 2) + fib(n - 1);"
        "}"
        "println(fib(8));";
    const char* expected = "21\n";
    run_interpreter_test("Functions: recursion (fibonacci)", source, expected);
}

static void test_simple_break() {
    const char* source =
        "var i = 0;"
        "while (i < 5) {"
        "  if (i == 3) break;"
        "  println(i);"
        "  i = i + 1;"
        "}"
        "println(\"done\");";
    const char* expected = "0\n1\n2\ndone\n";
    run_interpreter_test("Control Flow: simple break", source, expected);
}

static void test_nested_break() {
    const char* source =
        "var i = 0;"
        "while (i < 2) {"
        "  println(\"i=\" + i);"
        "  var j = 0;"
        "  while (j < 3) {"
        "    if (j == 1) break;"
        "    println(\"  j=\" + j);"
        "    j = j + 1;"
        "  }"
        "  i = i + 1;"
        "}"
        "println(\"finished\");";
    const char* expected = "i=0\n  j=0\ni=1\n  j=0\nfinished\n";
    run_interpreter_test("Control Flow: nested break only exits inner loop", source, expected);
}

static void test_break_does_not_exit_function() {
    const char* source =
        "function test() { var i = 0; while (i < 5) { if (i == 2) break; i=i+1; } return \"broken at \" + i; }"
        "println(test());";
    const char* expected = "broken at 2\n";
    run_interpreter_test("Control Flow: break does not exit function", source, expected);
}

static void test_return_from_loop() {
    const char* source =
        "function find_first() {"
        "  var i = 0;"
        "  while (i < 10) {"
        "    if (i == 3) return i;"
        "    i = i + 1;"
        "  }"
        "  return -1;"
        "}"
        "println(find_first());";
    const char* expected = "3\n";
    run_interpreter_test("Control Flow: return from loop", source, expected);
}

static void test_array_literal() {
    const char* source = "var a = [1, \"two\", 3 * 2, [4]]; println(a);";
    const char* expected = "[1, two, 6, [4]]\n";
    run_interpreter_test("Interpreter: Array literal", source, expected);
}

static void test_object_literal() {
    // Note: Hash map order is not guaranteed. We test for key presence and value.
    const char* source = "var o = {\"a\": 1, b: \"two\"}; println(o[\"a\"]); println(o[\"b\"]);";
    const char* expected = "1\ntwo\n";
    run_interpreter_test("Interpreter: Object literal", source, expected);
}

static void test_index_access() {
    const char* source = 
        "var a = [10, 20, 30];"
        "var o = {\"key\": \"value\"};"
        "println(a[1]);"
        "println(o[\"key\"]);";
    const char* expected = "20\nvalue\n";
    run_interpreter_test("Interpreter: Index access", source, expected);
}

static void test_index_assignment() {
    const char* source =
        "var a = [1, 2, 3];"
        "var o = {};"
        "a[1] = 99;"
        "o[\"new\"] = 123;"
        "println(a[1]);"
        "println(o[\"new\"]);";
    const char* expected = "99\n123\n";
    run_interpreter_test("Interpreter: Index assignment", source, expected);
}

static void test_builtin_len() {
    const char* source = 
        "println(len(\"hello\"));"
        "println(len([1,2,3]));"
        "println(len({a:1,b:2}));";
    const char* expected = "5\n3\n2\n";
    run_interpreter_test("Built-ins: len()", source, expected);
}

static void test_builtin_push_pop() {
    const char* source =
        "var a = [];"
        "push(a, 10);"
        "push(a, 20);"
        "println(a);"
        "var p = pop(a);"
        "println(p);"
        "println(a);";
    const char* expected = "[10, 20]\n20\n[10]\n";
    run_interpreter_test("Built-ins: push() and pop()", source, expected);
}

static void test_builtin_keys() {
    // Cannot test order, but can test that we get the right number of keys
    // and that they can be used to access values.
    const char* source =
        "var o = {b: 2, a: 1};"
        "var k = keys(o);"
        "println(len(k));";
    const char* expected = "2\n";
    run_interpreter_test("Built-ins: keys()", source, expected);
}

static void test_nested_structures() {
    const char* source = 
        "var data = { users: [ {name: \"A\", id:1}, {name:\"B\", id:2} ] };"
        "data[\"users\"][1][\"name\"] = \"Z\";"
        "println(data[\"users\"][1][\"name\"]);";
    const char* expected = "Z\n";
    run_interpreter_test("Interpreter: Nested data structures", source, expected);
}

/*── Dot Syntax Tests ───────────────────────────────────────────────────*/
static void test_dot_access_get() {
    const char* source = "var o = { a: 123, b: \"hello\" }; println(o.a); println(o.b);";
    const char* expected = "123\nhello\n";
    run_interpreter_test("Dot: Get property", source, expected);
}

static void test_dot_access_set() {
    const char* source = "var o = { a: 1 }; o.a = 99; o.b = \"new\"; println(o.a); println(o.b);";
    const char* expected = "99\nnew\n";
    run_interpreter_test("Dot: Set property", source, expected);
}

static void test_dot_access_chain() {
    const char* source = "var o = { a: { b: { c: \"deep\" } } }; println(o.a.b.c);";
    const char* expected = "deep\n";
    run_interpreter_test("Dot: Chained access", source, expected);
}

static void test_dot_access_chain_assignment() {
    const char* source = "var o = { a: { b: 1 } }; o.a.b = \"changed\"; println(o.a.b);";
    const char* expected = "changed\n";
    run_interpreter_test("Dot: Chained assignment", source, expected);
}

static void test_dot_mixed_with_call() {
    const char* source = "var o = { method: function() { return 42; } }; println(o.method());";
    const char* expected = "42\n";
    run_interpreter_test("Dot: Mixed with function call", source, expected);
}

static void test_dot_mixed_with_index() {
    const char* source = "var o = { data: [10, 20] }; o.data[1] = 99; println(o.data[1]);";
    const char* expected = "99\n";
    run_interpreter_test("Dot: Mixed with index access", source, expected);
}

static void test_dot_runtime_errors() {
    const char* err_msg = "Can only index into arrays and objects.";
    const char* array_err_msg = "Array index must be a number.";
    run_interpreter_error_test("Dot: Access on nil", "var x=nil; x.prop;", err_msg);
    run_interpreter_error_test("Dot: Access on number", "123.prop;", err_msg);
    run_interpreter_error_test("Dot: Access on string", "\"hello\".prop;", err_msg);
    run_interpreter_error_test("Dot: Access on array", "[].prop;", array_err_msg);
    run_interpreter_error_test("Dot: Access on function", "function f(){ var a = 0;} f.prop;", err_msg);
}

static void test_dot_syntax_errors() {
    const char* err_msg = "property name after '.'";
    run_parser_error_test("Dot: Syntax error (number after dot)", "var o = {}; o.123;", err_msg);
    run_parser_error_test("Dot: Syntax error (string after dot)", "var o = {}; o.\"prop\";", err_msg);
    run_parser_error_test("Dot: Syntax error (EOF after dot)", "var o = {}; o.;", err_msg);
    run_parser_error_test("Dot: Syntax error (operator after dot)", "var o = {}; o.+;", err_msg);
}

static void run_dot_syntax_tests() {
    test_dot_access_get();
    test_dot_access_set();
    test_dot_access_chain();
    test_dot_access_chain_assignment();
    test_dot_mixed_with_call();
    test_dot_mixed_with_index();
    test_dot_runtime_errors();
    test_dot_syntax_errors();
}

/*── Garbage Collector Tests ──────────────────────────────────────────*/
static void test_gc_simple_collection() {
    const char* source =
        "{"
        // Create a non-interned string that is definitely on the heap.
        "  var a = upper(\"some temporary string\");"
        "}"
        // 'a' is now out of scope and the temporary string should be unreachable.
        "var before = gc.allocated();"
        "gc.collect();"
        "var after = gc.allocated();"
        // The memory used by the temporary string should have been reclaimed.
        "println(before > after);";
    run_interpreter_test("GC: Simple collection", source, "true\n");
}

static void test_gc_global_root() {
    const char* source =
        "var g;"
        "{"
        "  var a = [10, 20];" // Create a new object.
        "  g = a;"           // Put it in a global root.
        "}"
        // 'a' is out of scope, but 'g' should keep the array alive.
        "gc.collect();"
        // Accessing it after GC proves it's still alive and correct.
        "println(g[1]);";
    run_interpreter_test("GC: Global variable keeps object alive", source, "20\n");
}

static void test_gc_array_root() {
    run_gc_test("GC: Array keeps its elements alive",
        "var g = [[]]; gc.collect();",
        NULL // Just check it runs without error
    );
}

static void test_gc_object_root() {
    run_gc_test("GC: Object keeps its values alive",
        "var g = {key: {}}; gc.collect();",
        NULL // Just check it runs without error
    );
}

static void test_gc_closure_root() {
    const char* source =
        "var g;"
        "{"
        "  var local = [1, 2, 3];"
        "  function closure() { println(len(local)); }"
        "  g = closure;"
        "}" // local goes out of scope, but should be kept alive by closure
        "gc.collect();"
        "g();"; // If 'local' was collected, this would crash or error.
    run_interpreter_test("GC: Closure keeps captured variables alive", source, "3\n");
}

static void test_gc_cyclic_reference() {
    const char* source =
        // Intern keys first to not affect memory measurement during the test.
        "var key_a = \"a\"; var key_b = \"b\";"
        "{"
        "  var a = {};"
        "  var b = {};"
        "  a[key_a] = b;"
        "  b[key_b] = a;"
        "}" // a and b are now unreachable from roots, but point to each other.
        "var before = gc.allocated();"
        "gc.collect();"
        "var after = gc.allocated();"
        // The memory for objects 'a' and 'b' should have been reclaimed.
        "println(before > after);";
    run_interpreter_test("GC: Collects cyclic references", source, "true\n");
}

static void test_gc_temporary_rooting_in_expression() {
    const char* source = "println(\"a\" + \"b\" + \"c\" + \"d\" + \"e\");";
    const char* expected = "abcde\n";
    run_interpreter_test("GC: Temporaries in expressions are rooted", source, expected);
}

static void test_gc_builtins() {
    const char* source =
        "var a = { \"key\": \"value\"};"
        "var allocated_before = gc.allocated();"
        "gc.collect();"
        "var allocated_after = gc.allocated();"
        "println(allocated_before > 0);"
        "println(allocated_after > 0);"
        "println(gc.next_gc() > allocated_after);";
    const char* expected = "true\ntrue\ntrue\n";
    run_interpreter_test("GC: Built-in functions work", source, expected);
}

static void test_gc_high_churn() {
    const char* source =
        // This loop creates and immediately discards a new, non-interned string.
        // With DEBUG_STRESS_GC, this will trigger a GC on almost every iteration.
        "var i = 0;"
        "while (i < 500) {"
        "  var temp = \"string\" + \"-\" + i;" // upper() also works well here.
        "  i = i + 1;"
        "}"
        // The test passes if it completes without crashing or memory errors.
        "println(\"churn complete\");";
    const char* expected = "churn complete\n";
    run_interpreter_test("GC: High memory churn", source, expected);
}

static void test_gc_long_chain() {
    const char* source =
        "var head = [nil];"
        "var current = head;"
        "var i = 0;"
        "while (i < 1000) {"
        "  current[0] = [nil];"  // Create a new array (a new list node)
        "  current = current[0];"  // Move to the new node
        "  i = i + 1;"
        "}"
        // At this point, `head` is the root of a 1000-element linked list.
        // A manual GC call here will test if the marker can trace the whole chain.
        "gc.collect();"
        "println(\"long chain survived GC\");";

    const char* expected = "long chain survived GC\n";
    run_interpreter_test("GC: Long linked-list survival", source, expected);
}

static void test_gc_argument_rooting() {
    const char* source =
        "function a(x) { return \"a(\" + x + \")\"; }"
        "function b(x) { return \"b(\" + x + \")\"; }"
        "function c(x) { return \"c(\" + x + \")\"; }"
        "function f(p1, p2, p3) { return p1 + p2 + p3; }"
        // Each call to a, b, c creates a new string object. All three must
        // be protected from GC before f is finally called.
        "println(f(a(\"x\"), b(\"y\"), c(\"z\")));";
    const char* expected = "a(x)b(y)c(z)\n";
    run_interpreter_test("GC: Rooting of multiple temporary arguments", source, expected);
}

static void test_gc_graph_collection() {
    const char* source =
        "var dictator = [];"
        "var i = 0;"
        "while (i < 500) {"
        "  push(dictator, { \"id\": i });" // Create many objects held by one
        "  i = i + 1;"
        "}"
        "var mem_before = gc.allocated();"
        "dictator = nil;" // Abdicate! The entire object graph is now garbage.
        "gc.collect();"
        "var mem_after = gc.allocated();"
        // Most, but not all, memory should be freed (interned strings, etc remain).
        // We expect a significant drop.
        "println(mem_before > mem_after * 2);"; // Check for substantial reduction
    const char* expected = "true\n";
    run_interpreter_test("GC: Collection of a large object graph", source, expected);
}

static void test_gc_closure_complex_root() {
    const char* source =
        "var my_closure;"
        "{"
        "  var big_obj = { data: [] };" // An object with an array
        "  var i = 0; while (i < 100) { push(big_obj[\"data\"], i); i=i+1; }"
        "  function get_obj() { return big_obj; }"
        "  my_closure = get_obj;"
        "}" // big_obj is out of scope here
        "gc.collect();" // Should not collect big_obj
        "var result = my_closure();"
        "println(len(result[\"data\"]));"; // Access after GC
    const char* expected = "100\n";
    run_interpreter_test("GC: Closure correctly roots complex upvalues", source, expected);
}

static void test_gc_string_concat_loop() {
    const char* source =
        "var s = \"\";"
        "var i = 0;"
        "while (i < 100) {"
        "  s = s + i;" // Each iteration creates garbage
        "  i = i + 1;"
        "}"
        // Just needs to run without crashing.
        "println(len(s) > 100);";
    const char* expected = "true\n";
    run_interpreter_test("GC: String concatenation loop", source, expected);
}

/*── For Loop Tests ───────────────────────────────────────────────────────*/
static void test_for_loop_parsing() {
    TEST_START("Parsing: for loop with all clauses");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("for (var i = 0; i < 10; i = i + 1) { println(i); }", &p, &interp);
    ASSERT(ast && ast->type == ST_BLOCK && ast->as.block.count == 1, "Expected single for stmt in block");

    Statement *for_stmt = ast->as.block.list[0];
    ASSERT(for_stmt->type == ST_FOR, "Expected for statement");
    ASSERT(for_stmt->as.for_s.initializer, "For should have an initializer");
    ASSERT(for_stmt->as.for_s.initializer->type == ST_VAR, "Initializer should be a var declaration");
    ASSERT(for_stmt->as.for_s.condition, "For should have a condition");
    ASSERT(for_stmt->as.for_s.condition->type == AST_BINARY, "Condition should be a binary expression");
    ASSERT(for_stmt->as.for_s.increment, "For should have an increment expression");
    ASSERT(for_stmt->as.for_s.increment->type == AST_ASSIGN, "Increment should be an assignment expression");
    ASSERT(for_stmt->as.for_s.body, "For should have a body");
    ASSERT(for_stmt->as.for_s.body->type == ST_BLOCK, "Body should be a block");

    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}

static void test_for_loop_empty_clauses() {
    TEST_START("Parsing: for loop with empty clauses");
    Interpreter interp;
    Parser p;
    Statement *ast = parse_program_test("for (;;) { break; }", &p, &interp);
    ASSERT(ast && ast->type == ST_BLOCK && ast->as.block.count == 1, "Expected single for stmt in block");

    Statement *for_stmt = ast->as.block.list[0];
    ASSERT(for_stmt->type == ST_FOR, "Expected for statement");
   ASSERT(for_stmt->as.for_s.initializer == NULL, "Initializer should be NULL");
    ASSERT(for_stmt->as.for_s.condition == NULL, "Condition should be NULL");
    ASSERT(for_stmt->as.for_s.increment == NULL, "Increment should be NULL");

    parser_destroy(&p);
    interpreter_destroy(&interp);
    TEST_PASS();
}
static void test_for_loop_execution() {
    const char* source = "var total = 0; for (var i = 0; i < 5; i = i + 1) { total = total + i; } println(total);";
    const char* expected = "10\n"; // 0+1+2+3+4
    run_interpreter_test("For Loop: Basic execution", source, expected);
}

static void test_for_loop_scoping() {
    const char* source = "var i = 99; for (var i = 0; i < 2; i=i+1) { println(\"inner: \" + i); } println(\"outer: \" + i);";
   const char* expected = "inner: 0\ninner: 1\nouter: 99\n";
    run_interpreter_test("For Loop: Variable scoping", source, expected);
}

static void test_for_loop_break() {
    const char* source = "var total = 0; for (var i = 0; i < 10; i = i + 1) { if (i == 3) break; total = total + 1; } println(total);";
    const char* expected = "3\n"; // 0, 1, 2
   run_interpreter_test("For Loop: Break statement", source, expected);
}

static void test_for_loop_continue() {
    const char* source = "var s = \"\"; for (var i = 0; i < 5; i = i + 1) { if (i == 2 || i == 4) continue; s = s + i; } println(s);";
   const char* expected = "013\n";
    run_interpreter_test("For Loop: Continue statement", source, expected);
}

/*── Print Cycle Detection Tests ──────────────────────────────────────────*/
static void test_print_cycle_detection() {
    run_interpreter_test(
        "Print: Simple Array Cycle",
        "var a = [1]; push(a, a); println(a);",
        "[1, [...]]\n"
    );

    run_interpreter_test(
        "Print: Simple Object Cycle",
        "var o = {\"k\": \"v\"}; o[\"self\"] = o; println(o);",
        "{\"k\": v, \"self\": {...}}\n"
    );

    run_interpreter_test(
        "Print: Mutual Cycle (Array -> Object)",
        "var a = [1]; var o = {\"arr\": a}; push(a, o); println(a);",
        "[1, {\"arr\": [...]}]\n"
    );

    run_interpreter_test(
        "Print: Mutual Cycle (Object -> Array)",
        "var a = [1]; var o = {\"arr\": a}; push(a, o); println(o);",
        "{\"arr\": [1, {...}]}\n"
    );

    run_interpreter_test(
        "Print: Non-cyclical diamond dependency",
        "var root = []; var child = [1]; push(root, child); push(root, child); println(root);",
        "[[1], [1]]\n"
    );

    run_interpreter_test(
        "Print: Complex nested cycle",
        "var a = [{}]; a[0][\"parent\"] = a; println(a);",
        "[{\"parent\": [...]}]\n"
    );
    
    run_interpreter_test(
        "Print: Deeper cycle in object",
        "var o = { a: { b: {} } }; o.a.b.c = o; println(o);",
        "{\"a\": {\"b\": {\"c\": {...}}}}\n"
    );
}

/*── Tests for the dual-numeric system ──────────────────────────────*/
static void test_numeric_system() {
    TEST_START("Numeric: Large integer parsing");
    ASTNode* ast = parse_expression_test("9007199254740991"); // 2^53 - 1
    ASSERT(ast && ast->type == AST_NUMBER, "Expected number node");
    ASSERT(!ast->as.number.is_double, "Expected integer for large number");
    ASSERT(ast->as.number.as.i_val == 9007199254740991LL, "Value mismatch for large integer");
    TEST_PASS();

    TEST_START("Numeric: Integer parse overflow to double");
    ast = parse_expression_test("9223372036854775808"); // INT64_MAX + 1
    ASSERT(ast && ast->type == AST_NUMBER, "Expected number node");
    ASSERT(ast->as.number.is_double, "Expected double for integer overflow");
    ASSERT(ast->as.number.as.d_val > 9.223372036854775e18, "Double value should be approximately INT64_MAX");
    TEST_PASS();
    
    // Note: The exact double representation might vary slightly. `e+18` is standard.
    run_interpreter_test("Numeric: Integer add overflow", "println(9223372036854775807 + 1);", "9.22337e+18\n");
    run_interpreter_test("Numeric: Integer sub overflow", "println(-9223372036854775807 - 2);", "-9.22337e+18\n");
    run_interpreter_test("Numeric: Integer mul overflow", "println(4611686018427387904 * 3);", "1.38351e+19\n");
    run_interpreter_test("Numeric: Integer division produces double", "println(10 / 4);", "2.5\n");
    run_interpreter_test("Numeric: Mixed-type arithmetic (int + double)", "println(10 + 2.5);", "12.5\n");
    run_interpreter_test("Numeric: Mixed-type arithmetic (double + int)", "println(2.5 + 10);", "12.5\n");
    run_interpreter_test("Numeric: Comparison of mixed types", "println(123 == 123.0); println(123.0 != 124);", "true\ntrue\n");
    run_interpreter_test("Numeric: Builtin len() returns integer", "var a = len(\"hi\"); println(a == 2);", "true\n");
}

/*── Comment Handling Tests ───────────────────────────────────────────*/
static void test_comment_handling() {
    run_interpreter_test("Comments: single line",
                         "// This is a comment at the start of the file.\n"
                         "println(123); // This is a comment at the end of a line.\n"
                         "// This is a comment on its own line.\n"
                         "println(456);",
                         "123\n456\n");

    run_interpreter_test("Comments: multi-line",
                         "/* This is a multi-line comment. */\n"
                         "println(123);\n"
                         "println(/* This is an inline multi-line comment. */ 456);",
                         "123\n456\n");

    run_interpreter_test("Comments: multi-line spanning lines",
                         "println(123);\n"
                         "/* This is a\n"
                         "   multi-line comment with line breaks.\n"
                         "   It even tracks line numbers correctly. */\n"
                         "println(456);",
                         "123\n456\n");
                         
    run_interpreter_test("Comments: empty multi-line comment",
                         "/**/println(1 + /**/ 23);",
                         "24\n");

    run_interpreter_test("Comments: inside expressions",
                         "var x = 1 + /* add two */ 2;\n"
                         "println(x); // should be 3",
                         "3\n");
                         
    run_interpreter_test("Comments: commenting out code",
                         "// var x = 10;\n"
                         "var x = 5;\n"
                         "/* println(99); */\n"
                         "println(x);",
                         "5\n");
                         
    run_interpreter_test("Comments: unclosed multi-line comment",
                         "println(123); /* this is an unclosed comment\n"
                         "println(456); // this should be eaten by the comment",
                         "123\n");
                         
    run_interpreter_test("Comments: mixed and adjacent",
                         "println(1);/*one*///two\n"
                         "println(2);",
                         "1\n2\n");
}

static void run_continue_tests() {
    test_continue_statement_parsing();
    test_simple_continue();
    test_nested_continue();
    test_continue_and_break();
}

static void run_fs_tests() {
    printf("\n--- FS Built-in Tests ---\n");
    // Setup: create a temporary directory for file operations
#ifdef _WIN32
    _mkdir("test_temp_dir");
#else
    mkdir("test_temp_dir", 0777);
#endif

    run_interpreter_test("fs.writeFile and fs.exists",
        "var success = fs.writeFile(\"test_temp_dir/test.txt\", \"hello\");"
        "println(success);"
        "println(fs.exists(\"test_temp_dir/test.txt\"));"
        "println(fs.exists(\"test_temp_dir/nonexistent.txt\"));",
        "true\ntrue\nfalse\n");

    run_interpreter_test("fs.readFile",
        "println(fs.readFile(\"test_temp_dir/test.txt\"));",
        "hello\n");

    run_interpreter_error_test("fs.readFile error",
        "fs.readFile(\"test_temp_dir/nonexistent.txt\");",
        "Could not open file");

    run_interpreter_test("fs.writeFile append",
        "fs.writeFile(\"test_temp_dir/test.txt\", \" world\", true);"
        "println(fs.readFile(\"test_temp_dir/test.txt\"));",
        "hello world\n");

    run_interpreter_test("fs.listDir",
        "fs.writeFile(\"test_temp_dir/file2.txt\", \"\");"
        "var files = array.sort(fs.listDir(\"test_temp_dir\"));" // Sort for consistent test output
        "println(files);",
        "[file2.txt, test.txt]\n");

    run_interpreter_test("fs.remove file",
        "var success = fs.remove(\"test_temp_dir/test.txt\");"
        "println(success);"
        "println(fs.exists(\"test_temp_dir/test.txt\"));",
        "true\nfalse\n");
    
    // Cleanup remaining files from this test group
    remove("test_temp_dir/file2.txt");

    run_interpreter_error_test("fs.remove error",
        "fs.remove(\"test_temp_dir/nonexistent.txt\");",
        "Could not remove");

    // Teardown
#ifdef _WIN32
    _rmdir("test_temp_dir");
#else
    rmdir("test_temp_dir");
#endif
}

static void run_path_tests() {
    printf("\n--- Path Built-in Tests ---\n");
#ifdef _WIN32
    #define SEP "\\"
#else
    #define SEP "/"
#endif
    run_interpreter_test("path.join",
        "println(path.join(\"a\", \"b\", \"c\"));"
        "println(path.join(\"a/\", \"b\"));"
        "println(path.join(\"a\", \"\", \"b\"));",
        "a" SEP "b" SEP "c\n"
        "a" SEP "b\n"
        "a" SEP "b\n");

    run_interpreter_test("path.basename",
        "println(path.basename(\"/foo/bar/baz.txt\"));"
        "println(path.basename(\"/foo/bar/\"));"
        "println(path.basename(\"baz.txt\"));"
        "println(path.basename(\"C:\\\\foo\\\\bar.txt\"));",
        "baz.txt\nbar\nbaz.txt\nbar.txt\n");

    run_interpreter_test("path.dirname",
        "println(path.dirname(\"/foo/bar/baz.txt\"));"
        "println(path.dirname(\"/foo/bar/\"));"
        "println(path.dirname(\"baz.txt\"));"
        "println(path.dirname(\"/\"));",
        "/foo/bar\n/foo\n.\n/\n");

    run_interpreter_test("path.extname",
        "println(path.extname(\"foo.txt\"));"
        "println(path.extname(\"foo.bar.txt\"));"
        "println(path.extname(\"foo.\"));"
        "println(path.extname(\".bashrc\"));"
        "println(path.extname(\"noext\"));"
        "println(path.extname(\"\"));",
        ".txt\n.txt\n.\n.bashrc\n\n\n");
}

static void run_os_tests() {
    printf("\n--- OS Built-in Tests ---\n");
    
    // --- os.platform ---
    run_interpreter_test("os.platform",
        "println(len(os.platform()) > 0);",
        "true\n");

    // --- os.getenv ---
    run_interpreter_test("os.getenv finds existing var (PATH)",
        "println(len(os.getenv(\"PATH\")) > 0);", // PATH should exist on most systems
        "true\n");

    run_interpreter_test("os.getenv non-existent var returns nil",
        "println(os.getenv(\"THIS_VAR_SHOULD_NOT_EXIST_12345\"));",
        "nil\n");
    
    run_interpreter_error_test("os.getenv error (wrong argc)", "os.getenv();", "expects a single string argument");
    run_interpreter_error_test("os.getenv error (wrong type)", "os.getenv(123);", "expects a single string argument");

    // --- os.setenv / os.unsetenv ---
    run_interpreter_test("os.setenv, os.getenv, and os.unsetenv work together",
        "var success_set = os.setenv(\"PRATTSCRIPT_TEST_VAR\", \"hello\");"
        "println(success_set);"
        "println(os.getenv(\"PRATTSCRIPT_TEST_VAR\"));"
        "var success_unset = os.unsetenv(\"PRATTSCRIPT_TEST_VAR\");"
        "println(success_unset);"
        "println(os.getenv(\"PRATTSCRIPT_TEST_VAR\"));",
        "true\nhello\ntrue\nnil\n");

    run_interpreter_error_test("os.setenv error (wrong argc)", "os.setenv(\"a\");", "expects two string arguments");
    run_interpreter_error_test("os.setenv error (wrong type)", "os.setenv(\"a\", 123);", "expects two string arguments");
    run_interpreter_error_test("os.unsetenv error (wrong argc)", "os.unsetenv();", "expects a single string argument");

    // --- os.getcwd / os.setcwd ---
    // C-level setup for the CWD test
    const char* temp_dir = "prattscript_cwd_test_dir";
#ifdef _WIN32
    _mkdir(temp_dir);
#else
    mkdir(temp_dir, 0777);
#endif
    
    // We must build the source string and expected output dynamically to include the temp dir name.
    char setcwd_source[512];
    snprintf(setcwd_source, sizeof(setcwd_source),
        "var old_cwd = os.getcwd();"
        "var success_set = os.setcwd(\"%s\");"
        "println(success_set);"                         // Should be true
        "var new_cwd = os.getcwd();"
        "println(path.basename(new_cwd));"              // Check we are in the new dir
        "var success_reset = os.setcwd(old_cwd);"       // Change back
        "println(success_reset);"                       // Should be true
        "println(path.basename(os.getcwd()) != \"%s\");",// Check we are back
        temp_dir, temp_dir);
    
    char setcwd_expected[128];
    snprintf(setcwd_expected, sizeof(setcwd_expected), "true\n%s\ntrue\ntrue\n", temp_dir);
        
    run_interpreter_test("os.getcwd and os.setcwd", setcwd_source, setcwd_expected);

    run_interpreter_error_test("os.setcwd error (non-existent dir)", 
        "os.setcwd(\"this_dir_really_should_not_exist_abc123\");", 
        "Could not change directory");
    run_interpreter_error_test("os.setcwd error (wrong argc)", "os.setcwd();", "expects a single string argument");
    run_interpreter_error_test("os.setcwd error (wrong type)", "os.setcwd(true);", "expects a single string argument");

    // C-level teardown for the CWD test
#ifdef _WIN32
    _rmdir(temp_dir);
#else
    rmdir(temp_dir);
#endif

    // --- os.exec ---
    run_interpreter_error_test("os.exec error", "os.exec(123);", "expects a string command");
#ifdef _WIN32
    run_interpreter_test("os.exec", "println(os.exec(\"echo test > NUL\"));", "0\n");
#else
    run_interpreter_test("os.exec", "println(os.exec(\"echo test > /dev/null\"));", "0\n");
#endif
}

static void run_date_tests() {
    printf("\n--- Date Built-in Tests ---\n");
    run_interpreter_test("date.now",
        "var n = date.now(); println(n > 1600000000);",
        "true\n");
    
    // This test is timezone-dependent, but should pass in most cases.
    run_interpreter_test("date.format",
        "var ts = 1672531200; // 2023-01-01 00:00:00 UTC\n"
        "var s = date.format(ts, \"%Y-%m-%d\");"
        "println(s == \"2023-01-01\" || s == \"2022-12-31\");",
        "true\n");

#ifdef HAVE_STRPTIME
    run_interpreter_test("date.parse",
        "var ts = date.parse(\"2023-01-01 12:30:00\", \"%Y-%m-%d %H:%M:%S\");"
        "println(typeof(ts));",
        "number\n");
    run_interpreter_error_test("date.parse error", "date.parse(\"invalid\", \"%Y\");", "does not match format");
#endif

    run_interpreter_test("date.utc and date.local",
        "var ts = 0; // The epoch\n"
        "var u = date.utc(ts);"
        "var l = date.local(ts);"
        "println(u.year); println(u.month); println(u.day); println(u.hour);"
        "println(l.year == 1969 || l.year == 1970); // Depending on timezone",
        "1970\n1\n1\n0\ntrue\n");
}



static void run_extra_builtin_tests() {
    run_interpreter_test("Builtin: typeof",
        "println(typeof(123));"
        "println(typeof(1.5));"
        "println(typeof(\"hi\"));"
        "println(typeof(true));"
        "println(typeof(nil));"
        "println(typeof([]));"
        "println(typeof({}));"
        "function f(){} println(typeof(f));"
        "println(typeof(println));",
        "number\nnumber\nstring\nboolean\nnil\narray\nobject\nfunction\nbuiltin\n"
    );
    run_interpreter_error_test("Builtin Error: typeof wrong argc", "typeof(1, 2);", "expects 1 argument");

    run_interpreter_test("Builtin: clock",
        "println(clock() >= 0);", // Cannot test exact value, just that it's a valid number
        "true\n"
    );
    run_interpreter_error_test("Builtin Error: clock wrong argc", "clock(1);", "expects 0 arguments");

    run_interpreter_test("Builtin: assert success", "assert(true); assert(1); assert(\"ok\"); println(\"ok\");", "ok\n");
    run_interpreter_error_test("Builtin Error: assert failure", "assert(false);", "Assertion failed.");
    run_interpreter_error_test("Builtin Error: assert failure with message", "assert(0, \"it was zero\");", "it was zero");
    run_interpreter_error_test("Builtin Error: assert wrong argc", "assert();", "expects 1 or 2 arguments");

    run_interpreter_error_test("Builtin Error: exit wrong type", "exit(\"die\");", "must be an integer");
    run_interpreter_error_test("Builtin Error: exit wrong argc", "exit(0, 1);", "expects 0 or 1 argument");

    run_interpreter_test("Builtin: math.abs",
        "println(math.abs(5));"
        "println(math.abs(-5));"
        "println(math.abs(-3.14));",
        "5\n5\n3.14\n"
    );
    run_interpreter_error_test("Builtin Error: math.abs wrong type", "math.abs(\"hi\");", "expects 1 number argument");

    run_interpreter_test("Builtin: math.floor", "println(math.floor(3.9)); println(math.floor(-3.9));", "3\n-4\n");
    run_interpreter_error_test("Builtin Error: math.floor wrong argc", "math.floor();", "expects 1 number argument");

    run_interpreter_test("Builtin: math.ceil", "println(math.ceil(3.1)); println(math.ceil(-3.1));", "4\n-3\n");
    run_interpreter_error_test("Builtin Error: math.ceil wrong type", "math.ceil([]);", "expects 1 number argument");

    run_interpreter_test("Builtin: math.pow", "println(math.pow(2, 3)); println(math.pow(4, 0.5));", "8\n2\n");
    run_interpreter_error_test("Builtin Error: math.pow wrong argc", "math.pow(2);", "expects 2 number arguments");

    run_interpreter_test("Builtin: math.random", "var r = math.random(); println(r >= 0 && r < 1);", "true\n");

    run_interpreter_test("Builtin: string.trim",
        "println(string.trim(\"  hello  \"));"
        "println(string.trim(\"no-space\"));"
        "println(string.trim(\"   \"));",
        "hello\nno-space\n\n"
    );
    run_interpreter_error_test("Builtin Error: string.trim wrong type", "string.trim(123);", "expects 1 string argument");

    run_interpreter_test("Builtin: string.split",
        "println(string.split(\"a,b,c\", \",\"));"
        "println(string.split(\"hello\", \"\"));"
        "println(string.split(\"a, b,c\", \", \"));"
        "println(string.split(\"only one\", \",\"));",
        "[a, b, c]\n[h, e, l, l, o]\n[a, b,c]\n[only one]\n"
    );
    run_interpreter_error_test("Builtin Error: string.split wrong argc", "string.split(\"a\");", "expects a string to split and a separator");

    run_interpreter_test("Builtin: array.slice",
        "var a = [0, 1, 2, 3, 4];"
        "println(array.slice(a, 1, 3));" // [1, 2]
        "println(array.slice(a, 2));"    // [2, 3, 4]
        "println(array.slice(a, -2));"   // [3, 4]
        "println(array.slice(a, 1, -1));"// [1, 2, 3]
        "println(array.slice(a, 5, 10));"// []
        "println(array.slice(a, 3, 2));",// []
        "[1, 2]\n[2, 3, 4]\n[3, 4]\n[1, 2, 3]\n[]\n[]\n"
    );
    run_interpreter_error_test("Builtin Error: array.slice wrong type", "array.slice({}, 0);", "expects an array");

    run_interpreter_test("Builtin: array.join",
        "var a = [1, \"two\", true];"
        "println(array.join(a));"
        "println(array.join(a, \", \"));"
        "println(array.join([]));",
        "1twotrue\n1, two, true\n\n"
    );
    run_interpreter_error_test("Builtin Error: array.join wrong sep", "array.join([], 1);", "optional string separator");

    printf("Core, Global Helpers\n");
    printf("--------------------\n");
    run_interpreter_test("Core: range()",
        "println(range(0, 5));"
        "println(range(2, 8, 2));"
        "println(range(3, 0, -1));",
        "[0, 1, 2, 3, 4]\n[2, 4, 6]\n[3, 2, 1]\n"
    );
    run_interpreter_error_test("Core: range() error", "range(0, 5, 0);", "step argument cannot be zero");

    run_interpreter_test("Core: min() and max()",
        "println(min(5, 2, 8, 1));"
        "println(max(5, 2, 8, 1));"
        "println(max(10, 15.5));"
        "println(min(-1, -5.5));",
        "1\n8\n15.5\n-5.5\n"
    );
    run_interpreter_error_test("Core: min() error", "min();", "requires at least one argument");
    run_interpreter_error_test("Core: max() error", "max(1, \"a\");", "arguments must be numbers");

    run_interpreter_test("Core: round()",
        "println(round(3.14159));"
        "println(round(3.14159, 2));"
        "println(round(12345.6, -2));", // Note: The C impl does not support negative digits. Test based on actual impl.
                                        // The provided C impl with pow(10, -2) works.
        "3\n3.14\n12300\n"
    );
    run_interpreter_error_test("Core: round() error", "round(1, 2, 3);", "expects 1 or 2 arguments");

    run_interpreter_test("Core: sleep()",
        "sleep(1); println(\"ok\");", // Just test it runs without error.
        "ok\n"
    );
    run_interpreter_error_test("Core: sleep() error", "sleep(-1);", "cannot be negative");


    printf("\nString Object Extensions\n");
    printf("------------------------\n");
    run_interpreter_test("String: replace()",
        "println(string.replace(\"one two two three\", \"two\", \"Z\"));"
        "println(string.replace(\"one two two three\", \"two\", \"Z\", 1));",
        "one Z Z three\none Z two three\n"
    );

    run_interpreter_test("String: startsWith() / endsWith()",
        "println(string.startsWith(\"foobar\", \"foo\"));"
        "println(string.startsWith(\"foobar\", \"bar\"));"
        "println(string.endsWith(\"foobar\", \"bar\"));"
        "println(string.endsWith(\"foobar\", \"foo\"));",
        "true\nfalse\ntrue\nfalse\n"
    );
    run_interpreter_error_test("String: startsWith() error", "string.startsWith(\"a\", 1);", "requires two string arguments");

    run_interpreter_test("String: indexOf()",
        "println(string.indexOf(\"hello world\", \"world\"));"
        "println(string.indexOf(\"hello world\", \"o\"));"
        "println(string.indexOf(\"hello world\", \"o\", 5));"
        "println(string.indexOf(\"hello world\", \"z\"));",
        "6\n4\n7\n-1\n"
    );
    
    run_interpreter_test("String: substring()",
        "println(string.substring(\"abcdef\", 1, 4));"
        "println(string.substring(\"abcdef\", 3, 3));",
        "bcd\n\n"
    );

    run_interpreter_test("String: repeat()",
        "println(string.repeat(\"ha\", 3));",
        "hahaha\n"
    );
    run_interpreter_error_test("String: repeat() error", "string.repeat(\"a\", -1);", "count cannot be negative");

    run_interpreter_test("String: padStart() / padEnd()",
        "println(string.padStart(\"abc\", 5, \"0\"));"
        "println(string.padEnd(\"abc\", 5, \"xy\"));",
        "00abc\nabcxy\n"
    );
    run_interpreter_error_test("String: padStart() error", "string.padStart(\"a\");", "requires a string and an integer length");


    printf("\nArray Object Extensions\n");
    printf("-----------------------\n");
    run_interpreter_test("Array: map()",
        "function double(x) { return x * 2; } "
        "println(array.map([1, 2, 3], double));",
        "[2, 4, 6]\n"
    );
    
    run_interpreter_test("Array: filter()",
        "function isOdd(x) { return x % 2 != 0; } "
        "println(array.filter([1, 2, 3, 4, 5], isOdd));",
        "[1, 3, 5]\n"
    );

    run_interpreter_test("Array: reduce()",
        "function sum(a, b) { return a + b; } "
        "println(array.reduce([1, 2, 3, 4], sum));"
        "println(array.reduce([1, 2, 3, 4], sum, 10));",
        "10\n20\n"
    );
    run_interpreter_error_test("Array: reduce() error", "array.reduce([], function(){});", "reduce of empty array with no initial value");

    run_interpreter_test("Array: indexOf() / includes()",
        "var a = [10, \"b\", 20];"
        "println(array.indexOf(a, \"b\"));"
        "println(array.indexOf(a, 99));"
        "println(array.includes(a, 20));"
        "println(array.includes(a, false));",
        "1\n-1\ntrue\nfalse\n"
    );

    run_interpreter_test("Array: sort() default",
        "var a = [\"c\", \"a\", \"b\", 10, 2]; println(array.sort(a));", // Default is lexicographic for strings
        "[2, 10, a, b, c]\n"
    );

    run_interpreter_test("Array: sort() with numbers",
        "var a = [3, 1, 10, 2]; println(array.sort(a));", // Default on numbers works as expected
        "[1, 2, 3, 10]\n"
    );

    run_interpreter_test("Array: sort() custom function",
        "function descending(a, b) { return b - a; } "
        "var a = [3, 1, 10, 2]; println(array.sort(a, descending));",
        "[10, 3, 2, 1]\n"
    );

    run_interpreter_test("Array: reverse()",
        "var a = [1, 2, 3]; println(array.reverse(a));",
        "[3, 2, 1]\n"
    );

    run_interpreter_test("Array: shuffle()",
        "var a = [1, 2, 3, 4, 5]; array.shuffle(a); println(len(a) == 5);", // Just check it runs and returns same length
        "true\n"
    );

    run_fs_tests();
    run_path_tests();
    run_os_tests();
    run_date_tests();
}

/*── Interpreter tests for the modulo operator ─────────────────────────*/
static void test_modulo_execution() {
    run_interpreter_test("Modulo: Integer operands", "println(10 % 3);", "1\n");
    run_interpreter_test("Modulo: Result is zero", "println(10 % 2);", "0\n");
    run_interpreter_test("Modulo: Negative operands (int)", "println(-10 % 3); println(10 % -3); println(-10 % -3);", "-1\n1\n-1\n");
    run_interpreter_test("Modulo: Floating point operands", "println(10.5 % 3.0);", "1.5\n");
    run_interpreter_test("Modulo: Mixed type (int % double)", "println(10 % 3.5);", "3\n");
    run_interpreter_test("Modulo: Mixed type (double % int)", "println(10.5 % 3);", "1.5\n");
    run_interpreter_test("Modulo: Zero as left operand", "println(0 % 5);", "0\n");
    run_interpreter_test("Modulo: Precedence with addition", "println(5 + 10 % 3);", "6\n");
    run_interpreter_test("Modulo: Precedence with multiplication", "println(2 * 10 % 3);", "2\n");
    run_interpreter_test("Modulo: Left-associativity execution", "println(20 % 7 % 4);", "2\n");
    run_interpreter_error_test("Modulo Error: by zero (int)", "10 % 0;", "Modulo by zero.");
    run_interpreter_error_test("Modulo Error: by zero (float)", "10.0 % 0.0;", "Modulo by zero.");
    run_interpreter_error_test("Modulo Error: Non-numeric operand (left)", "\"a\" % 5;", "Operands for '%' must be numbers.");
    run_interpreter_error_test("Modulo Error: Non-numeric operand (right)", "5 % \"a\";", "Operands for '%' must be numbers.");
}

// ======================= JSON TESTS =======================
static void run_json_tests() {
    printf("\nJSON Built-in Tests\n");
    printf("---------------------\n");

    // --- json.parse ---
    run_interpreter_test("JSON: parse primitives",
        "var json_text = '{\"a\":1, \"b\":-2.5, \"c\":true, \"d\":false, \"e\":null, \"f\":\"hello\"}';"
        "var d = json.parse(json_text);"
        "println(d.a); println(d.b); println(d.c); println(d.d); println(d.e); println(d.f);",
        "1\n-2.5\ntrue\nfalse\nnil\nhello\n"
    );
    
    run_interpreter_test("JSON: parse nested structures",
        "var json_text = '{\"users\":[{\"name\":\"A\",\"id\":1}, {\"name\":\"B\",\"tags\":[1,2]}]}';"
        "var d = json.parse(json_text);"
        "println(d.users[0].name);"
        "println(d.users[1].tags[1]);",
        "A\n2\n"
    );
    
    // Test GC robustness during parsing
    run_gc_test("JSON: parse with GC stress",
        "var big_json = '[' + string.repeat('{\"a\":1},', 1000) + '{\"a\":1}]';"
        "var data = json.parse(big_json);"
        "gc.collect();" // Force collection
        "println(len(data));",
        NULL // Just check for completion without error
    );
    run_interpreter_test("JSON: parse check result of GC stress",
        "var big_json = '[' + string.repeat('{\"a\":1},', 1000) + '{\"a\":1}]';"
        "var data = json.parse(big_json);"
        "println(len(data));",
        "1001\n"
    );

    // --- json.stringify ---
    run_interpreter_test("JSON: stringify primitives",
        "println(json.stringify(123));"
        "println(json.stringify(-4.5));"
        "println(json.stringify(\"hello\"));"
        "println(json.stringify(true));"
        "println(json.stringify(nil));",
        "123\n-4.5\n\"hello\"\ntrue\nnull\n"
    );
    
    run_interpreter_test("JSON: stringify composites",
        "var o = {a:[1, true], b: {c:\"d\"}};"
        "println(json.stringify(o));",
        // Note: key order is not guaranteed. "a", "b" is a common one.
        "{\"a\":[1,true],\"b\":{\"c\":\"d\"}}\n"
    );

    run_interpreter_test("JSON: stringify with pretty print",
        "var o = {a:[1]};"
        "println(json.stringify(o, true));",
        "{\n\t\"a\":\t[1]\n}\n"
    );

    run_interpreter_test("JSON: stringify unstringifiable types",
        "function f(){} var a = [f, println, 1];"
        "println(json.stringify(a));",
        "[null,null,1]\n"
    );
    run_interpreter_test("JSON: stringify unstringifiable top-level",
        "println(json.stringify(function(){}));",
        "null\n"
    );

    // --- stringify cycle detection ---
    run_interpreter_test("JSON: stringify array cycle",
        "var a = [1]; push(a, a);"
        "println(json.stringify(a));",
        "[1,null]\n"
    );

    run_interpreter_test("JSON: stringify object cycle",
        "var o = {}; o.self = o;"
        "println(json.stringify(o));",
        "{\"self\":null}\n"
    );
    
    // --- error cases ---
    run_interpreter_error_test("JSON Error: parse invalid json", "json.parse(\"{key:1}\");", "JSON parse error");
    run_interpreter_error_test("JSON Error: parse wrong type", "json.parse(123);", "expects one string argument");
    run_interpreter_error_test("JSON Error: parse wrong argc", "json.parse();", "expects one string argument");
    run_interpreter_error_test("JSON Error: stringify wrong argc", "json.stringify();", "expects 1 or 2 arguments");
    run_interpreter_error_test("JSON Error: stringify wrong pretty type", "json.stringify({}, 1);", "second argument must be a boolean");
}

static void run_resource_and_stream_tests() {
    printf("\n--- Generic Resource & File Stream Tests ---\n");

   // Setup: create a temporary directory for file operations
#ifdef _WIN32
    _mkdir("test_temp_dir");
#else
    mkdir("test_temp_dir", 0777);
#endif

    // --- Happy Path Tests ---
    run_interpreter_test("Resource: fs.open creates a resource",
        "var f = fs.open(\"test_temp_dir/res_test.txt\", \"w\");"
        "println(typeof(f));"
        "resource.close(f);",
       "resource\n"
    );

    run_interpreter_test("Resource: resource.type() for file",
        "var f = fs.open(\"test_temp_dir/res_test.txt\", \"w\");"
        "println(resource.type(f));"
        "resource.close(f);",
       "core.file\n"
    );

    run_interpreter_test("Stream: write and read",
        "var f = fs.open(\"test_temp_dir/stream_test.txt\", \"w+\");"
        "var written = fs.write(f, \"hello stream\");"
       "println(\"wrote: \" + written);"
        // Re-open to read from start (no seek yet)
        "resource.close(f);"
       "f = fs.open(\"test_temp_dir/stream_test.txt\", \"r\");"
        "var content = fs.read(f, 100);"
        "println(content);"
        "resource.close(f);",
        "wrote: 12\nhello stream\n"
    );
    run_interpreter_test("Stream: read in chunks",
        "fs.writeFile(\"test_temp_dir/chunk_test.txt\", \"1234567890\");"
        "var f = fs.open(\"test_temp_dir/chunk_test.txt\", \"r\");"
        "var c1 = fs.read(f, 4);"
        "var c2 = fs.read(f, 4);"
        "var c3 = fs.read(f, 4);" // reads remaining 2 bytes
        "var c4 = fs.read(f, 4);" // reads 0 bytes (EOF)
        "println(c1);"
        "println(c2);"
        "println(c3);"
        "println(len(c4));"
       "resource.close(f);",
        "1234\n5678\n90\n0\n"
    );

    run_interpreter_test("Resource: open failure returns nil",
        "var f = fs.open(\"test_temp_dir/non/existent/path.txt\", \"r\");"
       "println(f == nil);",
        "true\n"
    );

    run_interpreter_test("Resource: isClosed() status",
        "var f = fs.open(\"test_temp_dir/res_test.txt\", \"w\");"
       "println(resource.isClosed(f));"
        "resource.close(f);"
        "println(resource.isClosed(f));"
        "resource.close(f);" // should be idempotent
        "println(resource.isClosed(f));",
        "false\ntrue\ntrue\n"
    );
    // --- Error Handling Tests ---
    run_interpreter_error_test("Resource: fs.read with wrong resource type",
        "var o = {}; fs.read(o, 10);",
        "fs.read(resource, numBytes) expects a resource and an integer."
    );

    run_interpreter_error_test("Resource: fs.read on closed resource",
        "var f = fs.open(\"test_temp_dir/err_test.txt\", \"w\");"
       "resource.close(f);"
        "fs.read(f, 10);",
        "Cannot read from a closed file resource."
    );

   run_interpreter_error_test("Resource: fs.write on closed resource",
        "var f = fs.open(\"test_temp_dir/err_test.txt\", \"w\");"
        "resource.close(f);"
        "fs.write(f, \"data\");",
        "Cannot write to a closed file resource."
    );

    run_interpreter_error_test("Stream: fs.read negative bytes",
        "var f = fs.open(\"test_temp_dir/err_test.txt\", \"r\");"
        "fs.read(f, -1);"
        "resource.close(f);",
        "Number of bytes to read cannot be negative"
   );

    // --- GC Finalization Test ---
    run_interpreter_test("Resource: GC finalizes unclosed file",
        "{"
        "  var f = fs.open(\"test_temp_dir/gc_test.txt\", \"w\");"
       "  fs.write(f, \"written\");"
        "}" // f is now out of scope
        "gc.collect();" // Should trigger finalizer and close the file
        // If the file wasn't closed, the data might not be flushed to disk.
        // We can't 100% guarantee flush on fclose, but this is a good smoke test.
        "println(fs.readFile(\"test_temp_dir/gc_test.txt\"));",
        "written\n"
   );

    // Teardown
    // This part is a bit tricky since we can't easily execute shell commands.
    // We'll rely on the OS to clean up the temp dir later.
    // For a production test suite, a shell script wrapper would handle this.
    remove("test_temp_dir/res_test.txt");
    remove("test_temp_dir/stream_test.txt");
    remove("test_temp_dir/chunk_test.txt");
    remove("test_temp_dir/err_test.txt");
    remove("test_temp_dir/gc_test.txt");
#ifdef _WIN32
    _rmdir("test_temp_dir");
#else
    rmdir("test_temp_dir");
#endif
}

/*── Run all tests ───────────────────────────────────────────────────────*/
static void run_all_tests() {
    printf("PrattLib Comprehensive Test Suite\n");
    printf("==================================\n\n");
    
    // Basic functionality tests
    test_simple_number();
    test_simple_string();
    test_decimal_number();
    test_identifier();
    
    // Arithmetic expression tests
    test_simple_addition();
    test_simple_subtraction();
    test_multiplication();
    test_division();
    test_modulo_operator();
    
    // Precedence tests
    test_precedence_multiplication_first();
    test_precedence_with_parentheses();
    test_complex_precedence();
    test_right_associativity_exponent();
    
    // Unary operator tests
    test_unary_minus();
    test_unary_precedence();
    test_double_unary();
    
    // Associativity tests
    test_left_associativity_addition();
    test_left_associativity_subtraction();
    
    // Grouping tests
    test_nested_parentheses_1();
    test_nested_parentheses_2();
    test_multiple_groupings();
    
    // Complex expression tests
    test_complex_arithmetic();
    test_identifier_arithmetic();
    test_mixed_expressions();
    
    // Edge case tests
    test_single_character_identifier();
    test_zero();
    test_decimal_zero();
    test_large_number();
    test_long_identifier();
    
    // Whitespace handling tests
    test_whitespace_handling();
    test_no_whitespace();
    
    printf("\nComment Handling Tests\n");
    printf("--------------------------\n");
    test_comment_handling();

    // Error handling tests
    printf("\nError Handling Tests\n");
    printf("----------------------\n");
    test_empty_input();
    test_incomplete_expression();
    test_unmatched_parentheses();
    test_invalid_character_error();
    test_sparse_token_id_error();
    test_number_out_of_range_error();
    test_forced_oom_error();

    // Performance/stress tests
    test_deeply_nested_expression();
    test_long_chain_expression();
    
    // Real-world inspired tests
    test_formula_like_expression();
    test_distance_formula();
    test_physics_formula();

    test_negative_token_id_error();
    test_recursion_limit();
    test_error_recovery();
    

    printf("\nFunction Call Parsing Tests\n");
    printf("---------------------------\n");
    test_call_no_args();
    test_call_one_arg();
    test_call_multiple_args();
    test_call_nested();
    test_call_precedence();
    test_call_error_missing_rparen();

    test_comparison_simple();
    test_comparison_precedence();
    test_comparison_chaining();
    test_logical_precedence();
    test_logical_short_circuit_ast();
    test_ternary_simple();
    test_ternary_precedence();
    test_ternary_right_associativity();

    printf("\nStatement-Level Parsing Tests\n");
    printf("-----------------------------\n");
    test_declaration_and_use();
    test_shadowing_in_blocks();
    test_break_statement_parsing();

    printf("\nInterpreter Execution & Control Flow Tests\n");
    printf("------------------------------------------\n");
    test_variable_shadowing();
    test_assignment_expression();
    test_if_else_nesting();
    test_if_without_else();
    test_while_loop();
    test_simple_break();
    test_nested_break();
    test_break_does_not_exit_function();
    test_modulo_execution();
    
    printf("\nContinue Statement Tests\n");
    printf("------------------------\n");
    run_continue_tests();

    // Run function tests
    test_function_and_return();
    test_function_closure();
    test_function_recursion();
    test_return_from_loop();

    printf("\nControl Flow Error Tests\n");
    printf("--------------------------\n");
    test_control_flow_error();

    // Tests for composite types
    printf("\nComposite Type Tests\n");
    printf("----------------------\n");
    test_array_literal();
    test_object_literal();
    test_index_access();
    test_index_assignment();
    test_builtin_len();
    test_builtin_push_pop();
    test_builtin_keys();
    test_nested_structures();

    printf("\nDot Syntax Tests\n");
    printf("------------------\n");
    run_dot_syntax_tests();

    printf("\nComparison Logic Tests\n");
    printf("------------------------\n");
    test_comparison_logic_primitives();
    test_comparison_logic_arrays();
    test_comparison_logic_objects();
    test_comparison_logic_strict_typing();
    test_relational_operators_on_composites();
    
    printf("\nNumeric System Tests\n");
    printf("----------------------\n");
    test_numeric_system();

    printf("\nGarbage Collector Tests\n");
    printf("-------------------------\n");
    test_gc_simple_collection();
    test_gc_global_root();
    test_gc_array_root();
    test_gc_object_root();
    test_gc_closure_root();
    test_gc_cyclic_reference();
    test_gc_temporary_rooting_in_expression();
    test_gc_builtins();
    test_gc_high_churn();
    test_gc_long_chain();
    test_gc_argument_rooting();
    test_gc_graph_collection();
    test_gc_closure_complex_root();
    test_gc_string_concat_loop();

    printf("\nPrint Cycle Detection Tests\n");
    printf("---------------------------\n");
    test_print_cycle_detection();

    printf("\nFor Loop Tests\n");
    printf("------------------\n");
    test_for_loop_parsing();
    test_for_loop_empty_clauses();
    test_for_loop_execution();
    test_for_loop_scoping();
    test_for_loop_break();
    test_for_loop_continue();

    printf("\nExtra Builtin Function Tests\n");
    printf("----------------------------\n");
    run_extra_builtin_tests();
    
    // Run JSON tests
    run_json_tests();

    // --- Test function for streaming I/O ---
    run_resource_and_stream_tests();

    // Summary
    printf("\n==================================\n");
    printf("Test Results:\n");
    printf("  Total tests: %d\n", test_count);
    printf("  Passed:      %d\n", test_passed);
    printf("  Failed:      %d\n", test_failed);
    printf("  Success rate: %.1f%%\n", 
           test_count > 0 ? (100.0 * test_passed / test_count) : 0.0);
    
    if (test_failed == 0) {
        printf("\nAll tests passed!\n");
    } else {
        printf("\nSome tests failed. Check output above.\n");
    }
}

/*── Main function ───────────────────────────────────────────────────────*/
int main(int argc, char *argv[]) {
    printf("PrattLib Test Program\n");
    printf("Testing Pratt Parser Library functionality\n\n");
    
    // Check if user wants to run a specific test
    if (argc > 1) {
        printf("Interactive mode: parsing '%s'\n", argv[1]);
        ASTNode *ast = parse_expression_test(argv[1]);
        if (ast) {
            printf("Parsed successfully! AST:\n");
            print_ast(ast);
        } else {
            printf("Failed to parse expression.\n");
        }
        destroy_tracked_harnesses();
        return 0;
    }
    
    // Run all tests
    run_all_tests();

    /* Free every arena that is still alive from the individual tests.   */
    destroy_tracked_harnesses();

    return test_failed > 0 ? 1 : 0;
}
