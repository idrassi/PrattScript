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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
// Include interpreter to access the interning function via parser's user_ctx
#include "interpreter_core.h"

/*
 *  ======================================================================
 *  DEFAULT PARSELETS – demo only
 *  ----------------------------------------------------------------------
 *  This file shows how to plug a rule table into the generic Pratt core.
 *  Real host languages are expected to supply their own table (possibly
 *  building on these helpers) and should not rely on the exact node
 *  kinds emitted here.
 *  ======================================================================
 */

extern void *arena_alloc(Arena *a, size_t sz);
extern void parser_error(Parser *p, const char *fmt, ...);
extern ASTNode *parse_precedence(Parser *p, int prec);
extern int check(Parser *p, PrattTokenType t);
extern PrattToken advance(Parser *p);
extern int consume(Parser *p, PrattTokenType t, const char *desc);
extern PrattToken peek(Parser *p);

/*── Helpers to allocate ASTNodes in arena ───────────────────────────────*/
static ASTNode *new_node(Parser *p, ASTNodeType type) {
    ASTNode *n = arena_alloc(p->arena, sizeof(ASTNode));
    if (!n) { parser_error(p, "Out of memory"); return NULL; }
    n->type = type;
    n->user_data = NULL; /* Initialize user data field */
    return n;
}

ASTNode *default_literal_prefix(Parser *p) {
    PrattToken t = p->cur;
    switch (t.type) {
        case T_TRUE: {
            ASTNode *n = new_node(p, AST_BOOL);
            if (n) {
                n->as.boolean.value = 1;
                n->as.boolean.tok = t;
            }
            return n;
        }
        case T_FALSE: {
            ASTNode *n = new_node(p, AST_BOOL);
            if (n) {
                n->as.boolean.value = 0;
                n->as.boolean.tok = t;
            }
            return n;
        }
        case T_NIL: {
            ASTNode *n = new_node(p, AST_NIL);
            if (n) {
                n->as.nil.tok = t;
            }
            return n;
        }
        default: // Should be unreachable
            parser_error(p, "Invalid literal token.");
            return NULL;
    }
}

/*── ternary: cond ? then_branch : else_branch ───────────────────────────*/
static ASTNode *new_ternary_node(Parser *p, ASTNode *cond, ASTNode *then_b, ASTNode *else_b) {
    ASTNode *n = new_node(p, AST_TERNARY);
    if (n) {
        n->as.ternary.cond = cond;
        n->as.ternary.then_branch = then_b;
        n->as.ternary.else_branch = else_b;
    }
    return n;
}

/*── number: parse as int64_t or double, intelligently ──────────────────*/
ASTNode *default_number_prefix(Parser *p) {
    PrattToken t = p->cur;
    /* Micro-opt: use a stack buffer for most numeric literals */
    enum { SMALL_NUM_BUF = 64 };
    char stack_buf[SMALL_NUM_BUF];
    char *buf;
    if (t.length < SMALL_NUM_BUF) {
        buf = stack_buf;
    } else {
        buf = arena_alloc(p->arena, t.length + 1);
        if (!buf) { parser_error(p, "Out of memory"); return NULL; }
    }
    memcpy(buf, t.start, t.length);
    buf[t.length] = '\0';

    // Check if it looks like a double (contains '.', 'e', or 'E')
    if (memchr(buf, '.', t.length) || memchr(buf, 'e', t.length) || memchr(buf, 'E', t.length)) {
        errno = 0;
        char *endptr;
        double val = strtod(buf, &endptr);
        if (endptr == buf || *endptr != '\0') {
            parser_error(p, "Invalid floating-point number '%.*s'", (int)t.length, t.start);
            return NULL;
        }
        if (errno == ERANGE) {
            parser_error(p, "Floating-point number out of range '%.*s'", (int)t.length, t.start);
            return NULL;
        }

        ASTNode *n = new_node(p, AST_NUMBER);
        if (n) {
            n->as.number.is_double = true;
            n->as.number.as.d_val = val;
            n->as.number.tok = t;
        }
        return n;
    }

    // Otherwise, try to parse as a 64-bit integer
    errno = 0;
    char *endptr;
    long long val_ll = strtoll(buf, &endptr, 10);

    // If it overflows, fall back to parsing as a double for max precision.
    if (errno == ERANGE) {
        errno = 0;
        double val_d = strtod(buf, &endptr);
        if (errno == ERANGE) {
             parser_error(p, "Number out of range '%.*s'", (int)t.length, t.start);
             return NULL;
        }
        ASTNode *n = new_node(p, AST_NUMBER);
        if (n) {
            n->as.number.is_double = true;
            n->as.number.as.d_val = val_d;
            n->as.number.tok = t;
        }
        return n;
    }

    if (endptr == buf || *endptr != '\0') {
        parser_error(p, "Invalid integer '%.*s'", (int)t.length, t.start);
        return NULL;
    }

    // Successfully parsed as an integer.
    ASTNode *n = new_node(p, AST_NUMBER);
    if (n) {
        n->as.number.is_double = false;
        n->as.number.as.i_val = val_ll;
        n->as.number.tok = t;
    }
    return n;
}

/*── string: copy into arena ────────────────────────────────────────────*/
ASTNode *default_string_prefix(Parser *p) {
    PrattToken t = p->cur;
    ASTNode *n = new_node(p, AST_STRING);
    if (!n) return NULL;

    // The string value from the source code is copied into the parser's
    // temporary arena. It will be converted to a GC-managed ObjString
    // during interpretation.
    char *value = arena_alloc(p->arena, t.length + 1);
    if (!value) { parser_error(p, "Out of memory"); return NULL; }
    memcpy(value, t.start, t.length);
    value[t.length] = '\0';

    n->as.string.value = value;
    n->as.string.length = t.length;
    n->as.string.tok = t;
    return n;
}

/*── identifier: intern string and create node ──────────────────────────*/
ASTNode *default_ident_prefix(Parser *p) {
    PrattToken t = p->cur;
    ASTNode *n = new_node(p, AST_IDENT);
    if (!n) return NULL;

    // Intern the identifier string
    Interpreter *interp = p->user_ctx;
    // interpreter_intern_string now returns an ObjString*, but for the AST
    // we just need the canonical const char* for now. We can store this
    // directly in the ASTIdent node.
    ObjString *name_obj = interpreter_intern_string(interp, t.start, t.length);
    if (!name_obj) { parser_error(p, "Out of memory during string interning"); return NULL; }
    const char *name = name_obj->chars;

    n->as.ident.name = name;
    n->as.ident.tok  = t;
    return n;
}

/*── grouping: '(' expr ')' ─────────────────────────────────────────────*/
ASTNode *default_grouping_prefix(Parser *p) {
    ASTNode *expr = parse_precedence(p, PREC_NONE); // Parse inner expression
    if (!consume(p, T_RPAREN, "')' to match opening parenthesis")){
        return NULL; // Error, expected ')'
    }
    return expr;
}

/*── array literal: '[' (expr (',' expr)*)? ']' ──────────────────────────*/
ASTNode *default_array_prefix(Parser *p) {
    PrattToken bracket = p->cur;
    ASTNode **elements = NULL;
    size_t capacity = 0;
    size_t count = 0;

    if (!check(p, T_RBRACKET)) {
        do {
            if (count >= capacity) {
                size_t old_capacity = capacity;
                capacity = old_capacity < 8 ? 8 : old_capacity * 2;
                ASTNode **new_elements = arena_alloc(p->arena, capacity * sizeof(ASTNode *));
                if (!new_elements) return NULL;
                if (elements) memcpy(new_elements, elements, old_capacity * sizeof(ASTNode *));
                elements = new_elements;
            }
            // Allow dangling comma before the ']'
            if (check(p, T_RBRACKET)) break;

            ASTNode *element = parse_precedence(p, PREC_ASSIGNMENT);
            if (!element) return NULL;
            elements[count++] = element;
        } while (check(p, T_COMMA) && (advance(p), 1));
    }

    if (!consume(p, T_RBRACKET, "']' after array elements")) return NULL;

    ASTNode *n = new_node(p, AST_ARRAY);
    if (!n) return NULL;
    n->as.array.elements = elements;
    n->as.array.count = count;
    n->as.array.bracket = bracket;
    return n;
}

/*── object literal: '{' (key:expr (',' key:expr)*)? '}' ──────────────────*/
ASTNode *default_object_prefix(Parser *p) {
    PrattToken brace = p->cur;
    const char **keys = NULL;
    ASTNode **values = NULL;
    size_t capacity = 0;
    size_t count = 0;

    if (!check(p, T_RBRACE)) {
        do {
            // Allow dangling comma before '}'
            if (check(p, T_RBRACE)) break;

            // Grow arrays if necessary
            if (count >= capacity) {
                size_t old_cap = capacity;
                capacity = old_cap < 4 ? 4 : old_cap * 2;
                const char **new_keys = arena_alloc(p->arena, capacity * sizeof(const char*));
                ASTNode **new_values = arena_alloc(p->arena, capacity * sizeof(ASTNode*));
                if (!new_keys || !new_values) return NULL;
                if (keys) memcpy(new_keys, keys, old_cap * sizeof(const char*));
                if (values) memcpy(new_values, values, old_cap * sizeof(ASTNode*));
                keys = new_keys;
                values = new_values;
            }

            // Parse key (must be an identifier or a string)
            if (!check(p, T_IDENT) && !check(p, T_STRING)) {
                parser_error(p, "Expected identifier or string as object key.");
                return NULL;
            }
            PrattToken key_tok = advance(p);
            Interpreter* interp = p->user_ctx;
            ObjString* key_obj = interpreter_intern_string(interp, key_tok.start, key_tok.length);
            if (!key_obj) { parser_error(p, "Out of memory"); return NULL; }
            const char* key_name = key_obj->chars;
            keys[count] = key_name;

            if (!consume(p, T_COLON, "':' after object key")) return NULL;

            // Parse value
            values[count] = parse_precedence(p, PREC_ASSIGNMENT);
            if (!values[count]) return NULL;

            count++;
        } while (check(p, T_COMMA) && (advance(p), 1));
    }

    if (!consume(p, T_RBRACE, "'}' after object properties")) return NULL;
    
    ASTNode *n = new_node(p, AST_OBJECT);
    if (!n) return NULL;
    n->as.object.keys = keys;
    n->as.object.values = values;
    n->as.object.count = count;
    n->as.object.brace = brace;
    return n;
}


/*── unary: prefix '-' or '~' ───────────────────────────────────────────*/
ASTNode *default_unary_prefix(Parser *p) {
    PrattToken op = p->cur; // The dispatcher consumed the '-' or '~'
    ASTNode *child = parse_precedence(p, PREC_UNARY);
    if (!child) return NULL;
    ASTNode *n = new_node(p, AST_UNARY);
    if (!n) return NULL;
    n->as.unary.op    = op;
    n->as.unary.child = child;
    return n;
}

/*── binary: left (op) right ────────────────────────────────────────────*/
ASTNode *default_binary_infix(Parser *p, ASTNode *left) {
    PrattToken op = p->cur;
    const ParseRule *rule = &p->rules[op.type];
    ASTNode *right = parse_precedence(p, rule->rbp);
    if (!right) return NULL;

    ASTNode *n = new_node(p, AST_BINARY);
    if (!n) return NULL;
    n->as.binary.op = op;
    n->as.binary.left = left;
    n->as.binary.right = right;
    return n;
}

/*── ternary: cond ? then : else ────────────────────────────────────────*/
ASTNode *default_ternary_infix(Parser *p, ASTNode *left) {
    // We've just consumed '?', `left` is the condition.
    // The precedence of the middle ('then') expression is low, allowing complex expressions.
    // Parsing stops automatically at ':' because it has no infix rule.
    ASTNode *then_branch = parse_precedence(p, PREC_NONE);
    if (!then_branch) return NULL;

    if (!consume(p, T_COLON, "':' for ternary operator")) {
        return NULL;
    }

    // The 'else' branch's precedence is governed by the '?' operator's right-binding power.
    const ParseRule *rule = &p->rules[T_QUESTION];
    ASTNode *else_branch = parse_precedence(p, rule->rbp);
    if (!else_branch) return NULL;

    return new_ternary_node(p, left, then_branch, else_branch);
}

/*── infix: function call - expr '(' arglist? ')' ──────────────────────*/
static ASTNode *default_call_infix(Parser *p, ASTNode *callee) {
    ASTNode **args = NULL;
    size_t capacity = 0;
    size_t argc = 0;

    // Check for arguments. If next token is not ')', we have an arg list.
    if (!check(p, T_RPAREN)) {
        do {
            if (argc >= capacity) {
                size_t old_capacity = capacity;
                capacity = old_capacity < 8 ? 8 : old_capacity * 2;
                ASTNode **new_args = arena_alloc(p->arena, capacity * sizeof(ASTNode *));
                if (!new_args) return NULL;
                if (args) memcpy(new_args, args, old_capacity * sizeof(ASTNode *));
                args = new_args;
            }
            // Parse a full expression for the argument. The lowest precedence (PREC_NONE)
            // is required to allow expressions like `x = 5` as arguments.
            ASTNode *arg = parse_precedence(p, PREC_NONE);
            if (!arg) return NULL;
            args[argc++] = arg;
        } while (check(p, T_COMMA) && (advance(p), 1));
    }

    PrattToken rparen_tok;
    if (!consume(p, T_RPAREN, "')' after arguments")) return NULL;
    rparen_tok = p->cur;

    ASTNode *n = new_node(p, AST_CALL);
    if (!n) return NULL;

    n->as.call.callee = callee;
    n->as.call.args = args;
    n->as.call.argc = argc;
    n->as.call.rparen = rparen_tok;

    return n;
}


/*── infix: dot property access - expr '.' IDENT ───────────────────────*/
ASTNode *default_dot_infix(Parser *p, ASTNode *left) {
    PrattToken dot_tok = p->cur; // The '.' token

    if (!consume(p, T_IDENT, "property name after '.'")) {
        return NULL;
    }
    PrattToken property_tok = p->cur;

    // To simplify the interpreter, we transform `obj.prop` into the
    // same AST structure as `obj["prop"]`. This means creating an
    // AST_STRING node for the property name and wrapping it all in an
    // AST_INDEX node.

    // 1. Create an AST_STRING node for the property identifier.
    ASTNode *index_expr = new_node(p, AST_STRING);
    if (!index_expr) return NULL;

    // The string value needs to be copied into the arena from the token's lexeme.
    char *value = arena_alloc(p->arena, property_tok.length + 1);
    if (!value) {
        parser_error(p, "Out of memory");
        return NULL;
    }
    memcpy(value, property_tok.start, property_tok.length);
    value[property_tok.length] = '\0';

    index_expr->as.string.value = value;
    index_expr->as.string.length = property_tok.length;
    index_expr->as.string.tok = property_tok;

    // 2. Create the wrapping AST_INDEX node.
    ASTNode *n = new_node(p, AST_INDEX);
    if (!n) return NULL;

    n->as.index.object = left;
    n->as.index.index = index_expr;
    n->as.index.bracket = dot_tok; // Use the dot token for location info.

    return n;
}

/*── infix: index access - expr '[' index_expr ']' ─────────────────────*/
ASTNode *default_index_infix(Parser *p, ASTNode *object) {
    PrattToken bracket = p->cur;
    ASTNode *index = parse_precedence(p, PREC_NONE);
    if (!index) return NULL;

    if (!consume(p, T_RBRACKET, "']' after index expression")) return NULL;

    ASTNode *n = new_node(p, AST_INDEX);
    if (!n) return NULL;
    n->as.index.object = object;
    n->as.index.index = index;
    n->as.index.bracket = bracket;
    return n;
}

/*── infix: assignment - target = value ──────────────────────────────────*/
ASTNode *default_assignment_infix(Parser *p, ASTNode *left) {
    // The left-hand side must be a valid l-value (e.g., identifier, index)
    if (left->type != AST_IDENT && left->type != AST_INDEX) {
        parser_error(p, "Invalid assignment target.");
        return NULL;
    }

    PrattToken op = p->cur; // The '=' token
    // Assignment is right-associative, so we parse with a slightly lower precedence
    ASTNode *value = parse_precedence(p, PREC_ASSIGNMENT - 1);
   if (!value) return NULL;

    ASTNode *n = new_node(p, AST_ASSIGN);
    if (!n) return NULL;
    n->as.assign.target = left;
    n->as.assign.value = value;
    n->as.assign.op = op;
    return n;
}

/*── Helpers to allocate Statement nodes in arena ───────────────────────*/
static Statement *new_statement(Parser *p, StatementType type) {
    Statement *s = arena_alloc(p->arena, sizeof(Statement));
    if (!s) { parser_error(p, "Out of memory"); return NULL; }
    s->type = type;
    s->user_data = NULL;
    return s;
}

/*── Break Statement: 'break' ';' ───────────────────────────────────────*/
static Statement *break_statement(Parser *p) {
    PrattToken keyword = p->next; // The 'break' token
    advance(p); // consume 'break'
    if (!consume(p, T_SEMICOLON, "';' after 'break'")) return NULL;

    Statement *s = new_statement(p, ST_BREAK);
    if (!s) return NULL;
    s->as.break_s.keyword = keyword;
    return s;
}

/*── Continue Statement: 'continue' ';' ─────────────────────────────────*/
static Statement *continue_statement(Parser *p) {
    PrattToken keyword = p->next; // The 'continue' token
    advance(p); // consume 'continue'
    if (!consume(p, T_SEMICOLON, "';' after 'continue'")) return NULL;

    Statement *s = new_statement(p, ST_CONTINUE);
    if (!s) return NULL;
    s->as.continue_s.keyword = keyword;
    return s;
}

/*── If Statement: 'if' '(' condition ')' then ('else' else)? ───────────*/
/* Forward declarations for statement parsers */
static Statement *for_statement(Parser *p);
static Statement *declaration(Parser *p);
static Statement *expression_statement(Parser *p);
static Statement *if_statement(Parser *p) {
    advance(p); // consume 'if'
    if (!consume(p, T_LPAREN, "'(' after 'if'")) return NULL;
    ASTNode *condition = parse_precedence(p, PREC_NONE);
    if (!condition) return NULL;
    if (!consume(p, T_RPAREN, "')' after if condition")) return NULL;

    Statement *then_branch = parse_statement(p);
    if (!then_branch) return NULL;

    Statement *else_branch = NULL;
    if (check(p, T_ELSE)) {
        advance(p); // consume 'else'
        else_branch = parse_statement(p);
        if (!else_branch) return NULL;
    }

    Statement *s = new_statement(p, ST_IF);
    if (!s) return NULL;
    s->as.if_s.condition = condition;
    s->as.if_s.then_branch = then_branch;
    s->as.if_s.else_branch = else_branch;
    return s;
}

/*── While Statement: 'while' '(' condition ')' body ────────────────────*/
static Statement *while_statement(Parser *p) {
    advance(p); // consume 'while'
    if (!consume(p, T_LPAREN, "'(' after 'while'")) return NULL;
    ASTNode *condition = parse_precedence(p, PREC_NONE);
    if (!condition) return NULL;
    if (!consume(p, T_RPAREN, "')' after while condition")) return NULL;

    Statement *body = parse_statement(p);
    if (!body) return NULL;

    Statement *s = new_statement(p, ST_WHILE);
    if (!s) return NULL;
    s->as.while_s.condition = condition;
    s->as.while_s.body = body;
    return s;
}

/*── For Statement: 'for' '(' (varDecl | exprStmt | ';') condition? ';' increment? ')' body --*/
static Statement *for_statement(Parser *p) {
    advance(p); // consume 'for'

    if (!consume(p, T_LPAREN, "'(' after 'for'")) return NULL;

    // 1. Initializer Clause
    Statement* initializer = NULL;
    if (check(p, T_SEMICOLON)) {
        // Empty initializer, just consume the semicolon
        advance(p);
    } else if (check(p, T_VAR)) {
        // A full variable declaration statement (which consumes its own ';')
        initializer = declaration(p);
    } else { // It's an expression statement.
        initializer = expression_statement(p);
    }
    // The initializer parsers above consume their own semicolon, so we don't.
    if (p->had_error) return NULL; 

    // 2. Condition Clause
    ASTNode* condition = NULL;
    if (!check(p, T_SEMICOLON)) {
        condition = parse_precedence(p, PREC_NONE);
    }
    if (!consume(p, T_SEMICOLON, "';' after loop condition")) return NULL;
    if (p->had_error) return NULL;
    // 3. Increment Clause
    ASTNode* increment = NULL;
    if (!check(p, T_RPAREN)) {
        increment = parse_precedence(p, PREC_NONE);
    }
    if (!consume(p, T_RPAREN, "')' after for clauses")) return NULL;
    if (p->had_error) return NULL;

    // 4. Body
    Statement* body = parse_statement(p);
    if (!body) return NULL;

    Statement* s = new_statement(p, ST_FOR);
    if (!s) return NULL;
    s->as.for_s.initializer = initializer;
    s->as.for_s.condition = condition;
    s->as.for_s.increment = increment;
    s->as.for_s.body = body;
    return s;
}

/*── Return Statement: 'return' expression? ';' ─────────────────────────*/
static Statement *return_statement(Parser *p) {
    PrattToken keyword = p->next;
    advance(p); // consume 'return'
    ASTNode *value = NULL;
    if (!check(p, T_SEMICOLON)) {
        value = parse_precedence(p, PREC_NONE);
        if (!value) return NULL;
    }

    if (!consume(p, T_SEMICOLON, "';' after return value")) return NULL;

    Statement *s = new_statement(p, ST_RETURN);
    if (!s) return NULL;
    s->as.ret.value = value;
    s->as.ret.keyword = keyword;
    return s;
}

/*── Expression Statement: (l-value '=' expression)? | expression ';' ───*/
static Statement *expression_statement(Parser *p) {
    // An expression statement is simply an expression followed by a semicolon.
    // Since assignment is now a proper expression, this handles both cases.
    ASTNode *expr = parse_precedence(p, PREC_NONE);
    if (!expr) return NULL;

    if (!consume(p, T_SEMICOLON, "';' after expression")) return NULL;

    Statement *s = new_statement(p, ST_EXPR);
    if (!s) return NULL;
    s->as.expr.expr = expr;
    return s;
}

/*── Variable Declaration: 'var' IDENT ('=' expression)? ';' ─────────────*/
static Statement *declaration(Parser *p) {
    advance(p); // consume 'var'
    if (!consume(p, T_IDENT, "variable name")) return NULL;
    PrattToken name_tok = p->cur;

    // Intern the variable name here at parse time.
    Interpreter *interp = p->user_ctx;
    ObjString* name_obj = interpreter_intern_string(interp, name_tok.start, name_tok.length);
    if (!name_obj) { parser_error(p, "Out of memory"); return NULL; }
    const char *name = name_obj->chars;

    ASTNode *initializer = NULL;
    if (check(p, T_EQUAL)) {
        advance(p); // consume '='
        initializer = parse_precedence(p, PREC_NONE);
        if (!initializer) return NULL;
    }

    if (!consume(p, T_SEMICOLON, "';' after variable declaration")) return NULL;

    Statement *s = new_statement(p, ST_VAR);
    if (!s) return NULL;
    s->as.var.name = name;
    s->as.var.name_tok = name_tok;
    s->as.var.initializer = initializer;
    return s;
}

// Function Declaration: 'function' IDENT '(' params? ')' block
static Statement *function_statement(Parser *p) {
    advance(p); // consume 'function'
    if (!consume(p, T_IDENT, "function name")) return NULL;
    PrattToken name_tok = p->cur;

    Interpreter *interp = p->user_ctx;
    ObjString *name_obj = interpreter_intern_string(interp, name_tok.start, name_tok.length);
    if (!name_obj) { parser_error(p, "Out of memory"); return NULL; }
    const char *name = name_obj->chars;

    if (!consume(p, T_LPAREN, "'(' after function name")) return NULL;

    const char **params = NULL;
    PrattToken *param_toks = NULL;
    size_t param_count = 0;
    size_t param_capacity = 0;

    if (!check(p, T_RPAREN)) {
        do {
            if (!consume(p, T_IDENT, "parameter name")) return NULL;
            PrattToken param_tok = p->cur;
            ObjString *param_obj = interpreter_intern_string(interp, param_tok.start, param_tok.length);
            const char *param_name = param_obj->chars;

            if (param_count >= param_capacity) {
                size_t old_cap = param_capacity;
                param_capacity = old_cap < 8 ? 8 : old_cap * 2;
                const char **new_params = arena_alloc(p->arena, param_capacity * sizeof(const char*));
                PrattToken *new_toks = arena_alloc(p->arena, param_capacity * sizeof(PrattToken));
                if (!new_params || !new_toks) { parser_error(p, "Out of memory"); return NULL; }
                if (params) memcpy(new_params, params, old_cap * sizeof(const char*));
                if (param_toks) memcpy(new_toks, param_toks, old_cap * sizeof(PrattToken));
                params = new_params;
                param_toks = new_toks;
            }
            params[param_count] = param_name;
            param_toks[param_count] = param_tok;
            param_count++;
        } while (check(p, T_COMMA) && (advance(p), 1));
    }

    if (!consume(p, T_RPAREN, "')' after parameters")) return NULL;
    if (!check(p, T_LBRACE)) {
        parser_error(p, "Expected '{' before function body.");
        return NULL;
    }

    Statement *body = parse_block(p);
    if (!body) return NULL;

    Statement *s = new_statement(p, ST_FUNCTION);
    if (!s) return NULL;

    s->as.func.name = name;
    s->as.func.name_tok = name_tok;
    s->as.func.params = params;
    s->as.func.param_toks = param_toks;
    s->as.func.param_count = param_count;
    s->as.func.body = body;
    return s;
}

// Function Expression: 'function' [IDENT] '(' params? ')' block
ASTNode *default_function_expression_prefix(Parser *p) {
    // The 'function' keyword was already consumed.
    const char *name = NULL;

    // Function expressions can be anonymous or named.
    if (check(p, T_IDENT)) {
        advance(p);
        PrattToken name_tok = p->cur;
        Interpreter *interp = p->user_ctx;
        ObjString *name_obj = interpreter_intern_string(interp, name_tok.start, name_tok.length);
        if (!name_obj) { parser_error(p, "Out of memory"); return NULL; }
        name = name_obj->chars;
    }

    if (!consume(p, T_LPAREN, "'(' after function name")) return NULL;

    const char **params = NULL;
    PrattToken *param_toks = NULL;
    size_t param_count = 0;
    size_t param_capacity = 0;
    Interpreter *interp = p->user_ctx;

    if (!check(p, T_RPAREN)) {
        do {
           if (!consume(p, T_IDENT, "parameter name")) return NULL;
            PrattToken param_tok = p->cur;
            ObjString *param_obj = interpreter_intern_string(interp, param_tok.start, param_tok.length);
            if (!param_obj) { parser_error(p, "Out of memory"); return NULL; }
            const char *param_name = param_obj->chars;

            if (param_count >= param_capacity) {
                size_t old_cap = param_capacity;
                param_capacity = old_cap < 8 ? 8 : old_cap * 2;
               const char **new_params = arena_alloc(p->arena, param_capacity * sizeof(const char*));
                PrattToken *new_toks = arena_alloc(p->arena, param_capacity * sizeof(PrattToken));
                if (!new_params || !new_toks) { parser_error(p, "Out of memory"); return NULL; }
                if (params) memcpy(new_params, params, old_cap * sizeof(const char*));
                if (param_toks) memcpy(new_toks, param_toks, old_cap * sizeof(PrattToken));
                params = new_params;
                param_toks = new_toks;
            }
           params[param_count] = param_name;
            param_toks[param_count] = param_tok;
            param_count++;
        } while (check(p, T_COMMA) && (advance(p), 1));
    }

    if (!consume(p, T_RPAREN, "')' after parameters")) return NULL;
    if (!check(p, T_LBRACE)) {
       parser_error(p, "Expected '{' before function body.");
        return NULL;
    }

    Statement *body = parse_block(p);
    if (!body) return NULL;

    ASTNode *n = new_node(p, AST_FUNCTION);
   if (!n) return NULL;

    n->as.function.name = name;
    n->as.function.params = params;
    n->as.function.param_toks = param_toks;
    n->as.function.param_count = param_count;
    n->as.function.body = body;
   return n;
}

/*── Top-level Statement Dispatcher ─────────────────────────────────────*/
Statement *parse_statement(Parser *p) {
    if (check(p, T_IF))       return if_statement(p);
    if (check(p, T_BREAK))    return break_statement(p);
    if (check(p, T_CONTINUE)) return continue_statement(p);
    if (check(p, T_VAR))      return declaration(p);
    if (check(p, T_WHILE))    return while_statement(p);
    if (check(p, T_FOR))      return for_statement(p);
    if (check(p, T_RETURN))   return return_statement(p);
    if (check(p, T_LBRACE))   return parse_block(p);
    // function keyword is a statement, not an expression prefix
    if (check(p, T_FUNCTION)) return function_statement(p);

    // Default to an expression statement
    return expression_statement(p);
}

/*── Block Statement: '{' statement* '}' ────────────────────────────────*/
Statement *parse_block(Parser *p) {
    consume(p, T_LBRACE, "'{' to start a block");

    // Use a dynamic array allocated in the arena
    size_t capacity = 8;
    size_t count = 0;
    Statement **statements = arena_alloc(p->arena, capacity * sizeof(Statement*));
    if (!statements) return NULL; // OOM already handled by arena_alloc

    while (!check(p, T_RBRACE) && !check(p, T_EOF)) {
        if (p->had_error) break;
        if (count == capacity) {
            size_t new_capacity = capacity * 2;
            Statement **new_statements = arena_alloc(p->arena, new_capacity * sizeof(Statement*));
            if (!new_statements) return NULL;
            memcpy(new_statements, statements, count * sizeof(Statement*));
            statements = new_statements;
            capacity = new_capacity;
        }
        statements[count++] = parse_statement(p);
    }

    if (!consume(p, T_RBRACE, "'}' to end a block")) return NULL;

    Statement *s = new_statement(p, ST_BLOCK);
    if (!s) return NULL;
    s->as.block.list = statements;
    s->as.block.count = count;
    return s;
}

/*── A token name provider for the default token set ────────────────────*/
const char *default_token_name(PrattTokenType t) {
    if (t < T_USER_BASE) {
        switch (t) {
            case T_EOF:   return "end of file";
            case T_ERROR: return "invalid token";
            default:      return "<? internal ?>";
        }
    }
    switch (t) {
        case T_NUMBER:    return "number";
        case T_STRING:    return "string";
        case T_IDENT:     return "identifier";
        // --- KEYWORDS ---
        case T_VAR:       return "'var'";
        case T_IF:        return "'if'";
        case T_ELSE:      return "'else'";
        case T_WHILE:     return "'while'";
        case T_FOR:       return "'for'";
        case T_RETURN:    return "'return'";
        case T_BREAK:     return "'break'";
        case T_CONTINUE:  return "'continue'";
        case T_FUNCTION:  return "'function'";
        case T_TRUE:      return "'true'";
        case T_FALSE:     return "'false'";
        case T_NIL:       return "'nil'";

        case T_PLUS:      return "'+'";
        case T_MINUS:     return "'-'";
        case T_STAR:      return "'*'";
        case T_SLASH:     return "'/'";
        case T_PERCENT:   return "'%'";
        case T_CARET:     return "'^'";
        case T_EQUAL:     return "'='";
        case T_EQUAL_EQUAL: return "'=='";
        case T_BANG_EQUAL: return "'!='";
        case T_LESS: return "'<'";
        case T_LESS_EQUAL: return "'<='";
        case T_GREATER: return "'>'";
        case T_GREATER_EQUAL: return "'>='";
        case T_AMP_AMP: return "'&&'";
        case T_PIPE_PIPE: return "'||'";
        case T_QUESTION: return "'?'";
        case T_COLON: return "':'";
        case T_AMP: return "'&'";
        case T_PIPE: return "'|'";
        case T_TILDE: return "'~'";
        case T_LESS_LESS: return "'<<'";
        case T_GREATER_GREATER: return "'>>'";

        case T_DOT:       return "'.'";
        case T_COMMA:     return "','";
        case T_LPAREN:    return "'('";
        case T_RPAREN:    return "')'";
        case T_LBRACE:    return "'{'";
        case T_RBRACE:    return "'}'";
        case T_LBRACKET:  return "'['";
        case T_RBRACKET:  return "']'";
        case T_SEMICOLON: return "';'";
        default:          return "<? unknown ?>";
    }
}

/*── The default rule table with explicit lbp/rbp ────────────────────────*/
const ParseRule default_rules[T_TOKEN_COUNT] = {
    [T_NUMBER]    = PRATT_RULE(default_number_prefix,   NULL,                 PREC_NONE,   0),
    [T_STRING]    = PRATT_RULE(default_string_prefix,   NULL,                 PREC_NONE,   0),
    [T_IDENT]     = PRATT_RULE(default_ident_prefix,    NULL,                 PREC_NONE,   0),
    [T_TRUE]      = PRATT_RULE(default_literal_prefix,  NULL,                 PREC_NONE,   0),
    [T_FALSE]     = PRATT_RULE(default_literal_prefix,  NULL,                 PREC_NONE,   0),
    [T_NIL]       = PRATT_RULE(default_literal_prefix,  NULL,                 PREC_NONE,   0),

    // Keywords have no parselets; they are handled by the RD parser.
    [T_VAR]       = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_IF]        = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_ELSE]      = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_WHILE]     = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_FOR]       = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_RETURN]    = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_BREAK]     = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_CONTINUE]  = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_FUNCTION]  = PRATT_RULE(default_function_expression_prefix, NULL, PREC_NONE, 0),

    // Assignment is handled by expression_statement.
    [T_EQUAL]     = PRATT_RULE(NULL, default_assignment_infix, PREC_ASSIGNMENT, 0),

    /* Unary and Arithmetic Operators */
    [T_MINUS]     = PRATT_RULE(default_unary_prefix,    default_binary_infix, PREC_TERM,   PREC_TERM),
    [T_PLUS]      = PRATT_RULE(NULL,                    default_binary_infix, PREC_TERM,   PREC_TERM),
    [T_STAR]      = PRATT_RULE(NULL,                    default_binary_infix, PREC_FACTOR, PREC_FACTOR),
    [T_SLASH]     = PRATT_RULE(NULL,                    default_binary_infix, PREC_FACTOR, PREC_FACTOR),
    [T_PERCENT]   = PRATT_RULE(NULL,                    default_binary_infix, PREC_FACTOR, PREC_FACTOR),

    /* Bitwise Operators */
    [T_TILDE]     = PRATT_RULE(default_unary_prefix,    NULL,                 PREC_UNARY,  0),
    [T_LESS_LESS] = PRATT_RULE(NULL,                    default_binary_infix, PREC_SHIFT,  PREC_SHIFT),
    [T_GREATER_GREATER] = PRATT_RULE(NULL,              default_binary_infix, PREC_SHIFT,  PREC_SHIFT),
    [T_AMP]       = PRATT_RULE(NULL,                    default_binary_infix, PREC_BITWISE_AND, PREC_BITWISE_AND),
    [T_CARET]     = PRATT_RULE(NULL,                    default_binary_infix, PREC_BITWISE_XOR, PREC_BITWISE_XOR),
    [T_PIPE]      = PRATT_RULE(NULL,                    default_binary_infix, PREC_BITWISE_OR,  PREC_BITWISE_OR),

    /* Comparisons (left-associative) */
    [T_EQUAL_EQUAL]   = PRATT_RULE(NULL, default_binary_infix, PREC_COMPARISON, PREC_COMPARISON),
    [T_BANG_EQUAL]    = PRATT_RULE(NULL, default_binary_infix, PREC_COMPARISON, PREC_COMPARISON),
    [T_LESS]          = PRATT_RULE(NULL, default_binary_infix, PREC_COMPARISON, PREC_COMPARISON),
    [T_LESS_EQUAL]    = PRATT_RULE(NULL, default_binary_infix, PREC_COMPARISON, PREC_COMPARISON),
    [T_GREATER]       = PRATT_RULE(NULL, default_binary_infix, PREC_COMPARISON, PREC_COMPARISON),
    [T_GREATER_EQUAL] = PRATT_RULE(NULL, default_binary_infix, PREC_COMPARISON, PREC_COMPARISON),

    /* Logical (left-associative) */
    [T_PIPE_PIPE] = PRATT_RULE(NULL, default_binary_infix, PREC_LOGICAL_OR,  PREC_LOGICAL_OR),
    [T_AMP_AMP]   = PRATT_RULE(NULL, default_binary_infix, PREC_LOGICAL_AND, PREC_LOGICAL_AND),

    /* Ternary (right-associative) */
    [T_QUESTION]  = PRATT_RULE(NULL, default_ternary_infix, PREC_CONDITIONAL, PREC_CONDITIONAL - 1),
    [T_COLON]     = PRATT_RULE(NULL, NULL, PREC_NONE, 0), /* Delimiter, no parselets */

    /* Grouping, Calls, and Composites ---------------------------------- */
    [T_LPAREN]    = PRATT_RULE(default_grouping_prefix, default_call_infix,   PREC_CALL,   0),
    [T_LBRACKET]  = PRATT_RULE(default_array_prefix,    default_index_infix,  PREC_CALL,   0),
    [T_LBRACE]    = PRATT_RULE(default_object_prefix,   NULL,                 PREC_NONE,   0),
    [T_RPAREN]    = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
    [T_RBRACKET]  = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
    [T_RBRACE]    = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
    [T_DOT]       = PRATT_RULE(NULL,                    default_dot_infix,    PREC_CALL,   0),
    [T_COMMA]     = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
    [T_SEMICOLON] = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
};

const size_t default_rule_count = T_TOKEN_COUNT;

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
_Static_assert(T_NUMBER < T_TOKEN_COUNT, "Rule table size check");
_Static_assert(T_IDENT < T_TOKEN_COUNT, "Rule table size check");
_Static_assert(T_SEMICOLON < T_TOKEN_COUNT, "Rule table size check");
#endif
