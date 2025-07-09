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
extern int check(Parser *p, TokenType t);
extern Token advance(Parser *p);
extern int consume(Parser *p, TokenType t, const char *desc);
extern Token peek(Parser *p);

/*── Helpers to allocate ASTNodes in arena ───────────────────────────────*/
static ASTNode *new_node(Parser *p, ASTNodeType type) {
    ASTNode *n = arena_alloc(p->arena, sizeof(ASTNode));
    if (!n) { parser_error(p, "Out of memory"); return NULL; }
    n->type = type;
    n->user_data = NULL; /* Initialize user data field */
    return n;
}

ASTNode *default_literal_prefix(Parser *p) {
    Token t = p->cur;
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

/*── number: with strtod error checks ───────────────────────────────────*/
ASTNode *default_number_prefix(Parser *p) {
    Token t = p->cur;
    /* Micro-opt: use a stack buffer for most numeric literals */
    enum { SMALL_NUM_BUF = 64 };
    char stack_buf[SMALL_NUM_BUF];
    char *buf;
    if (t.length < SMALL_NUM_BUF) {
        buf = stack_buf;
    } else {
        buf = arena_alloc(p->arena, t.length + 1);
        if (!buf) return parser_error(p, "Out of memory"), NULL;
    }
    memcpy(buf, t.start, t.length);
    buf[t.length] = '\0';

    errno = 0;
    char *endptr;
    double val = strtod(buf, &endptr);
    if (endptr == buf)
        return parser_error(p, "Invalid number '%.*s'",
                            (int)t.length, t.start), NULL;
    if (errno == ERANGE)
        return parser_error(p, "Number out of range '%.*s'",
                            (int)t.length, t.start), NULL;

    ASTNode *n = new_node(p, AST_NUMBER);
    if (n) {
        n->as.number.value = val;
        n->as.number.tok   = t;
    }
    return n;
}

/*── string: copy into arena ────────────────────────────────────────────*/
ASTNode *default_string_prefix(Parser *p) {
    Token t = p->cur;
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
    Token t = p->cur;
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
    Token bracket = p->cur;
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
    Token brace = p->cur;
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
            Token key_tok = advance(p);
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


/*── unary: prefix '-' ─────────────────────────────────────────────────*/
ASTNode *default_unary_prefix(Parser *p) {
    Token op = p->cur; // The dispatcher consumed the '-'
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
    Token op = p->cur;
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
            ASTNode *arg = parse_precedence(p, PREC_ASSIGNMENT); // Can't have comma operator in args
            if (!arg) return NULL;
            args[argc++] = arg;
        } while (check(p, T_COMMA) && (advance(p), 1));
    }

    Token rparen_tok;
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


/*── infix: index access - expr '[' index_expr ']' ─────────────────────*/
ASTNode *default_index_infix(Parser *p, ASTNode *object) {
    Token bracket = p->cur;
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


/*── Helpers to allocate Statement nodes in arena ───────────────────────*/
static Statement *new_statement(Parser *p, StatementType type) {
    Statement *s = arena_alloc(p->arena, sizeof(Statement));
    if (!s) { parser_error(p, "Out of memory"); return NULL; }
    s->type = type;
    s->user_data = NULL;
    return s;
}

/*── If Statement: 'if' '(' condition ')' then ('else' else)? ───────────*/
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

/*── Return Statement: 'return' expression? ';' ─────────────────────────*/
static Statement *return_statement(Parser *p) {
    Token keyword = p->next;
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
    // An expression statement can be a simple expression or an assignment.
    // Parse the potential left-hand side first.
    ASTNode *expr = parse_precedence(p, PREC_ASSIGNMENT);
    if (!expr) return NULL;

    // If an '=' follows, it's an assignment.
    if (check(p, T_EQUAL)) {
        // The left-hand side of an assignment must be a valid l-value.
        // In our language, this is an identifier or an index expression.
        if (expr->type != AST_IDENT && expr->type != AST_INDEX) {
            parser_error(p, "Invalid assignment target.");
            // NOTE: The 'expr' node will be freed when the parser arena is destroyed.
            // No need to free it manually here.
            return NULL;
        }

        advance(p); // Consume the '='

        // Parse the right-hand side. Assignment is right-associative.
        ASTNode *value = parse_precedence(p, PREC_ASSIGNMENT - 1);
        if (!value) return NULL;

        if (!consume(p, T_SEMICOLON, "';' after assignment")) return NULL;

        Statement *s = new_statement(p, ST_ASSIGN);
        if (!s) return NULL;
        s->as.assign.target = expr;
        s->as.assign.value = value;
        return s;
    }

    // If no '=' followed, it's a regular expression statement.
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
    Token name_tok = p->cur;

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
    Token name_tok = p->cur;

    Interpreter *interp = p->user_ctx;
    ObjString *name_obj = interpreter_intern_string(interp, name_tok.start, name_tok.length);
    if (!name_obj) { parser_error(p, "Out of memory"); return NULL; }
    const char *name = name_obj->chars;

    if (!consume(p, T_LPAREN, "'(' after function name")) return NULL;

    const char **params = NULL;
    Token *param_toks = NULL;
    size_t param_count = 0;
    size_t param_capacity = 0;

    if (!check(p, T_RPAREN)) {
        do {
            if (!consume(p, T_IDENT, "parameter name")) return NULL;
            Token param_tok = p->cur;
            ObjString *param_obj = interpreter_intern_string(interp, param_tok.start, param_tok.length);
            const char *param_name = param_obj->chars;

            if (param_count >= param_capacity) {
                size_t old_cap = param_capacity;
                param_capacity = old_cap < 8 ? 8 : old_cap * 2;
                const char **new_params = arena_alloc(p->arena, param_capacity * sizeof(const char*));
                Token *new_toks = arena_alloc(p->arena, param_capacity * sizeof(Token));
                if (!new_params || !new_toks) { parser_error(p, "Out of memory"); return NULL; }
                if (params) memcpy(new_params, params, old_cap * sizeof(const char*));
                if (param_toks) memcpy(new_toks, param_toks, old_cap * sizeof(Token));
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

/*── Top-level Statement Dispatcher ─────────────────────────────────────*/
Statement *parse_statement(Parser *p) {
    if (check(p, T_IF))       return if_statement(p);
    if (check(p, T_VAR))      return declaration(p);
    if (check(p, T_WHILE))    return while_statement(p);
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
const char *default_token_name(TokenType t) {
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
        case T_RETURN:    return "'return'";
        case T_FUNCTION:  return "'function'";

        case T_PLUS:      return "'+'";
        case T_MINUS:     return "'-'";
        case T_STAR:      return "'*'";
        case T_SLASH:     return "'/'";
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
    [T_RETURN]    = PRATT_RULE(NULL, NULL, PREC_NONE, 0),
    [T_FUNCTION]  = PRATT_RULE(NULL, NULL, PREC_NONE, 0),

    // Assignment is handled by expression_statement.
    [T_EQUAL]     = PRATT_RULE(NULL, NULL, PREC_ASSIGNMENT, 0),

    [T_MINUS]     = PRATT_RULE(default_unary_prefix,    default_binary_infix, PREC_TERM,   PREC_TERM),
    [T_PLUS]      = PRATT_RULE(NULL,                    default_binary_infix, PREC_TERM,   PREC_TERM),
    [T_STAR]      = PRATT_RULE(NULL,                    default_binary_infix, PREC_FACTOR, PREC_FACTOR),
    [T_SLASH]     = PRATT_RULE(NULL,                    default_binary_infix, PREC_FACTOR, PREC_FACTOR),
    [T_CARET]     = PRATT_RULE(NULL,                    default_binary_infix, PREC_POWER,  PREC_POWER-1),

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
    [T_DOT]       = PRATT_RULE(NULL,                    NULL,                 PREC_CALL,   0),
    [T_COMMA]     = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
    [T_SEMICOLON] = PRATT_RULE(NULL,                    NULL,                 PREC_NONE,   0),
};

const size_t default_rule_count = T_TOKEN_COUNT;

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
_Static_assert(T_NUMBER < T_TOKEN_COUNT, "Rule table size check");
_Static_assert(T_IDENT < T_TOKEN_COUNT, "Rule table size check");
_Static_assert(T_SEMICOLON < T_TOKEN_COUNT, "Rule table size check");
#endif
