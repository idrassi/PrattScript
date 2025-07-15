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

#ifndef PRATT_DEFAULT_H
#define PRATT_DEFAULT_H

#include "pratt.h"

/*── Default token set for the example language ──────────────────────────*/
typedef enum {
    T_NUMBER = T_USER_BASE,
    T_STRING,
    T_IDENT,

    /* Keywords */
    T_VAR, T_IF, T_ELSE, T_WHILE, T_RETURN, T_FUNCTION, T_BREAK,
    T_CONTINUE, T_FOR,
    T_TRUE, T_FALSE, T_NIL,

    /* Operators */
    T_PLUS, T_MINUS, T_STAR, T_SLASH, T_PERCENT, T_CARET,
    T_EQUAL, T_EQUAL_EQUAL, T_BANG_EQUAL, T_LESS, T_LESS_EQUAL, T_GREATER, T_GREATER_EQUAL,
    T_AMP_AMP, T_PIPE_PIPE,
    T_QUESTION, T_COLON,

    /* Bitwise Operators */
    T_AMP, T_PIPE, T_TILDE, T_LESS_LESS, T_GREATER_GREATER,

    T_DOT,
    T_COMMA,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE, T_SEMICOLON,
    T_LBRACKET, T_RBRACKET,
    /* Sentinel to mark the end of the token list for rule array sizing. */
    T_TOKEN_COUNT
} DefaultTokenType;


/*── Default parselets ───────────────────────────────────────────────────*/
ASTNode *default_number_prefix   (Parser *p);
ASTNode *default_string_prefix   (Parser *p);
ASTNode *default_ident_prefix    (Parser *p);
ASTNode *default_grouping_prefix (Parser *p);
ASTNode *default_unary_prefix    (Parser *p);
ASTNode *default_binary_infix    (Parser *p, ASTNode *left);
ASTNode *default_ternary_infix   (Parser *p, ASTNode *left);
ASTNode *default_function_expression_prefix(Parser *p);
ASTNode *default_array_prefix    (Parser *p);
ASTNode *default_object_prefix   (Parser *p);
ASTNode *default_index_infix     (Parser *p, ASTNode *left);
ASTNode *default_dot_infix       (Parser *p, ASTNode *left);


/*── A token name provider for this default token set ───────────────────*/
const char *default_token_name(PrattTokenType t);

/*── A drop-in rule table ─────────────────────────────────────────────────*/
extern const ParseRule default_rules[];
extern const size_t    default_rule_count;

#endif /* PRATT_DEFAULT_H */
