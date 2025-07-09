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

#include "pratt_lexer.h"
#include <ctype.h>
#include <string.h>

// --- Keyword table ---
typedef struct {
    const char *name;
    TokenType   type;
} Keyword;

static Keyword keywords[] = {
    {"else",   T_ELSE},
    {"false",  T_FALSE},
    {"function", T_FUNCTION},
    {"if",     T_IF},
    {"nil",    T_NIL},
    {"return", T_RETURN},
    {"true",   T_TRUE},
    {"var",    T_VAR},
    {"while",  T_WHILE},
    {NULL,     T_EOF} // Sentinel
};

/*── Initialize the test lexer ───────────────────────────────────────────*/
void pratt_lexer_init(PrattLexer *lex, const char *source) {
    lex->source = source;
    lex->pos    = 0;
    lex->length = strlen(source);
    lex->line   = 1;
    lex->col    = 1;
}

/*── Helper to check if we're at end ─────────────────────────────────────*/
static int is_at_end(PrattLexer *lex) {
    return lex->pos >= lex->length;
}

/*── Helper to peek current character ────────────────────────────────────*/
static char lexer_peek(PrattLexer *lex) {
    if (is_at_end(lex)) return '\0';
    return lex->source[lex->pos];
}

/*── Helper to advance and return current character ──────────────────────*/
static char advance_char(PrattLexer *lex) {
    if (is_at_end(lex)) return '\0';
    char c = lex->source[lex->pos++];
    if (c == '\n') {
        lex->line++;
        lex->col = 1;
    } else {
        lex->col++;
    }
    return c;
}

/*── Helper to peek next character ───────────────────────────────────────*/
static char peek_next(PrattLexer *lex) {
    if (lex->pos + 1 >= lex->length) return '\0';
    return lex->source[lex->pos + 1];
}

/*── Skip whitespace ─────────────────────────────────────────────────────*/
static void skip_whitespace(PrattLexer *lex) {
    for (;;) {
        char c = lexer_peek(lex);
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance_char(lex);
        } else {
            break;
        }
    }
}

/*── Make a token ────────────────────────────────────────────────────────*/
static Token make_token(PrattLexer *lex, TokenType type, const char *start, size_t length) {
    Token token;
    token.type   = type;
    token.start  = start;
    token.length = length;
    token.line   = lex->line;
    token.col    = lex->col - (int)length;
    return token;
}

/*── Scan a number ───────────────────────────────────────────────────────*/
static Token scan_number(PrattLexer *lex) {
    const char *start = &lex->source[lex->pos];
    
    while (isdigit(lexer_peek(lex))) {
        advance_char(lex);
    }
    
    /* fraction */
    if (lexer_peek(lex) == '.' && isdigit(peek_next(lex))) {
        advance_char(lex); // consume '.'
        while (isdigit(lexer_peek(lex))) {
            advance_char(lex);
        }
    }

    /* exponent: (e|E)(+|−)?DIGITS+  */
    if (lexer_peek(lex) == 'e' || lexer_peek(lex) == 'E') {
        char s1 = peek_next(lex);
        char s2 = (s1 == '+' || s1 == '-') && lex->pos + 2 < lex->length
                  ? lex->source[lex->pos + 2] : s1;
        if (isdigit(s2)) {                 /* really an exponent */
            advance_char(lex);             /* consume 'e' / 'E'  */
            if (lexer_peek(lex) == '+' || lexer_peek(lex) == '-')
                advance_char(lex);
            while (isdigit(lexer_peek(lex))) advance_char(lex);
        }
    }
    
    size_t length = &lex->source[lex->pos] - start;
    return make_token(lex, T_NUMBER, start, length);
}

/*── Scan a string ───────────────────────────────────────────────────────*/
static Token scan_string(PrattLexer *lex) {
    /* The opening quote was eaten by pratt_lexer_next().
       lex->pos now points at the first character of the string body. */
    const char *start_of_literal  = &lex->source[lex->pos - 1]; /* for errors */
    const char *start_of_content  = &lex->source[lex->pos];

    while (lexer_peek(lex) != '"' && !is_at_end(lex)) {
        char c = lexer_peek(lex);
        if (c == '\\') {
            advance_char(lex); // consume the backslash
            if (is_at_end(lex)) {
                // Incomplete escape sequence at end of input
                return make_token(lex, T_ERROR,
                                  start_of_literal,
                                  &lex->source[lex->pos] - start_of_literal);
            }
            char escaped = lexer_peek(lex);
            switch (escaped) {
                case '"':   // \"
                case '\\':  // '\\'
                case '/':   // \/
                case 'b':   // \b (backspace)
                case 'f':   // \f (form feed)
                case 'n':   // \n (newline)
                case 'r':   // \r (carriage return)
                case 't':   // \t (tab)
                    advance_char(lex); // consume the escaped character
                    break;
                case 'u':   // \uXXXX (Unicode escape)
                    advance_char(lex); // consume 'u'
                    // Validate 4 hex digits
                    for (int i = 0; i < 4; i++) {
                        if (is_at_end(lex) || !isxdigit(lexer_peek(lex))) {
                            return make_token(lex, T_ERROR,
                                              start_of_literal,
                                              &lex->source[lex->pos] - start_of_literal);
                        }
                        advance_char(lex);
                    }
                    break;
                default:
                    // Invalid escape sequence
                    return make_token(lex, T_ERROR,
                                      start_of_literal,
                                      &lex->source[lex->pos] - start_of_literal);
            }
        } else {
            advance_char(lex);
        }
    }

    if (is_at_end(lex)) {
        return make_token(lex, T_ERROR,
                          start_of_literal,
                          &lex->source[lex->pos] - start_of_literal);
    }

    size_t length = &lex->source[lex->pos] - start_of_content;
    advance_char(lex); // Consume the closing '"'
    return make_token(lex, T_STRING, start_of_content, length);
}

/*── Helper to check if a lexeme is a keyword ────────────────────────────*/
static TokenType check_keyword(const char *start, size_t length) {
    for (Keyword *k = keywords; k->name; k++) {
        if (strlen(k->name) == length && memcmp(start, k->name, length) == 0) {
            return k->type;
        }
    }
    return T_IDENT;
}

/*── Scan an identifier or keyword ───────────────────────────────────────*/
static Token scan_identifier(PrattLexer *lex) {
    const char *start = &lex->source[lex->pos];
    
    while (isalnum(lexer_peek(lex)) || lexer_peek(lex) == '_') {
        advance_char(lex);
    }
    
    size_t length = &lex->source[lex->pos] - start;
    TokenType type = check_keyword(start, length); // Check against keywords
    return make_token(lex, type, start, length);
}

/*── Main lexer function ─────────────────────────────────────────────────*/
Token pratt_lexer_next(void *ctx) {
    PrattLexer *lex = (PrattLexer *)ctx;
    
    skip_whitespace(lex);
    
    if (is_at_end(lex)) {
        return make_token(lex, T_EOF, &lex->source[lex->pos], 0);
    }
    
    char c = lexer_peek(lex);
    const char *start = &lex->source[lex->pos];
    
    // Numbers
    if (isdigit(c)) {
        return scan_number(lex);
    }
    
    // Identifiers
    if (isalpha(c) || c == '_') {
        return scan_identifier(lex);
    }
    
    const char *start_ptr = &lex->source[lex->pos];
    advance_char(lex);
    switch (*start_ptr) {
        case '.': return make_token(lex, T_DOT,       start_ptr, 1);
        case '+': return make_token(lex, T_PLUS,      start_ptr, 1);
        case '-': return make_token(lex, T_MINUS,     start_ptr, 1);
        case '*': return make_token(lex, T_STAR,      start_ptr, 1);
        case '/': return make_token(lex, T_SLASH,     start_ptr, 1);
        case '^': return make_token(lex, T_CARET,     start_ptr, 1);
        case '(': return make_token(lex, T_LPAREN,    start_ptr, 1);
        case ')': return make_token(lex, T_RPAREN,    start_ptr, 1);
        case '{': return make_token(lex, T_LBRACE,    start_ptr, 1);
        case '}': return make_token(lex, T_RBRACE,    start_ptr, 1);
        case '[': return make_token(lex, T_LBRACKET,  start_ptr, 1);
        case ']': return make_token(lex, T_RBRACKET,  start_ptr, 1);
        case ',': return make_token(lex, T_COMMA,     start_ptr, 1);
        case ';': return make_token(lex, T_SEMICOLON, start_ptr, 1);
        case '?': return make_token(lex, T_QUESTION,  start_ptr, 1);
        case ':': return make_token(lex, T_COLON,     start_ptr, 1);

        case '!':
            if (lexer_peek(lex) == '=') { advance_char(lex); return make_token(lex, T_BANG_EQUAL, start_ptr, 2); }
            break;
        case '=':
            if (lexer_peek(lex) == '=') { advance_char(lex); return make_token(lex, T_EQUAL_EQUAL, start_ptr, 2); }
            return make_token(lex, T_EQUAL, start_ptr, 1);
        case '<':
            if (lexer_peek(lex) == '=') { advance_char(lex); return make_token(lex, T_LESS_EQUAL, start_ptr, 2); }
            return make_token(lex, T_LESS, start_ptr, 1);
        case '>':
            if (lexer_peek(lex) == '=') { advance_char(lex); return make_token(lex, T_GREATER_EQUAL, start_ptr, 2); }
            return make_token(lex, T_GREATER, start_ptr, 1);
        case '&':
            if (lexer_peek(lex) == '&') { advance_char(lex); return make_token(lex, T_AMP_AMP, start_ptr, 2); }
            break;
        case '|':
            if (lexer_peek(lex) == '|') { advance_char(lex); return make_token(lex, T_PIPE_PIPE, start_ptr, 2); }
            break;

        case '"':
            return scan_string(lex);

        default:
            break; /* Let fall through to error token */
    }
    /* Unknown byte: return an explicit error token so the
       parser can produce a diagnostic instead of silently
       stopping at (pretended) end-of-file. */
    return make_token(lex, T_ERROR, start_ptr, 1);
}
