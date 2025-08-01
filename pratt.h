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

#ifndef PRATT_H
#define PRATT_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

/*── PrattToken definitions ────────────────────────────────────────────────────*/
/**
 * PrattTokenType is an opaque integer ID. The library provides well-known
 * sentinel values. Host languages should define their own token enums
 * starting from T_USER_BASE to avoid collisions.
 */
typedef int PrattTokenType;

enum {
    T_EOF   = 0,
    T_ERROR = 1,
    /* Host language token enums should start here */
    T_USER_BASE = 256
};

/*── A single lexed token ─────────────────────────────────────────────────*/
typedef struct {
    PrattTokenType    type;
    const char  *start;   /* pointer into source buffer */
    size_t       length;  /* lexeme length */
    int          line;    /* 1-based */
    int          col;     /* 1-based */
} PrattToken;

/*── Streaming lexer interface ─────────────────────────────────────────────*/
typedef PrattToken (*LexFn)(void *ctx);

/*── Forward declaration of Statement struct ────────────────────────────*/
typedef struct Statement Statement;

/*── AST node definitions ─────────────────────────────────────────────────*/
typedef struct ASTNode ASTNode;
typedef enum {
    AST_NUMBER, AST_STRING, AST_IDENT, AST_BINARY, AST_UNARY, AST_TERNARY, AST_ASSIGN,
    AST_CALL, AST_ARRAY, AST_OBJECT, AST_INDEX, AST_FUNCTION,
    AST_BOOL, AST_NIL, /* Special AST nodes for literals */
} ASTNodeType;

typedef struct { ASTNode *left, *right; PrattToken op;   } ASTBinary;
typedef struct { ASTNode *child;       PrattToken op;   } ASTUnary;

// Represents a number literal. Can be an int or a double.
typedef struct {
    bool is_double; // Flag to distinguish
    union {
        int64_t i_val;
        double  d_val;
    } as;
    PrattToken tok;
} ASTNumber;

typedef struct { char     *value; size_t length; PrattToken tok; } ASTString;
typedef struct { ASTNode *cond, *then_branch, *else_branch; } ASTTernary;
// name is a canonical, interned string pointer.
typedef struct { const char *name;       PrattToken tok;  } ASTIdent;
typedef struct { ASTNode *callee; ASTNode **args; size_t argc; PrattToken rparen; } ASTCall;
typedef struct { ASTNode **elements; size_t count; PrattToken bracket; } ASTArray;
typedef struct { const char** keys; ASTNode** values; size_t count; PrattToken brace; } ASTObject;
typedef struct { ASTNode *object; ASTNode *index; PrattToken bracket; } ASTIndex;
typedef struct {
    const char *name; // Optional, for named function expressions
    const char **params;
    PrattToken *param_toks;
    size_t param_count;
    Statement *body; // A ST_BLOCK
} ASTFunction;
typedef struct { ASTNode *target; ASTNode *value; PrattToken op; } ASTAssign;
typedef struct { int       value;      PrattToken tok;  } ASTBool;
typedef struct { PrattToken     tok;                    } ASTNil;


struct ASTNode {
    ASTNodeType type;
    union {
        ASTBinary binary;
        ASTUnary  unary;
        ASTTernary ternary;
        ASTAssign assign;
        ASTNumber number;
        ASTString string;
        ASTIdent  ident;
        ASTCall   call;
        ASTArray  array;
        ASTObject object;
        ASTIndex  index;
        ASTFunction function;
        ASTBool   boolean;
        ASTNil    nil;
    } as;
    void *user_data; /* For host language to attach extra info */
};

/*── Statement node definitions ───────────────────────────────────────────*/
typedef struct Statement Statement;
typedef enum { ST_EXPR, ST_VAR, ST_BLOCK, ST_BREAK, ST_CONTINUE,
               ST_IF,  ST_WHILE, ST_FOR, ST_RETURN, ST_FUNCTION } StatementType;

struct Statement {
    StatementType type;
    union {
        // Use interned 'name' for performance. Keep token for errors.
        struct { const char *name; PrattToken name_tok; ASTNode *initializer; } var;
        struct { ASTNode *expr; } expr;
        struct { ASTNode *condition; Statement *then_branch, *else_branch; } if_s;
        struct { PrattToken keyword; } break_s;
        struct { PrattToken keyword; } continue_s;
        struct { ASTNode *condition; Statement *body; } while_s;
        struct {
            Statement *initializer; /* A full statement (var decl or expr stmt) */
            ASTNode   *condition;
            ASTNode   *increment;
            Statement *body;
        } for_s;
        struct { Statement **list; size_t count; } block;
        struct { ASTNode *value; PrattToken keyword; } ret;
        // Function definition statement
        struct {
            const char *name;
            PrattToken name_tok;
            const char **params; // Interned parameter names
            PrattToken *param_toks;   // Original tokens for param names
            size_t param_count;
            Statement *body;     // Should be a ST_BLOCK
        } func;
    } as;
    void *user_data; /* For host language to attach extra info */
};

/*── Precedence levels ────────────────────────────────────────────────────*/
#define PREC_NONE         0
#define PREC_ASSIGNMENT  10  /* = */
#define PREC_CONDITIONAL 15  /* ?: */
#define PREC_LOGICAL_OR  16  /* || */
#define PREC_LOGICAL_AND 17  /* && */
#define PREC_BITWISE_OR  18  /* | */
#define PREC_BITWISE_XOR 19  /* ^ */
#define PREC_BITWISE_AND 20  /* & */
#define PREC_COMPARISON  21  /* == != < > <= >= */
#define PREC_SHIFT       22  /* << >> */
#define PREC_TERM        23  /* + - */
#define PREC_FACTOR      30  /* * / % */
#define PREC_EXPONENT    35  /* ** */
#define PREC_UNARY       40  /* - ! ~ */
#define PREC_CALL        60  /* () . [] */
#define PREC_PRIMARY     70

/*── ParseRule: ties a token to its parselets and explicit binding powers ─*/
typedef struct Parser Parser;
typedef ASTNode *(*PrefixFn) (Parser *p);
typedef ASTNode *(*InfixFn)  (Parser *p, ASTNode *left);

typedef struct {
    PrefixFn    prefix;  /* how to parse in prefix position (may be NULL) */
    InfixFn     infix;   /* how to parse in infix position (may be NULL) */
    int         lbp;     /* left-binding power */
    int         rbp;     /* right-binding power */
} ParseRule;

/** Boilerplate-reducing macro for building rule table entries. */
#define PRATT_RULE(p, i, l, r) \
    {.prefix=(p), .infix=(i), .lbp=(l), .rbp=(r)}

/*── Arena Allocator ──────────────────────────────────────────────────────*/
/**
 * A simple bump allocator. The caller is responsible for initializing it
 * with `arena_init()` and freeing its memory with `arena_free()`.
 */
typedef struct {
    char   *mem;       /* block */
    size_t  capacity;  /* bytes allocated */
    size_t  used;      /* bytes used */
    int     oom;       /* set if any alloc ever failed */
} Arena;

/*── Arena allocator functions ────────────────────────────────────────────*/
void      arena_init(Arena *a, size_t initial_size);
void     *arena_alloc(Arena *a, size_t sz);
void      arena_free(Arena *a);

/*── Host-provided function to get a string name for a token type ───────*/
typedef const char *(*TokenNameFn)(PrattTokenType t);

/*── Richer error reporting struct ────────────────────────────────────────*/
typedef struct {
    const char *message; /* Arena-allocated string */
    PrattToken       token;   /* PrattToken that caused the error */
} PrattError;

/*── Parser state ─────────────────────────────────────────────────────────*/
/**
 * --- THREAD SAFETY ---
 * The parser is RE-ENTRANT but not THREAD-SAFE.
 * Do NOT share a single Parser instance across multiple threads without
 * external locking. The typical use case is one Parser per thread.
 * A single thread can safely use multiple Parser instances.
 */
struct Parser {
    LexFn              lex;
    void              *lex_ctx;
    // Context pointer for host application (e.g., interpreter for string interning)
    void              *user_ctx;
    PrattToken              cur, next;
    const ParseRule   *rules;
    size_t             rule_count;
    TokenNameFn        token_name_fn;
    int                had_error;
    PrattError         last_error;
    int                recover_errors;
    const PrattTokenType   *sync_tokens;
    size_t             sync_count;
    int                recursion_depth;
    int                max_recursion_depth;
    Arena             *arena; /* Parser uses an arena, but does not OWN it. */
};

/*── Public API ───────────────────────────────────────────────────────────*/
/**
 * Initialize a parser to use a caller-owned arena.
 *
 * The parser will use the provided arena for all its allocations (AST nodes,
 * error messages). The caller is responsible for initializing the arena
 * with `arena_init()` before calling this function and for freeing it with
 * `arena_free()` after the parser and its generated AST are no longer needed.
 */
void      parser_init   (Parser          *p,
                         LexFn            lex,
                         void            *lex_ctx,
                         void            *user_ctx, // Pass user context
                         const ParseRule *rules,
                         size_t           rule_count,
                         TokenNameFn      token_name_fn,
                         Arena           *arena);
/**
 * Parse a single expression and **require** that the next token is T_EOF.
 *
 * !! Lifetime: Every AST node is allocated inside the arena provided to the
 *    parser during initialization. The caller is responsible for managing
 *    the arena's lifetime. Destroying the parser with `parser_destroy()` does
 *    NOT free the arena.
 */
ASTNode  *parse_expression(Parser *p);

/**
 * Parses a single statement.
 * This is a recursive-descent style function that forms the top-level
 * of the grammar, calling parse_expression() for its expression parts.
 *
 * !! Lifetime: The returned Statement node and all its children are allocated
 *    from the arena provided to the parser during initialization.
 */
Statement *parse_statement(Parser *p);

/**
 * Parses a block statement: '{' statement* '}'
 *
 * !! Lifetime: The returned Statement node and all its children are allocated
 *    from the arena provided to the parser during initialization.
 */
Statement *parse_block(Parser *p);

/**
 * Parse an expression that must be **terminated** by one of the tokens in
 * `terminators[]`.
 * - If a terminator is found, it is consumed and the AST is returned.
 * - On syntax error, behaviour mirrors `parse_expression()`.
 *
 * !! Lifetime: Every AST node is allocated inside the arena provided to the
 *    parser during initialization. The caller is responsible for managing
 *    the arena's lifetime.
 */
ASTNode  *parse_expression_until(Parser           *p,
                                 const PrattTokenType  *terminators,
                                 size_t            term_count);

/**
 * Parse expression with explicit lbp/rbp.
 * This is the core parsing function used internally.
 */
ASTNode  *parse_precedence(Parser *p, int min_bp);
/**
 * Free any parser-owned resources.
 * Note: This does NOT free the arena, as the caller owns it.
 */
void      parser_destroy(Parser *p);

/*── Error‐Recovery Helpers ────────────────────────────────────────────────*/
/**
 * After an error, skip tokens until one of the sync_tokens appears
 * (e.g. semicolon, EOF). Only useful if p->recover_errors == 1.
 */
void      parser_sync(Parser *p,
                      const PrattTokenType *sync_tokens,
                      size_t sync_count);

/*── Configure which tokens to sync on when recover_errors==1 */
void parser_set_sync_tokens(Parser *p,
                            const PrattTokenType *sync_tokens,
                                 size_t sync_count);

void      parser_set_max_recursion(Parser *p, int depth);

/*── Helpers ──────────────────────────────────────────────────────────────*/
void        parser_error(Parser *p, const char *fmt, ...);
PrattToken       peek(Parser *p);
PrattToken       advance(Parser *p);
int         check(Parser *p, PrattTokenType t);
int         consume(Parser *p, PrattTokenType t, const char *expect_desc);

#endif /* PRATT_H */
