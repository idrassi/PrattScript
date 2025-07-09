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

#include "pratt.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

/* Use configurable allocators */
#include "pratt_config.h"

/*── A read-only, always-available out-of-memory message ──────────────────*/
static const char static_oom_msg[] =
    "[Error] Out of memory: parser cannot continue.";

/*── Arena implementation with alignment, 1.5× growth and OOM tracking ──────────────*/
void arena_init(Arena *a, size_t initial) {
    a->mem = PRATT_MALLOC(initial);
    a->capacity = a->mem ? initial : 0;
    a->used = 0;
    a->oom = (a->mem == NULL && initial > 0);
}

static size_t align_up(size_t n, size_t align) {
    return (n + align - 1) & ~(align - 1);
}

// Portable max alignment type for C99
typedef union {
    long l;
    double d;
    void *p;
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
    long double ld;
#endif
} pratt_max_align_t;

void *arena_alloc(Arena *a, size_t sz) {
    if (a->oom) return NULL;
    size_t align = sizeof(pratt_max_align_t);
    /* align the next allocation boundary */
    a->used = align_up(a->used, align);
    /* round size up so the next allocation is aligned too */
    size_t sz_padded = align_up(sz, align);

    if (sz_padded > a->capacity - a->used) { /* check for overflow and capacity */
        if (sz_padded > SIZE_MAX - a->used) {a->oom = 1; return NULL; }
        size_t need = a->used + sz_padded;
        /* When capacity is 0 (e.g. OOM at init), grown starts at 0.
         * It will be updated to `need` later, which is correct. */
        size_t grown = a->capacity;
        if (grown > SIZE_MAX / 3) grown = SIZE_MAX; else grown = (grown * 3) / 2;
        if (grown < need) grown = need;

        char *m = PRATT_REALLOC(a->mem, grown);
        if (!m) {
            a->oom = 1;
            a->capacity = 0;
            PRATT_FREE(a->mem); /* Use PRATT_FREE instead of raw free() */
            a->mem = NULL;
            return NULL;
        }
        a->mem = m;
        a->capacity = grown;
        /*
         * A previous allocation attempt may have set `oom`.
         * Successful (re)allocation means we are healthy again.
         */
        a->oom = 0;
    }
    void *ptr = a->mem + a->used;
    a->used += sz_padded;
    return ptr;
}

void arena_free(Arena *a) {
    PRATT_FREE(a->mem);
    a->mem = NULL;
    a->capacity = a->used = 0;
    a->oom = 0;
}

/* Decide whether token `look` can extend the expression currently in `left` */
static int is_valid_continuation(Parser *p, PrattToken look, int min_bp) {
    /* Guard against negative and out-of-bounds token IDs */
    if (look.type < 0 || (size_t)look.type >= p->rule_count) return 0;
    const ParseRule *rule = &p->rules[look.type];
    return (rule->infix && rule->lbp > min_bp);
}

/*── Parser initialization ───────────────────────────────────────────────*/
void parser_init(Parser *p,
                 LexFn lex, void *lex_ctx,
                 void *user_ctx,
                 const ParseRule *rules,
                 size_t rule_count,
                 TokenNameFn token_name_fn,
                 Arena *arena)
{
    p->lex = lex;
    p->lex_ctx = lex_ctx;
    p->user_ctx = user_ctx; // Store the user context
    p->rules = rules;
    p->rule_count = rule_count;
    p->token_name_fn = token_name_fn;
    p->had_error = 0;
    p->last_error.message = NULL;
    p->last_error.token.type = T_EOF;
    p->recover_errors = 0;
    p->sync_tokens = NULL;
    p->sync_count = 0;
    p->recursion_depth = 0;
    p->max_recursion_depth = 1000; /* Sane default */
    p->arena = arena; // Just store the pointer.

    // Initialize 'cur' to a meaningless state. It holds the *previous*
    // token, and at the start, there is no previous token.
    p->cur.type = T_EOF; // Any non-error type will do.
    p->cur.start = NULL;
    p->cur.length = 0;
    p->cur.line = 0;
    p->cur.col = 0;
    
    // Prime the lookahead with the very first token from the source.
    p->next = lex(lex_ctx);
}

void parser_destroy(Parser *p) {
    // Does not free the arena, as the caller owns it.
    // We can null out fields for safety.
    p->arena = NULL;
    p->lex = NULL;
    p->lex_ctx = NULL;
}

/* Portable way to get formatted string length */
#if defined(_MSC_VER) && _MSC_VER < 1900
#define PORTABLE_VSCPRINTF(fmt, ap) _vscprintf(fmt, ap)
#else
#define PORTABLE_VSCPRINTF(fmt, ap) vsnprintf(NULL, 0, fmt, ap)
#endif

/*── Error helper uses static message on OOM ─────────────────────────────*/
void parser_error(Parser *p, const char *fmt, ...) {
    if (p->had_error && !p->recover_errors) return;
    p->had_error = 1;
    p->last_error.token = p->next;

    if (p->arena->oom) { p->last_error.message = static_oom_msg; return; }

    char prefix_buf[128];
    int prefix_len = snprintf(prefix_buf, sizeof(prefix_buf), "[line %d:%d] Error: ", p->next.line, p->next.col);
    if (prefix_len < 0) prefix_len = 0;

    va_list ap, ap_copy;
    va_start(ap, fmt);
    va_copy(ap_copy, ap);
    int body_len = PORTABLE_VSCPRINTF(fmt, ap);
    va_end(ap);

    if (body_len < 0) {
        p->last_error.message = "[Error] Failed to format error message.";
        va_end(ap_copy);
        return;
    }

    char *buf = arena_alloc(p->arena, prefix_len + body_len + 1);
    if (!buf) {
        p->arena->oom = 1;
        p->last_error.message = static_oom_msg;
        va_end(ap_copy);
        return;
    }
    memcpy(buf, prefix_buf, prefix_len);
    vsnprintf(buf + prefix_len, body_len + 1, fmt, ap_copy);
    va_end(ap_copy);
    p->last_error.message = buf;
}

/*── Error-Recovery Synchronization ───────────────────────────────────────*/
void parser_sync(Parser *p,
                 const PrattTokenType *sync_tokens,
                 size_t sync_count)
{
    /* If the token that *caused* the error is already a sync point,
       stay where we are – the caller will advance past it later. */
    for (size_t i = 0; i < sync_count; ++i)
        if (p->cur.type == sync_tokens[i])
            return;

    /* Otherwise, skip until we *encounter* a sync token,
       then consume exactly that one and stop. */
    while (p->next.type != T_EOF) {
        int is_sync = 0;
        for (size_t i = 0; i < sync_count; ++i)
            if (p->next.type == sync_tokens[i]) { is_sync = 1; break; }
        advance(p);
        if (is_sync) return;
    }
}

void parser_set_sync_tokens(Parser *p,
                            const PrattTokenType *sync_tokens,
                            size_t sync_count)
{
    p->sync_tokens = sync_tokens;
    p->sync_count  = sync_count;
}

/*── Lexer/PrattToken helpers ─────────────────────────────────────────────────*/

void parser_set_max_recursion(Parser *p, int depth) {
    p->max_recursion_depth = (depth > 0) ? depth : 1000;
}

PrattToken peek(Parser *p) { return p->next; }
PrattToken advance(Parser *p) {
    p->cur  = p->next;
    p->next = p->lex(p->lex_ctx);
    return p->cur;
}
int check(Parser *p, PrattTokenType t) { return p->next.type == t; }

int consume(Parser *p, PrattTokenType t, const char *desc) {
    if (check(p, t)) { advance(p); return 1; }
    parser_error(p, "Expected %s but got %s", desc, p->token_name_fn(p->next.type));
    return 0;
}


/*── Pratt's dispatch loop with explicit binding powers ───────────────────*/
ASTNode *parse_precedence(Parser *p, int min_bp) {
    if (++p->recursion_depth > p->max_recursion_depth) {
        parser_error(p, "Maximum recursion depth (%d) exceeded", p->max_recursion_depth);
        p->recursion_depth--;
        return NULL;
    }

    advance(p);
    PrattToken prefix_tok = p->cur;
    /* Guard against negative and out-of-bounds token IDs */
    if (prefix_tok.type < 0 || (size_t)prefix_tok.type >= p->rule_count) {
        parser_error(p,
                     "PrattToken ID %d has no rule defined (invalid token id)",
                     prefix_tok.type);
        p->recursion_depth--;
        return NULL;
    }

    PrefixFn pre = p->rules[prefix_tok.type].prefix;
    if (!pre) {
        parser_error(p, "No parse-rule for token %s (%.*s)", p->token_name_fn(prefix_tok.type), (int)prefix_tok.length, prefix_tok.start);
        p->recursion_depth--;
        return NULL;
    }
    ASTNode *left = pre(p);
    if (p->had_error || !left) { p->recursion_depth--; return NULL; }

    while (is_valid_continuation(p, peek(p), min_bp)) {
        const ParseRule *rule = &p->rules[peek(p).type];
        advance(p);
        left = rule->infix(p, left);
        if (p->had_error || !left) {
            p->recursion_depth--;
            return NULL;
        }
    }

    p->recursion_depth--;
    return left;
}

ASTNode *parse_expression_until(Parser           *p,
                                const PrattTokenType  *terminators,
                                size_t            term_count)
{
    ASTNode *expr = parse_precedence(p, PREC_NONE);
    if (p->had_error) {
        if (p->recover_errors) parser_sync(p, terminators, term_count);
        return NULL;
    }

    /* Require one of the caller-supplied terminators. */
    for (size_t i = 0; i < term_count; ++i)
        if (check(p, terminators[i])) { advance(p); return expr; }

    if (p->next.type == T_ERROR)
        parser_error(p, "Unexpected character '%.*s'",
                     (int)p->next.length, p->next.start);
    else
        parser_error(p, "Unexpected %s - expected statement terminator",
                     p->token_name_fn(p->next.type));
    if (p->recover_errors) parser_sync(p, terminators, term_count);
    return NULL;
}

ASTNode *parse_expression(Parser *p) {
    static const PrattTokenType eof = T_EOF;
    return parse_expression_until(p, &eof, 1);
}
