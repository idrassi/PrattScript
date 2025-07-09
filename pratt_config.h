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

#ifndef PRATT_CONFIG_H
#define PRATT_CONFIG_H

#include <stdlib.h> // For malloc, realloc, free

/**
 * --------------------------------------------------------------------------
 *                            MEMORY ALLOCATION
 * --------------------------------------------------------------------------
 * To use a custom memory allocator, define these macros before including
 * pratt.h. Your custom functions must have the same signatures as the
 * standard library functions.
 *
 * Example:
 *   #define PRATT_MALLOC(sz)          my_game_malloc(sz)
 *   #define PRATT_REALLOC(ptr, sz)    my_game_realloc(ptr, sz)
 *   #define PRATT_FREE(ptr)           my_game_free(ptr)
 *   #include "pratt.h"
 */

#ifndef PRATT_MALLOC
#define PRATT_MALLOC(sz) malloc(sz)
#endif

#ifndef PRATT_REALLOC
#define PRATT_REALLOC(ptr, sz) realloc(ptr, sz)
#endif

#ifndef PRATT_FREE
#define PRATT_FREE(ptr) free(ptr)
#endif

#endif /* PRATT_CONFIG_H */
