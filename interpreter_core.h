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

#ifndef INTERPRETER_CORE_H
#define INTERPRETER_CORE_H

#include <stdint.h>
#include <stdbool.h>
#include "pratt_default.h"
#include "sfc64.h"

// All heap-allocated objects share this header.
typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_ARRAY,
    OBJ_OBJECT,
    OBJ_ENV,
    OBJ_RESOURCE
} ObjType;

typedef struct Obj {
    ObjType type;
    bool is_marked;
    struct Obj* next;
} Obj;

// A heap-allocated, GC-managed string.
typedef struct {
    Obj obj;
    size_t length;
    uint32_t hash;
    char chars[]; // Flexible array member
} ObjString;


// --- Generic External Resource ---
// The v-table defines the "class" of a resource.
typedef struct {
    // A unique, namespaced name for this resource type, e.g., "core.file".
    const char* type_name;
    // The function to call to release the resource.
    void (*finalize)(void* context);
} ResourceVTable;

// The generic resource object managed by the GC.
typedef struct {
    Obj obj;
    void* context;                  // Pointer to the actual C resource (e.g., FILE*)
    const ResourceVTable* vtable;   // The "class" implementation for this resource
    bool is_finalized;
} ObjResource;

// Forward declarations
typedef struct Function Function;
typedef struct Interpreter Interpreter;
typedef struct ObjArray ObjArray;
typedef struct ObjObject ObjObject;
typedef struct ObjEnv ObjEnv;

// Runtime values that expressions evaluate to.
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_INT,
    VAL_DOUBLE,
    VAL_BUILTIN,
    VAL_OBJ       // All heap types are now VAL_OBJ
} ValueType;

/*  Forward declaration for Value struct */
typedef struct Value Value;

// A C function callable from the language
typedef Value (*BuiltinFn)(Interpreter *interp, size_t argc, Value *args);

typedef struct Value {
    ValueType type;
    union {
        bool       boolean;
        int64_t    integer;
        double     number;
        BuiltinFn  builtin;
        Obj*       obj; // Points to a GC-managed object
    } as;
} Value;

// Helper macros for working with Values
#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_INT(value)     ((value).type == VAL_INT)
#define IS_DOUBLE(value)  ((value).type == VAL_DOUBLE)
#define IS_NUMERIC(value) (IS_INT(value) || IS_DOUBLE(value))
#define IS_NUMBER(value)  IS_NUMERIC(value)
#define IS_BUILTIN(value) ((value).type == VAL_BUILTIN)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

#define AS_BOOL(value)    ((value).as.boolean)
#define AS_INT(value)     ((value).as.integer)
#define AS_DOUBLE(value)  ((value).as.number)
// AS_NUMBER safely converts any numeric value to a double for mixed-type operations
#define AS_NUMBER(value)  (IS_INT(value) ? (double)AS_INT(value) : AS_DOUBLE(value))
#define AS_BUILTIN(value) ((value).as.builtin)
#define AS_OBJ(value)     ((value).as.obj)

// Macros to check the specific type of an Obj
#define OBJ_TYPE(value)   (AS_OBJ(value)->type)
#define IS_STRING(value)  (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_STRING)
#define IS_FUNCTION(value)(IS_OBJ(value) && OBJ_TYPE(value) == OBJ_FUNCTION)
#define IS_ARRAY(value)   (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_ARRAY)
#define IS_OBJECT(value)  (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_OBJECT)
#define IS_ENV(value)     (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_ENV)
#define IS_RESOURCE(value)(IS_OBJ(value) && OBJ_TYPE(value) == OBJ_RESOURCE)

// Macros to cast an Obj pointer to its specific type
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)   (AS_STRING(value)->chars)
#define AS_FUNCTION(value)  ((Function*)AS_OBJ(value))
#define AS_ARRAY(value)     ((ObjArray*)AS_OBJ(value))
#define AS_OBJECT(value)    ((ObjObject*)AS_OBJ(value))
#define AS_ENV(value)       ((ObjEnv*)AS_OBJ(value))
#define AS_RESOURCE(value)  ((ObjResource*)AS_OBJ(value))


// --- Hash Map for variables (uses pointer keys) ---
typedef struct {
    ObjString* key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Map;
// --- End Hash Map ---

// Runtime representation of an array
struct ObjArray {
    Obj obj;
    int count;
    int capacity;
    Value *values;
};

// Runtime representation of an object/hash-map
struct ObjObject {
    Obj obj;
    Map map;
};

/* GC-managed environment object */
struct ObjEnv {
    Obj obj;          // GC header
    struct ObjEnv *parent;
    Map table;
};

// Runtime representation of a user-defined function
struct Function {
    Obj obj;
    ObjString *name;
    size_t arity;
    Statement *body;       // The ST_BLOCK that is the function body
    ObjEnv *closure;    // The environment where the function was DEFINED
    ObjString *params[]; // Array of interned parameter names (ObjString*)
};

// Hash table for string interning
typedef struct {
    int count;
    int capacity;
    Entry* entries; // Re-using Entry struct is convenient here
} StringInterner;

typedef enum {
    EXEC_OK,      // Normal execution, continue
    EXEC_RETURN,  // A return statement was executed
    EXEC_BREAK,   // A break statement was executed
    EXEC_CONTINUE,// A continue statement was executed
    EXEC_ERROR,   // A runtime error occurred
} ExecStatus;

typedef struct {
    ExecStatus status;
    Value value;
} ExecResult;

// Interpreter state
struct Interpreter {
    Arena arena;
    ObjEnv *env; // Current environment
    StringInterner interner;

    Obj* objects; // Linked list of all heap objects
    size_t bytes_allocated;
    size_t next_gc;

    // Worklist for the GC's mark phase
    int gray_count;
    int gray_capacity;
    Obj** gray_stack;
    Obj** root_stack;
    int root_stack_count;
    int root_stack_capacity;

    // Error state
    int had_error;
    char error_message[256];

    // Random number generator context used in built-ins
    // SFC64 offers a fast, high-quality RNG suitable for games and simulations.
    // It is better than the standard rand() for most use cases.
    SFC64Context rng;
};

// Public API
ObjEnv* new_env_obj(Interpreter *interp, ObjEnv* parent);
void runtime_error(Interpreter *interp, const char *format, ...);
void interpreter_init(Interpreter *interp, size_t initial_arena_size);
void interpreter_destroy(Interpreter *interp);
ExecResult eval(Interpreter *interp, ASTNode *node);
ExecResult execute(Interpreter *interp, Statement *stmt);
ExecResult execute_block(Interpreter *interp, Statement **stmts, size_t count, ObjEnv *env);

void print_value(Value value);

ObjString* interpreter_intern_string(Interpreter* interp, const char* chars, size_t length);

// Helpers to create values
Value make_int(int64_t value);
Value make_double(double value);
Value make_bool(bool value);
Value make_nil(void);

#endif // INTERPRETER_CORE_H
