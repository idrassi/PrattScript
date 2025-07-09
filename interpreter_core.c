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

#include "interpreter_core.h"
#include "pratt_config.h"
#include "pratt.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdarg.h>
#include <inttypes.h>
#include <errno.h>

// Forward declare since they are mutually recursive
ExecResult eval(Interpreter *interp, ASTNode *node);
ExecResult execute(Interpreter *interp, Statement *stmt);
ExecResult execute_block(Interpreter *interp, Statement **stmts, size_t count, ObjEnv *env);
static ObjString* find_interned_string(StringInterner* interner, const char* chars, size_t length, uint32_t hash);
static void map_init(Map* map);


#define MAP_MAX_LOAD 0.75
#define GC_HEAP_GROW_FACTOR 2

// Helper macros for creating ExecResult structs
#define OK_RESULT(v) ((ExecResult){EXEC_OK, (v)})
#define RETURN_RESULT(v) ((ExecResult){EXEC_RETURN, (v)})
#define BREAK_RESULT()   ((ExecResult){EXEC_BREAK,  make_nil()})
#define CONTINUE_RESULT() ((ExecResult){EXEC_CONTINUE, make_nil()})
#define ERROR_RESULT()   ((ExecResult){EXEC_ERROR,  make_nil()})

// --- GC and Memory Management ---
static void* reallocate(Interpreter* interp, void* pointer, size_t old_size, size_t new_size);
void collect_garbage(Interpreter* interp);
static void mark_value(Interpreter* interp, Value value);
static void mark_object(Interpreter* interp, Obj* object);
void runtime_error(Interpreter *interp, const char *format, ...);


static void* reallocate(Interpreter* interp, void* pointer, size_t old_size, size_t new_size) {
    interp->bytes_allocated += new_size - old_size;

    // GC is triggered *before* allocation when memory is increasing.
    // This gives it a chance to free up space to fulfill the request.
    if (new_size > old_size) {
#ifdef DEBUG_STRESS_GC
        collect_garbage(interp);
#endif
        if (interp->bytes_allocated > interp->next_gc) {
            collect_garbage(interp);
        }
    }

    if (new_size == 0) {
        PRATT_FREE(pointer);
        return NULL;
    }
    void* result = PRATT_REALLOC(pointer, new_size);
    if (result == NULL) {
        // In a real-world scenario, we might try to GC one more time before failing.
        // For now, exiting is a reasonable strategy for OOM.
        fprintf(stderr, "Fatal: out of memory.\n");
        exit(1);
    }
    return result;
}

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(interp, pointer, type, oldCount, newCount) \
    (type*)reallocate(interp, pointer, sizeof(type) * (oldCount), \
                      sizeof(type) * (newCount))

#define FREE_ARRAY(interp, type, pointer, oldCount) \
    reallocate(interp, pointer, sizeof(type) * (oldCount), 0)

static void free_object(Interpreter* interp, Obj* object) {
    // Free the internal data structures of the object before freeing the object itself.
    switch (object->type) {
        case OBJ_STRING: {
            // Strings have their chars in the same allocation block.
            ObjString* string = (ObjString*)object;
            reallocate(interp, object, sizeof(ObjString) + string->length + 1, 0);
            break;
        }
        case OBJ_FUNCTION: {
            Function* function = (Function*)object;
            reallocate(interp, object, sizeof(Function) + sizeof(ObjString*) * function->arity, 0);
            break;
        }
        case OBJ_ARRAY: {
            ObjArray* array = (ObjArray*)object;
            FREE_ARRAY(interp, Value, array->values, array->capacity);
            reallocate(interp, object, sizeof(ObjArray), 0);
            break;
        }
        case OBJ_OBJECT: {
            ObjObject* obj = (ObjObject*)object;
            FREE_ARRAY(interp, Entry, obj->map.entries, obj->map.capacity);
            reallocate(interp, object, sizeof(ObjObject), 0);
            break;
        }
        case OBJ_ENV: {
            // Free the environment's hash table.
            ObjEnv* env = (ObjEnv*)object;
            FREE_ARRAY(interp, Entry, env->table.entries, env->table.capacity);

            /* clear the map */
            env->table.entries  = NULL;
            env->table.count    = 0;
            env->table.capacity = 0;

            reallocate(interp, object, sizeof(ObjEnv), 0);
            break;
        }
    }
}


static Obj* allocate_object(Interpreter* interp, size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(interp, NULL, 0, size);
    object->type = type;
    object->is_marked = false;

    // Insert new object at the head of the list for easy access.
    object->next = interp->objects;
    interp->objects = object;

    return object;
}

// --- Object Constructors ---
static ObjArray* new_array(Interpreter* interp) {
    ObjArray* array = (ObjArray*)allocate_object(interp, sizeof(ObjArray), OBJ_ARRAY);
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
    return array;
}

static ObjObject* new_object(Interpreter* interp) {
    ObjObject* object = (ObjObject*)allocate_object(interp, sizeof(ObjObject), OBJ_OBJECT);
    map_init(&object->map);
    return object;
}

static Function* new_function(Interpreter* interp, size_t param_count) {
    // Allocate enough space for the function object and its parameters.
    size_t func_size = sizeof(Function) + sizeof(ObjString*) * param_count;
    // The size includes space for the flexible array member for parameters.
    Function* function = (Function*)allocate_object(interp, func_size, OBJ_FUNCTION);
    function->arity = 0;
    function->name = NULL;
    function->body = NULL;
    function->closure = NULL;
    return function;
}

static ObjString* allocate_string_obj(Interpreter* interp, const char* chars, size_t length, uint32_t hash) {
    ObjString* string = (ObjString*)allocate_object(interp, sizeof(ObjString) + length + 1, OBJ_STRING);
    string->length = length;
    string->hash = hash;
    memcpy(string->chars, chars, length);
    string->chars[length] = '\0';
    return string;
}

static uint32_t hash_string(const char* key, size_t length) {
    uint32_t hash = 2166136261u;
    for (size_t i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

// Create a new heap-allocated string. This is for results of operations like concatenation.
// These strings are NOT interned by default.
static ObjString* make_heap_string(Interpreter* interp, const char* chars, size_t length) {
    uint32_t hash = hash_string(chars, length);
    // Don't look up in interner, just allocate.
    return allocate_string_obj(interp, chars, length, hash);
}

// --- Value Creation Helpers ---
Value make_int(int64_t value) { return (Value){VAL_INT, .as.integer = value}; }
Value make_double(double value) { return (Value){VAL_DOUBLE, .as.number = value}; }
Value make_bool(bool value) { return (Value){VAL_BOOL, .as.boolean = value}; }
Value make_nil(void) { return (Value){VAL_NIL}; }
Value make_builtin(BuiltinFn fn) { return (Value){VAL_BUILTIN, .as.builtin = fn}; }
Value make_obj(Obj* object) { return (Value){VAL_OBJ, .as.obj = object}; }

// --- DYNAMIC ARRAY (for ObjArray) ---
static void array_write(Interpreter* interp, ObjArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(old_capacity);
        array->values = GROW_ARRAY(interp, array->values, Value, old_capacity, array->capacity);
    }
    array->values[array->count] = value;
    array->count++;
}

// --- HASH MAP IMPLEMENTATION ---
static void map_init(Map* map) {
    map->count = 0;
    map->capacity = 0;
    map->entries = NULL;
}

static Entry* find_entry(Entry* entries, int capacity, ObjString* key) {
    if (capacity == 0) return NULL;
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;

    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) { // Empty bucket.
                return tombstone != NULL ? tombstone : entry;
            } else { // Found a tombstone.
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) { // Key is found by pointer equality due to interning.
            return entry;
        }
        index = (index + 1) % capacity;
    }
}

static void map_adjust_capacity(Interpreter* interp, Map* map, int capacity) {
    Entry* entries = GROW_ARRAY(interp, NULL, Entry, 0, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = make_nil();
    }

    map->count = 0;
    for (int i = 0; i < map->capacity; i++) {
        Entry* entry = &map->entries[i];
        if (entry->key == NULL) continue;
        Entry* dest = find_entry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        map->count++;
    }

    FREE_ARRAY(interp, Entry, map->entries, map->capacity);
    map->entries = entries;
    map->capacity = capacity;
}

static bool map_set(Interpreter* interp, Map* map, ObjString* key, Value value) {
    if (map->count + 1 > map->capacity * MAP_MAX_LOAD) {
        int capacity = GROW_CAPACITY(map->capacity);
        map_adjust_capacity(interp, map, capacity);
    }

    Entry* entry = find_entry(map->entries, map->capacity, key);
    if (!entry) return false;
    
    bool is_new_key = entry->key == NULL;
    if (is_new_key && IS_NIL(entry->value)) map->count++;

    entry->key = key;
    entry->value = value;
    return is_new_key;
}

static bool map_get(Map* map, ObjString* key, Value* value) {
    if (map->count == 0) return false;
    Entry* entry = find_entry(map->entries, map->capacity, key);
    if (!entry || entry->key == NULL) return false;
    if (value) *value = entry->value;
    return true;
}

static void map_remove_white(Interpreter* interp, Map* map) {
    for (int i = 0; i < map->capacity; i++) {
        Entry* entry = &map->entries[i];
        if (entry->key != NULL && !entry->key->obj.is_marked) {
            // This is essentially weak-key behavior for the string table.
            // An un-marked key means the string is no longer reachable anywhere else.
            entry->key = NULL;
            entry->value = make_bool(true); // Tombstone
            map->count--;
        }
    }
}

// --- STRING INTERNING ---
void interner_init(StringInterner* interner) {
    map_init((Map*)interner);
}

ObjString* find_interned_string(StringInterner* interner, const char* chars, size_t length, uint32_t hash) {
    if (interner->count == 0) return NULL;
    Map* map = (Map*)interner;
    uint32_t index = hash % map->capacity;

    for (;;) {
        Entry* entry = &map->entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) return NULL; // Empty bucket, not found.
        } else if (entry->key->length == length &&
                   entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0) {
            return entry->key; // Found it.
        }
        index = (index + 1) % map->capacity;
    }
}

ObjString* interpreter_intern_string(Interpreter* interp, const char* chars, size_t length) {
    uint32_t hash = hash_string(chars, length);
    ObjString* interned = find_interned_string(&interp->interner, chars, length, hash);
    if (interned != NULL) return interned;

    ObjString* string = allocate_string_obj(interp, chars, length, hash);

    // After creating the new string, add it to the interner's table.
    // The value stored is irrelevant, but it must not be a collectable object.
    map_set(interp, (Map*)&interp->interner, string, make_nil());

    return string;
}

// --- GARBAGE COLLECTOR ---

void mark_object(Interpreter* interp, Obj* object) {
    if (object == NULL || object->is_marked) return;
    object->is_marked = true;

    if (interp->gray_capacity < interp->gray_count + 1) {
        interp->gray_capacity = GROW_CAPACITY(interp->gray_capacity);
        interp->gray_stack = (Obj**)PRATT_REALLOC(interp->gray_stack, sizeof(Obj*) * interp->gray_capacity);
        if (interp->gray_stack == NULL) exit(1);
    }
    interp->gray_stack[interp->gray_count++] = object;
}

void mark_value(Interpreter* interp, Value value) {
    if (IS_OBJ(value)) {
        mark_object(interp, AS_OBJ(value));
    }
}

static void mark_map(Interpreter* interp, Map* map) {
    for (int i = 0; i < map->capacity; i++) {
        Entry* entry = &map->entries[i];
        mark_object(interp, (Obj*)entry->key);
        mark_value(interp, entry->value);
    }
}

static void push_root(Interpreter* interp, Obj* obj) {
    if (obj == NULL) return; // No need to root NULL
    if (interp->root_stack_count + 1 > interp->root_stack_capacity) {
        int old_capacity = interp->root_stack_capacity;
        interp->root_stack_capacity = GROW_CAPACITY(old_capacity);
        // This allocation is for the GC's own bookkeeping and should not be tracked
        // by the GC itself (to avoid recursive collection triggers), so we use 
        // PRATT_REALLOC directly instead of the interpreter's reallocate().
        interp->root_stack = (Obj**)PRATT_REALLOC(interp->root_stack, sizeof(Obj*) * interp->root_stack_capacity);
        if (interp->root_stack == NULL) {
            // This is a catastrophic failure. Cannot protect objects from GC.
            fprintf(stderr, "Fatal: out of memory growing GC root stack.\n");
            exit(1);
        }
    }
    interp->root_stack[interp->root_stack_count++] = obj;
}

static void pop_root(Interpreter* interp) {
    // Popping is simple, just decrement the count.
    // No need to shrink the array, as the capacity can be reused.
    interp->root_stack_count--;
}

static void mark_roots(Interpreter* interp) {
    // Mark the current environment object itself as a root.
    // The tracer will then handle its parent and contents.
    mark_object(interp, (Obj*)interp->env);

    for (int i = 0; i < interp->root_stack_count; i++) {
        mark_object(interp, interp->root_stack[i]);
    }
    // The string interner table itself is a root source.
    // We must mark all strings within it to prevent them from being collected
    // if they are only referenced by the table itself.
    mark_map(interp, (Map*)&interp->interner);
}

static void blacken_object(Interpreter* interp, Obj* object) {
    // Trace all outgoing references from the given object.
    switch (object->type) {
        case OBJ_FUNCTION: {
            Function* function = (Function*)object;
            mark_object(interp, (Obj*)function->name);
            for (size_t i = 0; i < function->arity; i++) {
                mark_object(interp, (Obj*)function->params[i]);
            }
            // A function keeps its closure environment alive. Mark it directly.
            // The tracer will handle the environment's parent chain and contents.
            mark_object(interp, (Obj*)function->closure);
            break;
        }
        case OBJ_ARRAY: {
            ObjArray* array = (ObjArray*)object;
            for (int i = 0; i < array->count; i++) {
                mark_value(interp, array->values[i]);
            }
            break;
        }
        case OBJ_OBJECT: {
            ObjObject* obj = (ObjObject*)object;
            mark_map(interp, &obj->map);
            break;
        }
        case OBJ_STRING: // No fields to trace.
            break; 
        case OBJ_ENV: {
            // Trace an environment's references.
            ObjEnv* env = (ObjEnv*)object;
            mark_object(interp, (Obj*)env->parent); // Mark the parent environment.
            mark_map(interp, &env->table);          // Mark everything in its variable table.
            break;
        }
    }
}

static void trace_references(Interpreter* interp) {
    while (interp->gray_count > 0) {
        Obj* object = interp->gray_stack[--interp->gray_count];
        blacken_object(interp, object);
    }
}

static void sweep(Interpreter* interp) {
    Obj* previous = NULL;
    Obj* object = interp->objects;
    while (object != NULL) {
        if (object->is_marked) {
            object->is_marked = false; // Unmark for next cycle.
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                interp->objects = object;
            }
            free_object(interp, unreached);
        }
    }
}

void collect_garbage(Interpreter* interp) {
    mark_roots(interp);
    trace_references(interp);
    map_remove_white(interp, (Map*)&interp->interner); // Clean weak refs from string table.
    sweep(interp);
    
    interp->next_gc = interp->bytes_allocated * GC_HEAP_GROW_FACTOR;
}


// --- ENVIRONMENT ---

// Environments are now GC-managed objects, allocated on the heap.
ObjEnv* new_env_obj(Interpreter *interp, ObjEnv* parent) {
    ObjEnv* env = (ObjEnv*)allocate_object(interp, sizeof(ObjEnv), OBJ_ENV);
    env->parent = parent;
    map_init(&env->table);
    return env;
}

static void env_define(Interpreter* interp, ObjEnv* env, ObjString* name, Value value) {
    map_set(interp, &env->table, name, value);
}

static bool env_get(ObjEnv* env, ObjString* name, Value* value) {
    if (map_get(&env->table, name, value)) {
        return true;
    }
    if (env->parent != NULL) {
        return env_get(env->parent, name, value);
    }
    return false;
}

static bool env_assign(Interpreter* interp, ObjEnv* env, ObjString* name, Value value) {
    if (map_get(&env->table, name, NULL)) {
        map_set(interp, &env->table, name, value);
        return true;
    }
    if (env->parent != NULL) {
        return env_assign(interp, env->parent, name, value);
    }
    return false;
}

// --- VALUE HELPERS ---
static int is_truthy(Value v) {
    switch (v.type) {
        case VAL_NIL:    return 0;
        case VAL_BOOL:   return v.as.boolean;
        case VAL_INT:    return v.as.integer != 0;
        case VAL_DOUBLE: return v.as.number != 0;
        case VAL_OBJ:    return 1; // All objects are truthy.
        case VAL_BUILTIN:return 1;
    }
    return 0;
}

static bool values_are_equal(Value a, Value b) {
    if (a.type != b.type) {
        // Allow numeric types to be compared.
        if (IS_NUMERIC(a) && IS_NUMERIC(b)) {
            return AS_NUMBER(a) == AS_NUMBER(b);
        }
        return false;
    }

    switch (a.type) {
        case VAL_BOOL:   return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:    return true;
        case VAL_INT:    return AS_INT(a) == AS_INT(b);
        case VAL_DOUBLE: return AS_DOUBLE(a) == AS_DOUBLE(b);
        case VAL_OBJ: {
            // For strings, compare content. For all other objects, compare pointers.
            if (OBJ_TYPE(a) == OBJ_STRING && OBJ_TYPE(b) == OBJ_STRING) {
                ObjString* a_str = AS_STRING(a);
                ObjString* b_str = AS_STRING(b);
                return (a_str->length == b_str->length &&
                        memcmp(a_str->chars, b_str->chars, a_str->length) == 0);
            }
            return AS_OBJ(a) == AS_OBJ(b);
        }
        case VAL_BUILTIN: return AS_BUILTIN(a) == AS_BUILTIN(b);
        default: return false; // Unreachable
    }
}

// Helper to convert any value to a string representation.
// Returns a new heap-allocated ObjString, unless the value is already a string,
// in which case the original is returned.
static ObjString* value_to_string(Interpreter* interp, Value value) {
    if (IS_STRING(value)) {
        return AS_STRING(value);
    }

    char* buffer = NULL;
    int length = 0;

    switch (value.type) {
        case VAL_BOOL: {
            const char* str = AS_BOOL(value) ? "true" : "false";
            length = (int)strlen(str);
            buffer = PRATT_MALLOC(length + 1);
            if (!buffer) goto oom;
            memcpy(buffer, str, length + 1);
            break;
        }
        case VAL_NIL: {
            const char* str = "nil";
            length = 3;
            buffer = PRATT_MALLOC(4);
            if (!buffer) goto oom;
            memcpy(buffer, str, 4);
            break;
        }
        case VAL_INT: {
            // First, determine the required length
            int needed = snprintf(NULL, 0, "%" PRId64, AS_INT(value));
            if (needed < 0) goto oom;
            length = needed;
            buffer = PRATT_MALLOC(length + 1);
            if (!buffer) goto oom;
            snprintf(buffer, length + 1, "%" PRId64, AS_INT(value));
            break;
        }
        case VAL_DOUBLE: {
            // First, determine the required length
            int needed = snprintf(NULL, 0, "%g", AS_DOUBLE(value));
            if (needed < 0) goto oom;
            length = needed;
            buffer = PRATT_MALLOC(length + 1);
            if (!buffer) goto oom;
            snprintf(buffer, length + 1, "%g", AS_DOUBLE(value));
            break;
        }
        case VAL_BUILTIN: {
            const char* str = "<builtin>";
            length = (int)strlen(str);
            buffer = PRATT_MALLOC(length + 1);
            if (!buffer) goto oom;
            memcpy(buffer, str, length + 1);
            break;
        }
        case VAL_OBJ: {
            Obj* obj = AS_OBJ(value);
            switch (obj->type) {
                case OBJ_STRING:
                    return AS_STRING(value);
                case OBJ_FUNCTION: {
                    Function* fn = AS_FUNCTION(value);
                    const char* prefix = "<fn ";
                    const char* name = fn->name ? fn->name->chars : "anonymous";
                    const char* suffix = ">";
                    length = (int)(strlen(prefix) + strlen(name) + strlen(suffix));
                    buffer = PRATT_MALLOC(length + 1);
                    if (!buffer) goto oom;
                    snprintf(buffer, length + 1, "%s%s%s", prefix, name, suffix);
                    break;
                }
                case OBJ_ARRAY: {
                    const char* str = "[object Array]";
                    length = (int)strlen(str);
                    buffer = PRATT_MALLOC(length + 1);
                    if (!buffer) goto oom;
                    memcpy(buffer, str, length + 1);
                    break;
                }
                case OBJ_OBJECT: {
                    const char* str = "[object Object]";
                    length = (int)strlen(str);
                    buffer = PRATT_MALLOC(length + 1);
                    if (!buffer) goto oom;
                    memcpy(buffer, str, length + 1);
                    break;
                }
                case OBJ_ENV: {
                    const char* str = "[environment]";
                    length = (int)strlen(str);
                    buffer = PRATT_MALLOC(length + 1);
                    if (!buffer) goto oom;
                    memcpy(buffer, str, length + 1);
                    break;
                }
            }
            break;
        }
        default:
            goto oom;
    }

    ObjString* result = make_heap_string(interp, buffer, length);
    PRATT_FREE(buffer);
    return result;

oom:
    runtime_error(interp, "Out of memory in value_to_string.");
    return make_heap_string(interp, "<oom>", 5);
}


void runtime_error(Interpreter *interp, const char *format, ...) {
    interp->had_error = 1;
    va_list args;
    va_start(args, format);
    vsnprintf(interp->error_message, sizeof(interp->error_message), format, args);
    va_end(args);
}

// --- Standard Built-in Functions ---
typedef struct { const char* name; BuiltinFn fn; } BuiltinDef;

// Forward declare all built-in functions
static Value builtin_print(Interpreter*, size_t, Value*);
static Value builtin_println(Interpreter*, size_t, Value*);
static Value builtin_sqrt(Interpreter*, size_t, Value*);
static Value builtin_upper(Interpreter*, size_t, Value*);
static Value builtin_lower(Interpreter*, size_t, Value*);
static Value builtin_compare(Interpreter*, size_t, Value*);
static Value builtin_len(Interpreter*, size_t, Value*);
static Value builtin_push(Interpreter*, size_t, Value*);
static Value builtin_pop(Interpreter*, size_t, Value*);
static Value builtin_keys(Interpreter*, size_t, Value*);
static Value builtin_gc_collect(Interpreter*, size_t, Value*);
static Value builtin_gc_allocated(Interpreter*, size_t, Value*);
static Value builtin_gc_next_gc(Interpreter*, size_t, Value*);
static Value builtin_toString(Interpreter*, size_t, Value*);
static Value builtin_toNumber(Interpreter*, size_t, Value*);

static BuiltinDef builtins[] = {
    {"print",   builtin_print},
    {"println", builtin_println},
    {"sqrt",    builtin_sqrt},
    {"upper",   builtin_upper},
    {"lower",   builtin_lower},
    {"compare", builtin_compare},
    {"len",     builtin_len},
    {"push",    builtin_push},
    {"pop",     builtin_pop},
    {"keys",    builtin_keys},
    {"toString", builtin_toString},
    {"toNumber", builtin_toNumber},
    {NULL, NULL}
};

// --- INTERPRETER LIFECYCLE ---
void interpreter_init(Interpreter* interp, size_t initial_arena_size) {
    interp->had_error = 0;
    interp->error_message[0] = '\0';
    
    interp->objects = NULL;
    interp->bytes_allocated = 0;
    interp->next_gc = 1024 * 1024;
    interp->gray_count = 0;
    interp->gray_capacity = 0;
    interp->gray_stack = NULL;
    interp->root_stack = NULL;
    interp->root_stack_count = 0;
    interp->root_stack_capacity = 0;

    arena_init(&interp->arena, initial_arena_size);
    interner_init(&interp->interner);

    // Create the global environment. It's an object now.
    interp->env = new_env_obj(interp, NULL);
    if (interp->env == NULL) { // Check for allocation failure
        runtime_error(interp, "Out of memory initializing interpreter.");
        return;
    }

    for (int i = 0; builtins[i].name != NULL; i++) {
        ObjString* name = interpreter_intern_string(interp, builtins[i].name, strlen(builtins[i].name));
        env_define(interp, interp->env, name, make_builtin(builtins[i].fn));
    }

    // --- GC Built-in Object ---
    ObjObject* gc_object = new_object(interp);
    push_root(interp, (Obj*)gc_object); // Protect from GC during setup

    // Intern method names
    ObjString* collect_name = interpreter_intern_string(interp, "collect", 7);
    ObjString* allocated_name = interpreter_intern_string(interp, "allocated", 9);
    ObjString* next_gc_name = interpreter_intern_string(interp, "next_gc", 7);

    // Add methods to the 'gc' object
    map_set(interp, &gc_object->map, collect_name, make_builtin(builtin_gc_collect));
    map_set(interp, &gc_object->map, allocated_name, make_builtin(builtin_gc_allocated));
    map_set(interp, &gc_object->map, next_gc_name, make_builtin(builtin_gc_next_gc));

    // Define 'gc' in the global scope
    ObjString* gc_name = interpreter_intern_string(interp, "gc", 2);
    env_define(interp, interp->env, gc_name, make_obj((Obj*)gc_object));

    pop_root(interp); // Un-root the gc object; it's now rooted by the global env.
}

void interpreter_destroy(Interpreter *interp) {
    Obj* object = interp->objects;
    while (object != NULL) {
        Obj* next = object->next;
        free_object(interp, object);
        object = next;
    }
    
    // Free the GC's worklist and other heap allocations.
    FREE_ARRAY(interp, Entry, interp->interner.entries, interp->interner.capacity);
    PRATT_FREE(interp->gray_stack);
    PRATT_FREE(interp->root_stack);
    arena_free(&interp->arena);
}

// --- FUNCTION CALL LOGIC ---
static ExecResult call_value(Interpreter* interp, Value callee_val, size_t argc, Value* args) {
    if (IS_OBJ(callee_val)) {
        if (OBJ_TYPE(callee_val) == OBJ_FUNCTION) {
            Function* function = AS_FUNCTION(callee_val);
            if (argc != function->arity) {
                runtime_error(interp, "Expected %zu arguments but got %zu for function '%s'.", function->arity, argc, function->name->chars);
                return ERROR_RESULT();
            }

            ObjEnv* frame = new_env_obj(interp, function->closure);
            for (size_t i = 0; i < function->arity; i++) {
                env_define(interp, frame, function->params[i], args[i]);
            }
            
            ExecResult result = execute_block(interp, function->body->as.block.list, function->body->as.block.count, frame);
            
            if (result.status == EXEC_RETURN) return OK_RESULT(result.value);
            if (result.status == EXEC_OK) return OK_RESULT(make_nil()); // Implicit return nil.
            return result; // Propagate error, break, continue
        }
    } else if (IS_BUILTIN(callee_val)) {
        Value builtin_result = AS_BUILTIN(callee_val)(interp, argc, args);
        if (interp->had_error) return ERROR_RESULT();
        return OK_RESULT(builtin_result);
    }

    runtime_error(interp, "Can only call functions and built-ins.");
    return ERROR_RESULT();
}

// --- THE EVALUATOR ---
ExecResult eval(Interpreter *interp, ASTNode *node) {
    if (interp->had_error) return ERROR_RESULT();
    
    switch (node->type) {
        case AST_NUMBER:
            if (node->as.number.is_double) {
                return OK_RESULT(make_double(node->as.number.as.d_val));
            }
            return OK_RESULT(make_int(node->as.number.as.i_val));

        case AST_STRING: {
            // Strings from source are interned.
            ObjString* str = interpreter_intern_string(interp, node->as.string.value, node->as.string.length);
            return OK_RESULT(make_obj((Obj*)str));
        }
        case AST_BOOL:   return OK_RESULT(make_bool(node->as.boolean.value));
        case AST_NIL:    return OK_RESULT(make_nil());
        case AST_IDENT: {
            // Must intern the identifier from the AST to get the correct ObjString* for lookup.
            ObjString* name = interpreter_intern_string(interp, node->as.ident.name, strlen(node->as.ident.name));
            Value value;
            if (env_get(interp->env, name, &value)) {
                return OK_RESULT(value);
            }
            runtime_error(interp, "Undefined variable '%s'.", node->as.ident.name);
            return ERROR_RESULT();
        }
        case AST_ARRAY: {
            ObjArray *array = new_array(interp);
            push_root(interp, (Obj*)array);

            for (size_t i = 0; i < node->as.array.count; i++) {
                ExecResult elem_res = eval(interp, node->as.array.elements[i]);
                if (elem_res.status != EXEC_OK) {
                    pop_root(interp);
                    return elem_res;
                }
                array_write(interp, array, elem_res.value);
            }

            pop_root(interp);
            return OK_RESULT(make_obj((Obj*)array));
        }
        case AST_OBJECT: {
            ObjObject *object = new_object(interp);
            push_root(interp, (Obj*)object);

            for (size_t i = 0; i < node->as.object.count; i++) {
                ObjString* key = interpreter_intern_string(interp, node->as.object.keys[i], strlen(node->as.object.keys[i]));
                push_root(interp, (Obj*)key);
                
                ExecResult val_res = eval(interp, node->as.object.values[i]);
                pop_root(interp); // Pop key after value is evaluated.

                if (val_res.status != EXEC_OK) {
                    pop_root(interp);
                    return val_res;
                }
                map_set(interp, &object->map, key, val_res.value);
            }

            pop_root(interp);
            return OK_RESULT(make_obj((Obj*)object));
        }
        case AST_FUNCTION: {
            // This handles function *expressions* (e.g., var f = function() {})
            // The logic is very similar to ST_FUNCTION in execute().
            ASTFunction* fn_node = &node->as.function;
           Function* func = new_function(interp, fn_node->param_count);
            
            // Name can be NULL for anonymous functions
            if (fn_node->name) {
                func->name = interpreter_intern_string(interp, fn_node->name, strlen(fn_node->name));
            } else {
               func->name = interpreter_intern_string(interp, "anonymous", 9);
            }

            func->arity = fn_node->param_count;
           func->body = fn_node->body;
            func->closure = interp->env;

            for(size_t i = 0; i < func->arity; i++) {
               func->params[i] = interpreter_intern_string(interp, fn_node->params[i], strlen(fn_node->params[i]));
            }
            return OK_RESULT(make_obj((Obj*)func));
        }
        case AST_ASSIGN: {
            // Evaluate the right-hand side first.
            ExecResult value_res = eval(interp, node->as.assign.value);
            if (value_res.status != EXEC_OK) return value_res;

            ASTNode *target = node->as.assign.target;

            if (target->type == AST_IDENT) {
                ObjString* name = interpreter_intern_string(interp, target->as.ident.name, strlen(target->as.ident.name));
                if (!env_assign(interp, interp->env, name, value_res.value)) {
                    runtime_error(interp, "Undefined variable '%s'.", target->as.ident.name);
                    return ERROR_RESULT();
                }
            } else if (target->type == AST_INDEX) {
                // GC-Robustness: protect value during collection/index evaluation
                if (IS_OBJ(value_res.value)) push_root(interp, AS_OBJ(value_res.value));

                ExecResult col_res = eval(interp, target->as.index.object);
                if (col_res.status != EXEC_OK) { if (IS_OBJ(value_res.value)) pop_root(interp); return col_res; }

                // GC-Robustness: protect collection during index evaluation
                if (IS_OBJ(col_res.value)) push_root(interp, AS_OBJ(col_res.value));
                ExecResult idx_res = eval(interp, target->as.index.index);
                if (IS_OBJ(col_res.value)) pop_root(interp);

                if (idx_res.status != EXEC_OK) { if (IS_OBJ(value_res.value)) pop_root(interp); return idx_res; }
                if (IS_ARRAY(col_res.value)) {
                     ObjArray* arr = AS_ARRAY(col_res.value);
                     int64_t idx;
                     if (IS_INT(idx_res.value)) { idx = AS_INT(idx_res.value); }
                     else { runtime_error(interp, "Array index must be an integer for assignment."); return ERROR_RESULT(); }

                     if (idx < 0 || (size_t)idx >= arr->count) { runtime_error(interp, "Array index out of bounds for assignment."); return ERROR_RESULT(); }
                    arr->values[(size_t)idx] = value_res.value;
                } else if (IS_OBJECT(col_res.value)) {
                     if (!IS_STRING(idx_res.value)) { runtime_error(interp, "Object key must be a string."); return ERROR_RESULT(); }
                     ObjString* key = AS_STRING(idx_res.value);
                     map_set(interp, &AS_OBJECT(col_res.value)->map, key, value_res.value);
                } else {
                    runtime_error(interp, "Can only assign to elements of arrays and objects."); return ERROR_RESULT();
                }
                if (IS_OBJ(value_res.value)) pop_root(interp);
            } else {
                runtime_error(interp, "Invalid assignment target."); return ERROR_RESULT();
            }

            // Assignment is an expression that evaluates to the assigned value.
           return OK_RESULT(value_res.value);
        }
        case AST_INDEX: {
            ExecResult collection_res = eval(interp, node->as.index.object);
            if (collection_res.status != EXEC_OK) return collection_res;
            
            // GC-Robustness: Protect collection while evaluating index.
            if (IS_OBJ(collection_res.value)) push_root(interp, AS_OBJ(collection_res.value));
            
            ExecResult index_res = eval(interp, node->as.index.index);

            if (IS_OBJ(collection_res.value)) pop_root(interp);
            if (index_res.status != EXEC_OK) return index_res;

            Value collection = collection_res.value;
            Value index = index_res.value;

            if (IS_ARRAY(collection)) {
                ObjArray* array = AS_ARRAY(collection);
                int64_t idx;

                if (IS_INT(index)) {
                    idx = AS_INT(index);
                } else if (IS_DOUBLE(index)) {
                    double d_idx = AS_DOUBLE(index);
                    if (floor(d_idx) != d_idx) {
                        runtime_error(interp, "Array index must be an integer."); return ERROR_RESULT();
                    }
                    idx = (int64_t)d_idx;
                } else {
                    runtime_error(interp, "Array index must be a number."); return ERROR_RESULT();
                }

                if (idx < 0 || (size_t)idx >= array->count) {
                    runtime_error(interp, "Array index out of bounds."); return ERROR_RESULT();
                }
                return OK_RESULT(array->values[(size_t)idx]);
            }
            if (IS_OBJECT(collection)) {
                if (!IS_STRING(index)) {
                    runtime_error(interp, "Object key must be a string."); return ERROR_RESULT();
                }
                ObjObject* object = AS_OBJECT(collection);
                ObjString* key = AS_STRING(index);
                Value val;
                if (map_get(&object->map, key, &val)) {
                    return OK_RESULT(val);
                }
                return OK_RESULT(make_nil()); // Key not found.
            }
            runtime_error(interp, "Can only index into arrays and objects.");
            return ERROR_RESULT();
        }
        case AST_UNARY: {
            ExecResult right_res = eval(interp, node->as.unary.child);
            if (right_res.status != EXEC_OK) return right_res;

            Value right = right_res.value;
            if (IS_INT(right)) {
                return OK_RESULT(make_int(-AS_INT(right)));
            }
            if (IS_DOUBLE(right)) {
                return OK_RESULT(make_double(-AS_DOUBLE(right)));
            }
            
            runtime_error(interp, "Operand must be a number for unary '-'.");
            return ERROR_RESULT();
        }
        case AST_BINARY: {
            if (node->as.binary.op.type == T_AMP_AMP) {
                ExecResult left_res = eval(interp, node->as.binary.left);
                if (left_res.status != EXEC_OK) return left_res;
                if (!is_truthy(left_res.value)) return OK_RESULT(make_bool(false));
                ExecResult right_res = eval(interp, node->as.binary.right);
                if (right_res.status != EXEC_OK) return right_res;
                return OK_RESULT(make_bool(is_truthy(right_res.value)));
            }
            if (node->as.binary.op.type == T_PIPE_PIPE) {
                ExecResult left_res = eval(interp, node->as.binary.left);
                if (left_res.status != EXEC_OK) return left_res;
                if (is_truthy(left_res.value)) return OK_RESULT(make_bool(true));
                ExecResult right_res = eval(interp, node->as.binary.right);
                if (right_res.status != EXEC_OK) return right_res;
                return OK_RESULT(make_bool(is_truthy(right_res.value)));
            }

            ExecResult left_res = eval(interp, node->as.binary.left);
            if (left_res.status != EXEC_OK) return left_res;
            
            // GC-Robustness: Protect left operand while evaluating right operand.
            if (IS_OBJ(left_res.value)) push_root(interp, AS_OBJ(left_res.value));

            ExecResult right_res = eval(interp, node->as.binary.right);

            if (IS_OBJ(left_res.value)) pop_root(interp);
            if (right_res.status != EXEC_OK) return right_res;
            
            Value left = left_res.value;
            Value right = right_res.value;

            switch (node->as.binary.op.type) {
                case T_EQUAL_EQUAL: return OK_RESULT(make_bool(values_are_equal(left, right)));
                case T_BANG_EQUAL:  return OK_RESULT(make_bool(!values_are_equal(left, right)));

                case T_PLUS:
                    if (IS_INT(left) && IS_INT(right)) {
                        int64_t a = AS_INT(left);
                        int64_t b = AS_INT(right);
                        if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
                            return OK_RESULT(make_double((double)a + (double)b));
                        }
                        return OK_RESULT(make_int(a + b));
                    }
                    if (IS_NUMERIC(left) && IS_NUMERIC(right)) {
                        return OK_RESULT(make_double(AS_NUMBER(left) + AS_NUMBER(right)));
                    }
                    if (IS_STRING(left) || IS_STRING(right)) {
                        ObjString* s1 = value_to_string(interp, left);
                        push_root(interp, (Obj*)s1); // Protect from GC if s2 creation triggers it

                        ObjString* s2 = value_to_string(interp, right);
                        pop_root(interp); // s1 is safe now

                        size_t total_len = s1->length + s2->length;
                        char* result_chars = PRATT_MALLOC(total_len + 1);
                        if (!result_chars) {
                            runtime_error(interp, "Out of memory during string concatenation.");
                            return ERROR_RESULT();
                        }

                        memcpy(result_chars, s1->chars, s1->length);
                        memcpy(result_chars + s1->length, s2->chars, s2->length);
                        result_chars[total_len] = '\0';
                        
                        ObjString* result_str = make_heap_string(interp, result_chars, total_len);
                        PRATT_FREE(result_chars);
                        return OK_RESULT(make_obj((Obj*)result_str));
                    }
                    runtime_error(interp, "Operands for '+' must be numbers or at least one string.");
                    return ERROR_RESULT();

                case T_MINUS:
                    if (IS_INT(left) && IS_INT(right)) {
                        int64_t a = AS_INT(left);
                        int64_t b = AS_INT(right);
                        if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b)) {
                            return OK_RESULT(make_double((double)a - (double)b));
                        }
                        return OK_RESULT(make_int(a - b));
                    }
                    if (IS_NUMERIC(left) && IS_NUMERIC(right)) {
                        return OK_RESULT(make_double(AS_NUMBER(left) - AS_NUMBER(right)));
                    }
                    runtime_error(interp, "Operands for '-' must be numbers.");
                    return ERROR_RESULT();

                case T_STAR:
                    if (IS_INT(left) && IS_INT(right)) {
                        int64_t a = AS_INT(left);
                        int64_t b = AS_INT(right);
                        // Check for overflow before multiplication
                        if (a > 0) {
                            if (b > 0) { if (a > INT64_MAX / b) goto mul_overflow; }
                            else { if (b < INT64_MIN / a) goto mul_overflow; }
                        } else if (a < 0) {
                            if (b > 0) { if (a < INT64_MIN / b) goto mul_overflow; }
                            else { if (a != 0 && b < INT64_MAX / a) goto mul_overflow; }
                        }
                        return OK_RESULT(make_int(a * b));
                    mul_overflow:
                        return OK_RESULT(make_double((double)a * (double)b));
                    }
                    if (IS_NUMERIC(left) && IS_NUMERIC(right)) {
                        return OK_RESULT(make_double(AS_NUMBER(left) * AS_NUMBER(right)));
                    }
                    runtime_error(interp, "Operands for '*' must be numbers.");
                    return ERROR_RESULT();
                
                case T_SLASH:
                    if (!IS_NUMERIC(left) || !IS_NUMERIC(right)) {
                        runtime_error(interp, "Operands for '/' must be numbers."); return ERROR_RESULT();
                    }
                    double r_div = AS_NUMBER(right);
                    if (r_div == 0) { runtime_error(interp, "Division by zero."); return ERROR_RESULT(); }
                    return OK_RESULT(make_double(AS_NUMBER(left) / r_div));
                
                case T_LESS: case T_LESS_EQUAL: case T_GREATER: case T_GREATER_EQUAL:
                    if (!IS_NUMERIC(left) || !IS_NUMERIC(right)) {
                        runtime_error(interp, "Operands must be numbers for comparison."); return ERROR_RESULT();
                    }
                    double l_cmp = AS_NUMBER(left), r_cmp = AS_NUMBER(right);
                    switch (node->as.binary.op.type) {
                         case T_LESS:          return OK_RESULT(make_bool(l_cmp < r_cmp));
                         case T_LESS_EQUAL:    return OK_RESULT(make_bool(l_cmp <= r_cmp));
                         case T_GREATER:       return OK_RESULT(make_bool(l_cmp > r_cmp));
                         case T_GREATER_EQUAL: return OK_RESULT(make_bool(l_cmp >= r_cmp));
                         default: break; // Unreachable
                    }
                default: break;
            }

            runtime_error(interp, "Unsupported binary operation for the given types.");
            return ERROR_RESULT();
        }
        case AST_TERNARY: {
            ExecResult cond_res = eval(interp, node->as.ternary.cond);
            if (cond_res.status != EXEC_OK) return cond_res;

            // GC-Robustness: Protect condition result while evaluating the chosen branch.
            if (IS_OBJ(cond_res.value)) push_root(interp, AS_OBJ(cond_res.value));

            ExecResult branch_res;
            if (is_truthy(cond_res.value)) {
                branch_res = eval(interp, node->as.ternary.then_branch);
            } else {
                branch_res = eval(interp, node->as.ternary.else_branch);
            }

            if (IS_OBJ(cond_res.value)) pop_root(interp);
            return branch_res;
        }
        case AST_CALL: {
            ExecResult callee_res = eval(interp, node->as.call.callee);
            if (callee_res.status != EXEC_OK) return callee_res;

            Value args[16]; // Demo limit
            if (node->as.call.argc > 16) {
                runtime_error(interp, "Cannot pass more than 16 arguments.");
                return ERROR_RESULT();
            }

            size_t push_count = 0;
            
            if (IS_OBJ(callee_res.value))
            {
                // Push callee onto the root stack before evaluating args to protect it from GC.
                push_root(interp, AS_OBJ(callee_res.value));
				push_count++;
			}

            for (size_t i = 0; i < node->as.call.argc; i++) {
                ExecResult arg_res = eval(interp, node->as.call.args[i]);
                if (arg_res.status != EXEC_OK) {
                    // On error, pop everything we've pushed so far.
                    for (size_t j = 0; j < push_count; j++) pop_root(interp);
                    return arg_res;
                }
                args[i] = arg_res.value;
                // Push current arg onto root stack to protect it during the next arg's evaluation.
				if (IS_OBJ(args[i])) {
                    push_root(interp, AS_OBJ(args[i]));
					push_count++;
				}
            }

            // Now that all args are evaluated and rooted, we can make the call.
            ExecResult result = call_value(interp, callee_res.value, node->as.call.argc, args);

            // Clean up the root stack.
            for (size_t i = 0; i < push_count; i++) pop_root(interp);

            return result;
        }
        default:
            runtime_error(interp, "Unknown AST node type.");
            return ERROR_RESULT();
    }
}

// --- THE EXECUTOR ---
ExecResult execute_block(Interpreter *interp, Statement **stmts, size_t count, ObjEnv *block_env) {
    ObjEnv* previous_env = interp->env;
    interp->env = block_env;
    ExecResult res = OK_RESULT(make_nil());

    for (size_t i = 0; i < count; i++) {
        res = execute(interp, stmts[i]);
        if (res.status != EXEC_OK) break;
    }
    
    interp->env = previous_env;
    return res;
}

ExecResult execute(Interpreter *interp, Statement *stmt) {
    if (interp->had_error || !stmt) return ERROR_RESULT();

    switch (stmt->type) {
        case ST_EXPR: {
            ExecResult res = eval(interp, stmt->as.expr.expr);
            if (res.status != EXEC_OK) return res;
            return OK_RESULT(make_nil());
        }
        case ST_VAR: {
            Value initializer = make_nil();
            if (stmt->as.var.initializer) {
                ExecResult init_res = eval(interp, stmt->as.var.initializer);
                if (init_res.status != EXEC_OK) return init_res;
                initializer = init_res.value;
            }
            ObjString* name = interpreter_intern_string(interp, stmt->as.var.name, strlen(stmt->as.var.name));
            env_define(interp, interp->env, name, initializer);
            break;
        }
        case ST_BLOCK: {
            // Create the new environment as a GC object, with the current env as its parent.
            ObjEnv* block_env = new_env_obj(interp, interp->env);
            if (!block_env) { /* new_env_obj would have exited on OOM */ return ERROR_RESULT(); }
            return execute_block(interp, stmt->as.block.list, stmt->as.block.count, block_env);
        }
        case ST_IF: {
            ExecResult cond_res = eval(interp, stmt->as.if_s.condition);
            if (cond_res.status != EXEC_OK) return cond_res;

            if (is_truthy(cond_res.value)) {
                return execute(interp, stmt->as.if_s.then_branch);
            } else if (stmt->as.if_s.else_branch != NULL) {
                return execute(interp, stmt->as.if_s.else_branch);
            }
            break;
        }
        case ST_WHILE: {
            while (true) {
                ExecResult cond_res = eval(interp, stmt->as.while_s.condition);
                if (cond_res.status != EXEC_OK) return cond_res;
                if (!is_truthy(cond_res.value)) break;

                ExecResult body_res = execute(interp, stmt->as.while_s.body);
                
                // Handle control flow statements
                if (body_res.status == EXEC_BREAK) {
                    break; // Exit the loop due to a 'break' statement.
                }
                if (body_res.status == EXEC_CONTINUE) {
                    continue; // Skip to the next iteration.
                }
                if (body_res.status != EXEC_OK) {
                    return body_res; // Propagate EXEC_RETURN or EXEC_ERROR.
                }
            }
            break;
        }
        case ST_FOR: {
            // A for loop introduces a new scope, primarily for variables
            // declared in its initializer.
            ObjEnv* for_env = new_env_obj(interp, interp->env);
            ObjEnv* previous_env = interp->env;
            interp->env = for_env;

            // 1. Execute the initializer once.
            if (stmt->as.for_s.initializer) {
                // The initializer is a statement, which could be a `var` decl
                // or an expression statement (which is just an expression).
                // We just execute it.
                if (stmt->as.for_s.initializer->type == ST_EXPR) {
                    // It's a pure expression, we need to eval it.
                    eval(interp, stmt->as.for_s.initializer->as.expr.expr);
                } else {
                    // It's a var declaration, execute it.
                    execute(interp, stmt->as.for_s.initializer);
               }
                if (interp->had_error) {
                     interp->env = previous_env; return ERROR_RESULT();
                }
            }

            // 2. Loop with condition, body, and increment.
            while (true) {
                // 2a. Check the condition. If none is provided, it's an infinite loop (always true).
                if (stmt->as.for_s.condition) {
                    ExecResult cond_res = eval(interp, stmt->as.for_s.condition);
                    if (cond_res.status != EXEC_OK) {
                        interp->env = previous_env;
                        return cond_res;
                    }
                    if (!is_truthy(cond_res.value)) {
                        break; // Condition is false, exit the loop.
                    }
                }

                // 2b. Execute the loop body.
                ExecResult body_res = execute(interp, stmt->as.for_s.body);

                // Handle control flow statements.
                if (body_res.status == EXEC_BREAK)   break; // Exit the loop.
                if (body_res.status == EXEC_RETURN) { interp->env = previous_env; return body_res; }
                if (body_res.status == EXEC_ERROR)  { interp->env = previous_env; return body_res; }

                // 2c. Execute the increment expression (if it exists) after the body.
                // This is also executed after a 'continue'.
                if (stmt->as.for_s.increment) {
                    ExecResult inc_res = eval(interp, stmt->as.for_s.increment);
                    if (inc_res.status != EXEC_OK) { interp->env = previous_env; return inc_res; }
                }
            }

            interp->env = previous_env; // Pop the for-loop's scope.
            break;
        }
        case ST_RETURN: {
            Value return_value = make_nil();
            if (stmt->as.ret.value) {
                ExecResult value_res = eval(interp, stmt->as.ret.value);
                if (value_res.status != EXEC_OK) return value_res;
                return_value = value_res.value;
            }
            return RETURN_RESULT(return_value);
        }
        case ST_BREAK: {
            return BREAK_RESULT();
        }
        case ST_CONTINUE: {
            return CONTINUE_RESULT();
        }
        case ST_FUNCTION: {
            Function* func = new_function(interp, stmt->as.func.param_count);
            push_root(interp, (Obj*)func);

            ObjString* name = interpreter_intern_string(interp, stmt->as.func.name, strlen(stmt->as.func.name));
            
            func->name = name;
            func->arity = stmt->as.func.param_count;
            func->body = stmt->as.func.body;
            func->closure = interp->env;

            if (func->arity > 0) {
                for(size_t i = 0; i < func->arity; i++) {
                    func->params[i] = interpreter_intern_string(interp, stmt->as.func.params[i], strlen(stmt->as.func.params[i]));
                }
            }

            // Now that the function is fully built, define it in the environment.
            // This makes the environment a root for the function.
            env_define(interp, interp->env, name, make_obj((Obj*)func));
            
            // The function is now rooted by the environment, so we can pop our temporary root.
            pop_root(interp); 
            break;
        }
        default:
            runtime_error(interp, "Unknown statement type for execution.");
            return ERROR_RESULT();
    }
    return OK_RESULT(make_nil()); // Default successful execution
}

// --- Built-in Functions ---

static Value builtin_toString(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1) {
        runtime_error(interp, "toString() expects 1 argument, but got %zu.", argc);
        return make_nil();
    }
    ObjString* str = value_to_string(interp, args[0]);
    return make_obj((Obj*)str);
}

static Value builtin_toNumber(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1) {
        runtime_error(interp, "toNumber() expects 1 argument, but got %zu.", argc);
        return make_nil();
    }

    Value val = args[0];
    switch (val.type) {
        case VAL_INT:
        case VAL_DOUBLE:
            return val; // Already a number
        case VAL_OBJ:
            if (IS_STRING(val)) {
                ObjString* str = AS_STRING(val);
                if (str->length == 0) return make_int(0);

                // Try to parse as an integer first.
                char* end_i;
                errno = 0;
                long long i_val = strtoll(str->chars, &end_i, 10);
                while (isspace((unsigned char)*end_i)) end_i++;

                if (*end_i == '\0' && errno != ERANGE) {
                    return make_int(i_val);
                }

                // If not a valid int, fall back to double.
                char* end_d;
                double d_val = strtod(str->chars, &end_d);
                while (isspace((unsigned char)*end_d)) end_d++;

                if (*end_d != '\0') {
                    return make_double(NAN); // Invalid number format
                }
                return make_double(d_val);
            }
            // All other object types convert to NaN
            return make_double(NAN);
        case VAL_BOOL:
            return make_int(AS_BOOL(val) ? 1 : 0);
        case VAL_NIL:
            return make_int(0);
        default:
            // All other types convert to NaN
            return make_double(NAN);
    }
}


// --- Cycle Detection for Printing ---
// A simple dynamic array to keep track of objects being visited during a print operation.
typedef struct {
    Obj** items;
    int count;
    int capacity;
} PrintVisitor;

static void visitor_init(PrintVisitor* visitor) {
    visitor->items = NULL;
    visitor->count = 0;
    visitor->capacity = 0;
}

static void visitor_free(PrintVisitor* visitor) {
    PRATT_FREE(visitor->items);
    visitor_init(visitor);
}

// Check if an object pointer is already in the visitor's list.
static bool visitor_contains(PrintVisitor* visitor, Obj* obj) {
    for (int i = 0; i < visitor->count; i++) {
        if (visitor->items[i] == obj) return true;
    }
    return false;
}

// Add an object pointer to the list.
static void visitor_push(PrintVisitor* visitor, Obj* obj) {
    if (visitor->capacity < visitor->count + 1) {
        int old_capacity = visitor->capacity;
        visitor->capacity = old_capacity < 8 ? 8 : old_capacity * 2;
        visitor->items = PRATT_REALLOC(visitor->items, sizeof(Obj*) * visitor->capacity);
        if (visitor->items == NULL) {
            // A non-fatal error. The print will be incomplete, but the interpreter won't crash.
            fprintf(stderr, "\n[prattscript-print-warning: out of memory tracking visited objects]\n");
            visitor->capacity = 0; // Prevent further attempts.
            return;
        }
    }
    visitor->items[visitor->count++] = obj;
}

// Remove the most recently added object pointer.
static void visitor_pop(PrintVisitor* visitor) {
    if (visitor->count > 0) {
        visitor->count--;
    }
}

// Forward declaration for the recursive helper
static void print_value_internal(Value value, PrintVisitor* visitor);

// The internal recursive function that does the work.
static void print_value_internal(Value value, PrintVisitor* visitor) {
    switch (value.type) {
        case VAL_INT:    printf("%" PRId64, AS_INT(value)); break;
        case VAL_DOUBLE: printf("%g", AS_DOUBLE(value)); break;
        case VAL_BOOL:   printf(AS_BOOL(value) ? "true" : "false"); break;
        case VAL_NIL:    printf("nil"); break;
        case VAL_BUILTIN: printf("<builtin>"); break;
        case VAL_OBJ: {
            // Check for a cycle before proceeding.
            if (visitor_contains(visitor, AS_OBJ(value))) {
                if (IS_ARRAY(value)) {
                    printf("[...]");
                } else if (IS_OBJECT(value)) {
                    printf("{...}");
                } else {
                    // This case should ideally not be hit for non-container types
                    // like strings and functions, but it's a safe fallback.
                    printf("<cycle>");
                }
                return;
            }

            // If it's a container, mark it as visited for the duration of this recursive branch.
            if (IS_ARRAY(value) || IS_OBJECT(value)) {
                visitor_push(visitor, AS_OBJ(value));
            }

            switch (OBJ_TYPE(value)) {
                case OBJ_STRING: printf("%s", AS_CSTRING(value)); break;
                case OBJ_FUNCTION: printf("<fn %s>", AS_FUNCTION(value)->name->chars); break;
                case OBJ_ARRAY: {
                    ObjArray* array = AS_ARRAY(value);
                    printf("[");
                    for (int i = 0; i < array->count; i++) {
                        print_value_internal(array->values[i], visitor);
                        if (i < array->count - 1) printf(", ");
                    }
                    printf("]");
                    break;
                }
                case OBJ_OBJECT: {
                    ObjObject* obj = AS_OBJECT(value);
                    printf("{");
                    int printed = 0;
                    for (int i = 0; i < obj->map.capacity; i++) {
                        if (obj->map.entries[i].key != NULL) {
                            if (printed > 0) printf(", ");
                            // Key is a string, which cannot be part of a value cycle itself.
                            printf("\"%s\": ", obj->map.entries[i].key->chars);
                            print_value_internal(obj->map.entries[i].value, visitor);
                            printed++;
                        }
                    }
                    printf("}");
                    break;
                }
                case OBJ_ENV: printf("[environment]"); break;
            }

            // Un-mark it after we are done printing its contents.
            if (IS_ARRAY(value) || IS_OBJECT(value)) {
                visitor_pop(visitor);
            }
            break;
        }
    }
}

// The public API function. It sets up the context for cycle detection.
void print_value(Value value) {
    PrintVisitor visitor;
    visitor_init(&visitor);
    print_value_internal(value, &visitor);
    visitor_free(&visitor);
}


static Value builtin_print(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1) { runtime_error(interp, "print() expects 1 argument, but got %zu.", argc); return make_nil(); }
    print_value(args[0]);
    return make_nil();
}

static Value builtin_println(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1) { runtime_error(interp, "println() expects 1 argument, but got %zu.", argc); return make_nil(); }
    print_value(args[0]);
    printf("\n");
    return make_nil();
}

static Value builtin_sqrt(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) { runtime_error(interp, "sqrt() expects 1 number argument."); return make_nil(); }
    return make_double(sqrt(AS_NUMBER(args[0])));
}

static Value builtin_upper(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1 || !IS_STRING(args[0])) { runtime_error(interp, "upper() expects 1 string argument."); return make_nil(); }
    
    ObjString* original = AS_STRING(args[0]);
    char* new_chars = PRATT_MALLOC(original->length + 1);
    if (!new_chars) { runtime_error(interp, "Out of memory."); return make_nil(); }
    for(size_t i = 0; i < original->length; i++) new_chars[i] = toupper(original->chars[i]);
    new_chars[original->length] = '\0';

    ObjString* result = make_heap_string(interp, new_chars, original->length);
    PRATT_FREE(new_chars);
    return make_obj((Obj*)result);
}

static Value builtin_lower(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1 || !IS_STRING(args[0])) { runtime_error(interp, "lower() expects 1 string argument."); return make_nil(); }

    ObjString* original = AS_STRING(args[0]);
    char* new_chars = PRATT_MALLOC(original->length + 1);
    if (!new_chars) { runtime_error(interp, "Out of memory."); return make_nil(); }
    for(size_t i = 0; i < original->length; i++) new_chars[i] = tolower(original->chars[i]);
    new_chars[original->length] = '\0';

    ObjString* result = make_heap_string(interp, new_chars, original->length);
    PRATT_FREE(new_chars);
    return make_obj((Obj*)result);
}

static Value builtin_compare(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 2) { runtime_error(interp, "compare() expects 2 arguments."); return make_nil(); }
    Value a = args[0];
    Value b = args[1];

    if (IS_STRING(a) && IS_STRING(b)) {
        int cmp = strcmp(AS_CSTRING(a), AS_CSTRING(b));
        return make_int(cmp < 0 ? -1 : (cmp > 0 ? 1 : 0));
    }
    if (IS_NUMERIC(a) && IS_NUMERIC(b)) {
        double diff = AS_NUMBER(a) - AS_NUMBER(b);
        return make_int(diff < 0 ? -1 : (diff > 0 ? 1 : 0));
    }
    runtime_error(interp, "compare() can only compare numbers and strings.");
    return make_nil();
}

static Value builtin_len(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1) { runtime_error(interp, "len() expects 1 argument, got %d", (int) argc); return make_nil(); }
    if (!IS_OBJ(args[0])) { runtime_error(interp, "len() argument must be a string, array, or object."); return make_nil(); }
    
    switch(OBJ_TYPE(args[0])) {
        case OBJ_STRING: return make_int((int64_t) AS_STRING(args[0])->length);
        case OBJ_ARRAY:  return make_int((int64_t) AS_ARRAY(args[0])->count);
        case OBJ_OBJECT: return make_int((int64_t) AS_OBJECT(args[0])->map.count);
        default: runtime_error(interp, "len() argument must be a string, array, or object."); return make_nil();
    }
}

static Value builtin_push(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 2 || !IS_ARRAY(args[0])) { runtime_error(interp, "push() expects an array and a value."); return make_nil(); }
    array_write(interp, AS_ARRAY(args[0]), args[1]);
    return args[0];
}

static Value builtin_pop(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1 || !IS_ARRAY(args[0])) { runtime_error(interp, "pop() expects an array."); return make_nil(); }
    ObjArray* arr = AS_ARRAY(args[0]);
    if (arr->count == 0) { runtime_error(interp, "Cannot pop from an empty array."); return make_nil(); }
    Value popped = arr->values[arr->count - 1];
    arr->count--;
    return popped;
}

static Value builtin_keys(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 1 || !IS_OBJECT(args[0])) { runtime_error(interp, "keys() expects an object."); return make_nil(); }
    
    ObjObject *obj = AS_OBJECT(args[0]);
    ObjArray *keys_array = new_array(interp);
    push_root(interp, (Obj*)keys_array);

    for (int i=0; i < obj->map.capacity; i++) {
        if (obj->map.entries[i].key != NULL) {
            array_write(interp, keys_array, make_obj((Obj*)obj->map.entries[i].key));
        }
    }
    
    pop_root(interp);
    return make_obj((Obj*)keys_array);
}

// --- GC Built-in Implementations ---

static Value builtin_gc_collect(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 0) {
        runtime_error(interp, "gc[\"collect\"]() expects 0 arguments, but got %zu.", argc);
        return make_nil();
    }
    collect_garbage(interp);
    return make_nil();
}

static Value builtin_gc_allocated(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 0) {
        runtime_error(interp, "gc[\"allocated\"]() expects 0 arguments, but got %zu.", argc);
        return make_nil();
    }
    return make_int((int64_t)interp->bytes_allocated);
}

static Value builtin_gc_next_gc(Interpreter *interp, size_t argc, Value *args) {
    if (argc != 0) {
        runtime_error(interp, "gc[\"next_gc\"]() expects 0 arguments, but got %zu.", argc);
        return make_nil();
    }
    return make_int((int64_t)interp->next_gc);
}
