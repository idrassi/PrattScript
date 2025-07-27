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
#include <windows.h>
#include <io.h>
#include <direct.h>
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#else
#include <unistd.h> // For access, unlink, getcwd, chdir, setenv, unsetenv
#include <sys/wait.h>
#include <dirent.h>
#endif
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
#include <time.h>
#include <stdlib.h>
#include <sys/stat.h>

// For cJSON integration
#include "cJSON.h"

// For strptime
#if !defined(_WIN32) && !defined(_MSC_VER)
#define HAVE_STRPTIME
#endif


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
        case OBJ_RESOURCE: {
            ObjResource* resource = (ObjResource*)object;
            // Finalizer: ensure the resource is released if the handle is GC'd
            if (!resource->is_finalized && resource->vtable->finalize) {
               resource->vtable->finalize(resource->context);
            }
            reallocate(interp, object, sizeof(ObjResource), 0);
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

// --- Generic Resource Implementation ---
static ObjResource* new_resource(Interpreter* interp, void* context, const ResourceVTable* vtable) {
    ObjResource* resource = (ObjResource*)allocate_object(interp, sizeof(ObjResource), OBJ_RESOURCE);
    resource->context = context;
    resource->vtable = vtable;
    resource->is_finalized = false;
    return resource;
}

// --- Value Creation Helpers ---
Value make_int(int64_t value) { return (Value){VAL_INT, .as.integer = value}; }
Value make_double(double value) { return (Value){VAL_DOUBLE, .as.number = value}; }
Value make_bool(bool value) { return (Value){VAL_BOOL, .as.boolean = value}; }
Value make_nil(void) { return (Value){VAL_NIL}; }
Value make_tombstone(void) { return (Value){VAL_TOMBSTONE}; }
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
        if (entry->key == NULL) { // Can be empty or a tombstone
            if (IS_TOMBSTONE(entry->value)) {
                // Found a tombstone.
                if (tombstone == NULL) tombstone = entry;
            } else { // It must be an empty bucket (IS_NIL).
                return tombstone != NULL ? tombstone : entry;
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
    
    // A new key is being inserted if the slot was previously empty or a tombstone.
    bool is_new_key = entry->key == NULL;
    if (is_new_key) map->count++;

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
            entry->value = make_tombstone(); // Tombstone
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
        case OBJ_RESOURCE: // No outgoing references to mark from a resource object
            break;
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
                case OBJ_RESOURCE: {
                    ObjResource* res = AS_RESOURCE(value);
                    const char* status = res->is_finalized ? " (closed)" : "";
                    length = snprintf(NULL, 0, "<resource %s%s>", res->vtable->type_name, status);
                    buffer = PRATT_MALLOC(length + 1);
                    if (!buffer) goto oom;
                    snprintf(buffer, length + 1, "<resource %s%s>", res->vtable->type_name, status);
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
static Value builtin_upper(Interpreter*, size_t, Value*);
static Value builtin_lower(Interpreter*, size_t, Value*);
static Value builtin_compare(Interpreter*, size_t, Value*);
static Value builtin_len(Interpreter*, size_t, Value*);
static Value builtin_push(Interpreter*, size_t, Value*);
static Value builtin_pop(Interpreter*, size_t, Value*);
static Value builtin_keys(Interpreter*, size_t, Value*);
static Value builtin_toString(Interpreter*, size_t, Value*);
static Value builtin_toNumber(Interpreter*, size_t, Value*);
static Value builtin_typeof(Interpreter*, size_t, Value*);
static Value builtin_clock(Interpreter*, size_t, Value*);
static Value builtin_time(Interpreter*, size_t, Value*);
static Value builtin_exit(Interpreter*, size_t, Value*);
static Value builtin_assert(Interpreter*, size_t, Value*);
// GC object functions
static Value builtin_gc_collect(Interpreter*, size_t, Value*);
static Value builtin_gc_allocated(Interpreter*, size_t, Value*);
static Value builtin_gc_next_gc(Interpreter*, size_t, Value*);
// Math object functions
static Value builtin_math_abs(Interpreter*, size_t, Value*);
static Value builtin_math_floor(Interpreter*, size_t, Value*);
static Value builtin_math_ceil(Interpreter*, size_t, Value*);
static Value builtin_math_pow(Interpreter*, size_t, Value*);
static Value builtin_math_sqrt(Interpreter*, size_t, Value*);
static Value builtin_math_sin(Interpreter*, size_t, Value*);
static Value builtin_math_cos(Interpreter*, size_t, Value*);
static Value builtin_math_tan(Interpreter*, size_t, Value*);
static Value builtin_math_log(Interpreter*, size_t, Value*);
static Value builtin_math_log10(Interpreter*, size_t, Value*);
static Value builtin_math_random(Interpreter*, size_t, Value*);
static Value builtin_math_random_seed(Interpreter*, size_t, Value*);
static Value builtin_math_random_int(Interpreter*, size_t, Value*);
static Value builtin_math_random_bytes(Interpreter*, size_t, Value*);

// String object functions
static Value builtin_string_split(Interpreter*, size_t, Value*);
static Value builtin_string_trim(Interpreter*, size_t, Value*);
// Array object functions
static Value builtin_array_slice(Interpreter*, size_t, Value*);
static Value builtin_array_join(Interpreter*, size_t, Value*);

// Core, global helpers
static Value builtin_range(Interpreter*, size_t, Value*);
static Value builtin_min(Interpreter*, size_t, Value*);
static Value builtin_max(Interpreter*, size_t, Value*);
static Value builtin_round(Interpreter*, size_t, Value*);
static Value builtin_sleep(Interpreter*, size_t, Value*);

// String object extensions
static Value builtin_string_replace(Interpreter*, size_t, Value*);
static Value builtin_string_startsWith(Interpreter*, size_t, Value*);
static Value builtin_string_endsWith(Interpreter*, size_t, Value*);
static Value builtin_string_indexOf(Interpreter*, size_t, Value*);
static Value builtin_string_substring(Interpreter*, size_t, Value*);
static Value builtin_string_repeat(Interpreter*, size_t, Value*);
static Value builtin_string_padStart(Interpreter*, size_t, Value*);
static Value builtin_string_padEnd(Interpreter*, size_t, Value*);

// Array object extensions
static Value builtin_array_map(Interpreter*, size_t, Value*);
static Value builtin_array_filter(Interpreter*, size_t, Value*);
static Value builtin_array_reduce(Interpreter*, size_t, Value*);
static Value builtin_array_indexOf(Interpreter*, size_t, Value*);
static Value builtin_array_includes(Interpreter*, size_t, Value*);
static Value builtin_array_sort(Interpreter*, size_t, Value*);
static Value builtin_array_reverse(Interpreter*, size_t, Value*);
static Value builtin_array_shuffle(Interpreter*, size_t, Value*);

// fs object
static Value builtin_fs_readFile(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_writeFile(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_exists(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_listDir(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_remove(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_open(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_read(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_write(Interpreter* interp, size_t argc, Value* args);
static Value builtin_fs_close(Interpreter* interp, size_t argc, Value* args);

// path object
static Value builtin_path_join(Interpreter* interp, size_t argc, Value* args);
static Value builtin_path_basename(Interpreter* interp, size_t argc, Value* args);
static Value builtin_path_dirname(Interpreter* interp, size_t argc, Value* args);
static Value builtin_path_extname(Interpreter* interp, size_t argc, Value* args);

// os object
static Value builtin_os_getenv(Interpreter* interp, size_t argc, Value* args);
static Value builtin_os_setenv(Interpreter* interp, size_t argc, Value* args);
static Value builtin_os_unsetenv(Interpreter* interp, size_t argc, Value* args);
static Value builtin_os_exec(Interpreter* interp, size_t argc, Value* args);
static Value builtin_os_getcwd(Interpreter* interp, size_t argc, Value* args);
static Value builtin_os_setcwd(Interpreter* interp, size_t argc, Value* args);
static Value builtin_os_platform(Interpreter* interp, size_t argc, Value* args);

// date object
static Value builtin_date_now(Interpreter* interp, size_t argc, Value* args);
static Value builtin_date_format(Interpreter* interp, size_t argc, Value* args);
static Value builtin_date_parse(Interpreter* interp, size_t argc, Value* args);
static Value builtin_date_utc(Interpreter* interp, size_t argc, Value* args);
static Value builtin_date_local(Interpreter* interp, size_t argc, Value* args);

// resource object
static Value builtin_resource_close(Interpreter* interp, size_t argc, Value* args);
static Value builtin_resource_type(Interpreter* interp, size_t argc, Value* args);
static Value builtin_resource_is_closed(Interpreter* interp, size_t argc, Value* args);

// *********************** JSON BUILTINS ***********************
static Value builtin_json_parse(Interpreter* interp, size_t argc, Value* args);
static Value builtin_json_stringify(Interpreter* interp, size_t argc, Value* args);
// *************************************************************

static BuiltinDef builtins[] = {
    {"print",   builtin_print},
    {"println", builtin_println},
    {"upper",   builtin_upper},
    {"lower",   builtin_lower},
    {"compare", builtin_compare},
    {"len",     builtin_len},
    {"push",    builtin_push},
    {"pop",     builtin_pop},
    {"keys",    builtin_keys},
    {"toString", builtin_toString},
    {"toNumber", builtin_toNumber},
    {"typeof",  builtin_typeof},
    {"clock",   builtin_clock},
    {"time",    builtin_time},
    {"exit",    builtin_exit},
    {"assert",  builtin_assert},
    {"range",   builtin_range},
    {"min",     builtin_min},
    {"max",     builtin_max},
    {"round",   builtin_round},
    {"sleep",   builtin_sleep},
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

    // --- Seed random number generator ---
    sfc64_init(&interp->rng, (uint64_t)time(NULL));

    // Create the global environment. It's an object now.
    interp->env = new_env_obj(interp, NULL);
    if (interp->env == NULL) { // Check for allocation failure
        runtime_error(interp, "Out of memory initializing interpreter.");
        return;
    }
    
    // --- Push env to root stack during initialization ---
    push_root(interp, (Obj*)interp->env);

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

    pop_root(interp);  // Un-root the gc object; it's now rooted by the global env.

    // --- Math Built-in Object ---
    ObjObject* math_object = new_object(interp);
    push_root(interp, (Obj*)math_object);
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "abs", 3), make_builtin(builtin_math_abs));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "floor", 5), make_builtin(builtin_math_floor));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "ceil", 4), make_builtin(builtin_math_ceil));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "pow", 3), make_builtin(builtin_math_pow));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "sqrt", 4), make_builtin(builtin_math_sqrt));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "sin", 3), make_builtin(builtin_math_sin));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "cos", 3), make_builtin(builtin_math_cos));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "tan", 3), make_builtin(builtin_math_tan));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "log", 3), make_builtin(builtin_math_log));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "log10", 5), make_builtin(builtin_math_log10));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "random", 6), make_builtin(builtin_math_random));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "seedrandom", 10), make_builtin(builtin_math_random_seed));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "randomInt", 9), make_builtin(builtin_math_random_int));
    map_set(interp, &math_object->map, interpreter_intern_string(interp, "randomBytes", 11), make_builtin(builtin_math_random_bytes));
    env_define(interp, interp->env, interpreter_intern_string(interp, "math", 4), make_obj((Obj*)math_object));
    pop_root(interp);

    // --- String Built-in Object ---
    ObjObject* string_object = new_object(interp);
    push_root(interp, (Obj*)string_object);
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "split", 5), make_builtin(builtin_string_split));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "trim", 4), make_builtin(builtin_string_trim));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "replace", 7), make_builtin(builtin_string_replace));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "startsWith", 10), make_builtin(builtin_string_startsWith));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "endsWith", 8), make_builtin(builtin_string_endsWith));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "indexOf", 7), make_builtin(builtin_string_indexOf));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "substring", 9), make_builtin(builtin_string_substring));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "repeat", 6), make_builtin(builtin_string_repeat));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "padStart", 8), make_builtin(builtin_string_padStart));
    map_set(interp, &string_object->map, interpreter_intern_string(interp, "padEnd", 6), make_builtin(builtin_string_padEnd));
    env_define(interp, interp->env, interpreter_intern_string(interp, "string", 6), make_obj((Obj*)string_object));
    pop_root(interp);

    // --- Array Built-in Object ---
    ObjObject* array_object = new_object(interp);
    push_root(interp, (Obj*)array_object);
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "slice", 5), make_builtin(builtin_array_slice));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "join", 4), make_builtin(builtin_array_join));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "map", 3), make_builtin(builtin_array_map));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "filter", 6), make_builtin(builtin_array_filter));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "reduce", 6), make_builtin(builtin_array_reduce));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "indexOf", 7), make_builtin(builtin_array_indexOf));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "includes", 8), make_builtin(builtin_array_includes));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "sort", 4), make_builtin(builtin_array_sort));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "reverse", 7), make_builtin(builtin_array_reverse));
    map_set(interp, &array_object->map, interpreter_intern_string(interp, "shuffle", 7), make_builtin(builtin_array_shuffle));
    env_define(interp, interp->env, interpreter_intern_string(interp, "array", 5), make_obj((Obj*)array_object));
    pop_root(interp);

    // --- fs, path, os, date objects ---

    // FS Object
    ObjObject* fs_object = new_object(interp);
    push_root(interp, (Obj*)fs_object);
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "readFile", 8), make_builtin(builtin_fs_readFile));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "writeFile", 9), make_builtin(builtin_fs_writeFile));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "exists", 6), make_builtin(builtin_fs_exists));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "listDir", 7), make_builtin(builtin_fs_listDir));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "remove", 6), make_builtin(builtin_fs_remove));
    // File functions operate on ObjResource
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "open", 4), make_builtin(builtin_fs_open));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "read", 4), make_builtin(builtin_fs_read));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "write", 5), make_builtin(builtin_fs_write));
    map_set(interp, &fs_object->map, interpreter_intern_string(interp, "close", 5), make_builtin(builtin_resource_close));
    env_define(interp, interp->env, interpreter_intern_string(interp, "fs", 2), make_obj((Obj*)fs_object));
    pop_root(interp);

    // Path Object
    ObjObject* path_object = new_object(interp);
    push_root(interp, (Obj*)path_object);
    map_set(interp, &path_object->map, interpreter_intern_string(interp, "join", 4), make_builtin(builtin_path_join));
    map_set(interp, &path_object->map, interpreter_intern_string(interp, "basename", 8), make_builtin(builtin_path_basename));
    map_set(interp, &path_object->map, interpreter_intern_string(interp, "dirname", 7), make_builtin(builtin_path_dirname));
    map_set(interp, &path_object->map, interpreter_intern_string(interp, "extname", 7), make_builtin(builtin_path_extname));
    env_define(interp, interp->env, interpreter_intern_string(interp, "path", 4), make_obj((Obj*)path_object));
    pop_root(interp);

    // OS Object
    ObjObject* os_object = new_object(interp);
    push_root(interp, (Obj*)os_object);
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "getenv", 6), make_builtin(builtin_os_getenv));
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "setenv", 6), make_builtin(builtin_os_setenv));
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "unsetenv", 8), make_builtin(builtin_os_unsetenv));
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "exec", 4), make_builtin(builtin_os_exec));
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "getcwd", 6), make_builtin(builtin_os_getcwd));
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "setcwd", 6), make_builtin(builtin_os_setcwd));
    map_set(interp, &os_object->map, interpreter_intern_string(interp, "platform", 8), make_builtin(builtin_os_platform));
    env_define(interp, interp->env, interpreter_intern_string(interp, "os", 2), make_obj((Obj*)os_object));
    pop_root(interp);

    // Date Object
    ObjObject* date_object = new_object(interp);
    push_root(interp, (Obj*)date_object);
    map_set(interp, &date_object->map, interpreter_intern_string(interp, "now", 3), make_builtin(builtin_date_now));
    map_set(interp, &date_object->map, interpreter_intern_string(interp, "format", 6), make_builtin(builtin_date_format));
    map_set(interp, &date_object->map, interpreter_intern_string(interp, "parse", 5), make_builtin(builtin_date_parse));
    map_set(interp, &date_object->map, interpreter_intern_string(interp, "utc", 3), make_builtin(builtin_date_utc));
    map_set(interp, &date_object->map, interpreter_intern_string(interp, "local", 5), make_builtin(builtin_date_local));
    env_define(interp, interp->env, interpreter_intern_string(interp, "date", 4), make_obj((Obj*)date_object));
    pop_root(interp);

    // --- Generic Resource Object ---
    ObjObject* resource_object = new_object(interp);
    push_root(interp, (Obj*)resource_object);
    map_set(interp, &resource_object->map, interpreter_intern_string(interp, "close", 5), make_builtin(builtin_resource_close));
    map_set(interp, &resource_object->map, interpreter_intern_string(interp, "type", 4), make_builtin(builtin_resource_type));
    map_set(interp, &resource_object->map, interpreter_intern_string(interp, "isClosed", 8), make_builtin(builtin_resource_is_closed));
    env_define(interp, interp->env, interpreter_intern_string(interp, "resource", 8), make_obj((Obj*)resource_object));
    pop_root(interp);
    
    // --- JSON Object ---
    ObjObject* json_object = new_object(interp);
    push_root(interp, (Obj*)json_object);
    map_set(interp, &json_object->map, interpreter_intern_string(interp, "parse", 5), make_builtin(builtin_json_parse));
    map_set(interp, &json_object->map, interpreter_intern_string(interp, "stringify", 9), make_builtin(builtin_json_stringify));
    env_define(interp, interp->env, interpreter_intern_string(interp, "json", 4), make_obj((Obj*)json_object));
    pop_root(interp);

    // --- Pop env, now that it's fully populated ---
    pop_root(interp); 
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
                     else { runtime_error(interp, "Array index must be an integer for assignment."); if (IS_OBJ(value_res.value)) pop_root(interp); return ERROR_RESULT(); }

                     if (idx < 0 || (size_t)idx >= arr->count) { runtime_error(interp, "Array index out of bounds for assignment."); if (IS_OBJ(value_res.value)) pop_root(interp); return ERROR_RESULT(); }
                    arr->values[(size_t)idx] = value_res.value;
                } else if (IS_OBJECT(col_res.value)) {
                     if (!IS_STRING(idx_res.value)) { runtime_error(interp, "Object key must be a string."); if (IS_OBJ(value_res.value)) pop_root(interp); return ERROR_RESULT(); }
                     ObjString* key = AS_STRING(idx_res.value);
                     map_set(interp, &AS_OBJECT(col_res.value)->map, key, value_res.value);
                } else {
                    runtime_error(interp, "Can only assign to elements of arrays and objects."); if (IS_OBJ(value_res.value)) pop_root(interp); return ERROR_RESULT();
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

            switch (node->as.unary.op.type) {
                case T_MINUS:
                    if (IS_INT(right)) {
                        int64_t val = AS_INT(right);
                        // Handle overflow when negating the most negative integer.
                        if (val == INT64_MIN) {
                            return OK_RESULT(make_double(-(double)val));
                        }
                        return OK_RESULT(make_int(-val));
                    }
                    if (IS_DOUBLE(right)) {
                        return OK_RESULT(make_double(-AS_DOUBLE(right)));
                    }
                    runtime_error(interp, "Operand for '-' must be a number.");
                    return ERROR_RESULT();
                
                case T_TILDE:
                    if (!IS_INT(right)) {
                        runtime_error(interp, "Operand for '~' must be an integer.");
                        return ERROR_RESULT();
                    }
                    return OK_RESULT(make_int(~AS_INT(right)));
                
                default:
                    // Should be unreachable if parser is correct.
                    runtime_error(interp, "Unsupported unary operator.");
                    return ERROR_RESULT();
            }
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

                case T_PERCENT:
                    if (!IS_NUMERIC(left) || !IS_NUMERIC(right)) {
                        runtime_error(interp, "Operands for '%%' must be numbers.");
                        return ERROR_RESULT();
                    }

                    // If both are integers, perform integer modulo.
                    if (IS_INT(left) && IS_INT(right)) {
                        int64_t l_int = AS_INT(left);
                        int64_t r_int = AS_INT(right);
                        if (r_int == 0) {
                            runtime_error(interp, "Modulo by zero.");
                            return ERROR_RESULT();
                        }
                        // Handle the specific integer overflow case of INT64_MIN % -1, which is UB in C.
                        // The mathematical result is 0, so we return that directly.
                        if (l_int == INT64_MIN && r_int == -1) {
                            return OK_RESULT(make_int(0));
                        }
                        return OK_RESULT(make_int(l_int % r_int));
                    }

                    // Otherwise, perform floating point remainder using fmod().
                    double r_mod = AS_NUMBER(right);
                    if (r_mod == 0.0) { runtime_error(interp, "Modulo by zero."); return ERROR_RESULT(); }
                    return OK_RESULT(make_double(fmod(AS_NUMBER(left), r_mod)));
                
                // --- Bitwise Operators ---
                case T_AMP:
                    if (!IS_INT(left) || !IS_INT(right)) {
                        runtime_error(interp, "Operands for '&' must be integers."); return ERROR_RESULT();
                    }
                    return OK_RESULT(make_int(AS_INT(left) & AS_INT(right)));
                
                case T_PIPE:
                    if (!IS_INT(left) || !IS_INT(right)) {
                        runtime_error(interp, "Operands for '|' must be integers."); return ERROR_RESULT();
                    }
                    return OK_RESULT(make_int(AS_INT(left) | AS_INT(right)));
                
                case T_CARET:
                    if (!IS_INT(left) || !IS_INT(right)) {
                        runtime_error(interp, "Operands for '^' must be integers."); return ERROR_RESULT();
                    }
                    return OK_RESULT(make_int(AS_INT(left) ^ AS_INT(right)));

                case T_STAR_STAR:
                    if (!IS_NUMERIC(left) || !IS_NUMERIC(right)) {
                        runtime_error(interp, "Operands for '**' must be numbers."); return ERROR_RESULT();
                    }
                    return OK_RESULT(make_double(pow(AS_NUMBER(left), AS_NUMBER(right))));

                case T_LESS_LESS:
                    if (!IS_INT(left) || !IS_INT(right)) {
                        runtime_error(interp, "Operands for '<<' must be integers."); return ERROR_RESULT();
                    }
                    int64_t l_shift_left = AS_INT(left);
                    int64_t r_shift_left = AS_INT(right);
                    if (r_shift_left < 0 || r_shift_left >= 64) {
                        runtime_error(interp, "Right operand for '<<' must be in the range [0, 63].");
                        return ERROR_RESULT();
                    }
                    // To avoid C's undefined behavior for left-shifting a negative number,
                    // we cast to unsigned, perform the shift, and cast back. This
                    // performs a well-defined logical shift on the bit pattern, which is
                    // the desired behavior in most high-level scripting languages.
                    uint64_t u_left = (uint64_t)l_shift_left;
                    uint64_t result = u_left << r_shift_left;
                    return OK_RESULT(make_int((int64_t)result));

                case T_GREATER_GREATER:
                    if (!IS_INT(left) || !IS_INT(right)) {
                        runtime_error(interp, "Operands for '>>' must be integers."); return ERROR_RESULT();
                    }
                    int64_t r_shift_right = AS_INT(right);
                     if (r_shift_right < 0 || r_shift_right >= 64) {
                        runtime_error(interp, "Right operand for '>>' must be in the range [0, 63].");
                        return ERROR_RESULT();
                    }
                    return OK_RESULT(make_int(AS_INT(left) >> r_shift_right));

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
            // The result of an expression statement is discarded, so we return nil.
            // This prevents values from printing implicitly in the REPL.
            // If we wanted implicit printing, we would return res.value here.
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
                case OBJ_RESOURCE: {
                    ObjResource* res = AS_RESOURCE(value);
                    const char* status = res->is_finalized ? " (closed)" : "";
                    printf("<resource %s%s>", res->vtable->type_name, status);
                    break;
                }
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


static Value builtin_typeof(Interpreter *interp, size_t argc, Value* args) {
    if (argc != 1) {
        runtime_error(interp, "typeof() expects 1 argument, but got %zu.", argc);
        return make_nil();
    }
    const char* type_str;
    switch(args[0].type) {
        case VAL_BOOL:    type_str = "boolean"; break;
        case VAL_NIL:     type_str = "nil"; break;
        case VAL_INT:
        case VAL_DOUBLE:  type_str = "number"; break;
        case VAL_BUILTIN: type_str = "builtin"; break;
        case VAL_OBJ:
            switch(OBJ_TYPE(args[0])) {
                case OBJ_STRING:   type_str = "string"; break;
                case OBJ_FUNCTION: type_str = "function"; break;
                case OBJ_ARRAY:    type_str = "array"; break;
                case OBJ_OBJECT:   type_str = "object"; break;
                case OBJ_ENV:      type_str = "environment"; break;
                case OBJ_RESOURCE: type_str = "resource"; break;
                default:           type_str = "unknown_object"; break;
            }
            break;
        default: type_str = "unknown"; break;
    }
    return make_obj((Obj*)make_heap_string(interp, type_str, strlen(type_str)));
}

static Value builtin_clock(Interpreter *interp, size_t argc, Value* args) {
    if (argc != 0) {
        runtime_error(interp, "clock() expects 0 arguments, but got %zu.", argc);
        return make_nil();
    }
    return make_double((double)clock() / CLOCKS_PER_SEC);
}

static Value builtin_time(Interpreter *interp, size_t argc, Value* args) {
    if (argc != 0) {
        runtime_error(interp, "time() expects 0 arguments, but got %zu.", argc);
        return make_nil();
    }
    time_t now = time(NULL);
    if (now == (time_t)-1) {
        runtime_error(interp, "Failed to get current time.");
        return make_nil();
    }
    return make_int((int64_t)now);
}

static Value builtin_exit(Interpreter *interp, size_t argc, Value* args) {
    if (argc > 1) {
        runtime_error(interp, "exit() expects 0 or 1 argument, but got %zu.", argc);
        return make_nil();
    }
    int code = 0;
    if (argc == 1) {
        if (!IS_INT(args[0])) {
            runtime_error(interp, "exit() argument must be an integer exit code.");
            return make_nil();
        }
        code = (int)AS_INT(args[0]);
    }
    exit(code);
    return make_nil(); // Unreachable, but pleases the compiler.
}

static Value builtin_assert(Interpreter *interp, size_t argc, Value* args) {
    if (argc < 1 || argc > 2) {
        runtime_error(interp, "assert() expects 1 or 2 arguments, but got %zu.", argc);
        return make_nil();
    }
    if (!is_truthy(args[0])) {
        const char* message = "Assertion failed.";
        if (argc == 2 && IS_STRING(args[1])) {
            message = AS_CSTRING(args[1]);
        }
        runtime_error(interp, message);
        return make_nil();
    }
    return make_nil();
}

// --- Core Implementations ---

static Value builtin_range(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 2 || argc > 3) {
        runtime_error(interp, "range() expects 2 or 3 arguments, but got %zu.", argc);
        return make_nil();
    }
    if (!IS_INT(args[0]) || !IS_INT(args[1])) {
        runtime_error(interp, "range() start and end arguments must be integers.");
        return make_nil();
    }

    int64_t start = AS_INT(args[0]);
    int64_t end = AS_INT(args[1]);
    int64_t step = 1;

    if (argc == 3) {
        if (!IS_INT(args[2])) {
            runtime_error(interp, "range() step argument must be an integer.");
            return make_nil();
        }
        step = AS_INT(args[2]);
    }

    if (step == 0) {
        runtime_error(interp, "range() step argument cannot be zero.");
        return make_nil();
    }

    ObjArray* array = new_array(interp);
    push_root(interp, (Obj*)array);

    if (step > 0) {
        for (int64_t i = start; i < end; i += step) {
            array_write(interp, array, make_int(i));
        }
    } else {
        for (int64_t i = start; i > end; i += step) {
            array_write(interp, array, make_int(i));
        }
    }

    pop_root(interp);
    return make_obj((Obj*)array);
}

static Value min_max_helper(Interpreter* interp, size_t argc, Value* args, bool is_max) {
    if (argc == 0) {
        runtime_error(interp, "%s() requires at least one argument.", is_max ? "max" : "min");
        return make_nil();
    }

    Value result_val = args[0];
    if (!IS_NUMERIC(result_val)) {
        runtime_error(interp, "%s() arguments must be numbers.", is_max ? "max" : "min");
        return make_nil();
    }

    for (size_t i = 1; i < argc; i++) {
        if (!IS_NUMERIC(args[i])) {
            runtime_error(interp, "%s() arguments must be numbers.", is_max ? "max" : "min");
            return make_nil();
        }
        double current_best = AS_NUMBER(result_val);
        double candidate = AS_NUMBER(args[i]);
        bool found_better = is_max ? (candidate > current_best) : (candidate < current_best);
        if (found_better) {
            result_val = args[i];
        }
    }
    return result_val;
}

static Value builtin_min(Interpreter* interp, size_t argc, Value* args) {
    return min_max_helper(interp, argc, args, false);
}

static Value builtin_max(Interpreter* interp, size_t argc, Value* args) {
    return min_max_helper(interp, argc, args, true);
}

static Value builtin_round(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 1 || argc > 2) {
        runtime_error(interp, "round() expects 1 or 2 arguments, but got %zu.", argc);
        return make_nil();
    }
    if (!IS_NUMERIC(args[0])) {
        runtime_error(interp, "round() first argument must be a number.");
        return make_nil();
    }
    
    double num = AS_NUMBER(args[0]);
    int64_t digits = 0;
    if (argc == 2) {
        if (!IS_INT(args[1])) {
            runtime_error(interp, "round() second argument (digits) must be an integer.");
            return make_nil();
        }
        digits = AS_INT(args[1]);
    }
    
    if (digits == 0) {
        return make_double(round(num));
    }
    
    double factor = pow(10.0, (double) digits);
    return make_double(round(num * factor) / factor);
}

static Value builtin_sleep(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1) {
        runtime_error(interp, "sleep() expects 1 argument (milliseconds), but got %zu.", argc);
        return make_nil();
    }
    if (!IS_INT(args[0])) {
        runtime_error(interp, "sleep() argument must be an integer.");
        return make_nil();
    }
    int64_t ms = AS_INT(args[0]);
    if (ms < 0) {
        runtime_error(interp, "sleep() argument cannot be negative.");
        return make_nil();
    }

#ifdef _WIN32
    // Windows-specific sleep implementation
    Sleep((DWORD)ms);
#else
    // POSIX-compliant sleep implementation
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000;
    
    nanosleep(&ts, NULL);
#endif
    
    return make_nil();
}

// --- String Object Implementations ---

static Value builtin_string_replace(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 3 || argc > 4 || !IS_STRING(args[0]) || !IS_STRING(args[1]) || !IS_STRING(args[2])) {
        runtime_error(interp, "string.replace(s, old, new, [count]) requires string arguments.");
        return make_nil();
    }
    
    ObjString* s = AS_STRING(args[0]);
    ObjString* old = AS_STRING(args[1]);
    ObjString* new = AS_STRING(args[2]);
    long long count = -1;
    if (argc == 4) {
        if (!IS_INT(args[3])) {
            runtime_error(interp, "string.replace() count must be an integer.");
            return make_nil();
        }
        count = AS_INT(args[3]);
    }

    if (old->length == 0) { // Can't replace empty string
        return args[0];
    }

    // Pass 1: Count occurrences and calculate final length
    size_t result_len = s->length;
    long long replacements = 0;
    const char* p = s->chars;
    while ((p = strstr(p, old->chars)) != NULL) {
        if (count != -1 && replacements >= count) break;
        result_len += new->length - old->length;
        replacements++;
        p += old->length;
    }

    if (replacements == 0) {
        return args[0]; // No replacements needed, return original string
    }

    // Pass 2: Allocate and build the new string
    char* result_chars = PRATT_MALLOC(result_len + 1);
    if (!result_chars) {
        runtime_error(interp, "Out of memory for string replacement.");
        return make_nil();
    }
    
    char* dest = result_chars;
    const char* start = s->chars;
    p = s->chars;
    replacements = 0;
    while ((p = strstr(start, old->chars)) != NULL) {
        if (count != -1 && replacements >= count) break;
        
        size_t segment_len = p - start;
        memcpy(dest, start, segment_len);
        dest += segment_len;

        memcpy(dest, new->chars, new->length);
        dest += new->length;
        
        start = p + old->length;
        replacements++;
    }
    strcpy(dest, start); // Copy the rest of the string
    
    ObjString* result_str = make_heap_string(interp, result_chars, result_len);
    PRATT_FREE(result_chars);
    return make_obj((Obj*)result_str);
}

static Value builtin_string_startsWith(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "string.startsWith(s, prefix) requires two string arguments.");
        return make_nil();
    }
    ObjString* s = AS_STRING(args[0]);
    ObjString* prefix = AS_STRING(args[1]);
    if (s->length < prefix->length) {
        return make_bool(false);
    }
    return make_bool(strncmp(s->chars, prefix->chars, prefix->length) == 0);
}

static Value builtin_string_endsWith(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "string.endsWith(s, suffix) requires two string arguments.");
        return make_nil();
    }
    ObjString* s = AS_STRING(args[0]);
    ObjString* suffix = AS_STRING(args[1]);
    if (s->length < suffix->length) {
        return make_bool(false);
    }
    const char* end_of_s = s->chars + s->length - suffix->length;
    return make_bool(memcmp(end_of_s, suffix->chars, suffix->length) == 0);
}

static Value builtin_string_indexOf(Interpreter* interp, size_t argc, Value* args) {
    if ((argc < 2 || argc > 3) || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "string.indexOf(s, needle, [from]) requires string arguments.");
        return make_nil();
    }
    ObjString* s = AS_STRING(args[0]);
    ObjString* needle = AS_STRING(args[1]);
    int64_t from = 0;
    if (argc == 3) {
        if (!IS_INT(args[2])) {
            runtime_error(interp, "string.indexOf() from index must be an integer.");
            return make_nil();
        }
        from = AS_INT(args[2]);
    }
    
    if (from < 0) from = 0;
    if ((size_t)from >= s->length) return make_int(-1);

    const char* found = strstr(s->chars + from, needle->chars);
    if (found == NULL) {
        return make_int(-1);
    }
    return make_int(found - s->chars);
}

static Value builtin_string_substring(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 3 || !IS_STRING(args[0]) || !IS_INT(args[1]) || !IS_INT(args[2])) {
        runtime_error(interp, "string.substring(s, start, end) requires a string and two integers.");
        return make_nil();
    }
    ObjString* s = AS_STRING(args[0]);
    int64_t start = AS_INT(args[1]);
    int64_t end = AS_INT(args[2]);

    // Clamp to bounds
    if (start < 0) start = 0;
    if (end > (int64_t)s->length) end = s->length;
    if (start > (int64_t)s->length) start = s->length;
    if (end < start) end = start;

    size_t new_len = end - start;
    return make_obj((Obj*)make_heap_string(interp, s->chars + start, new_len));
}

static Value builtin_string_repeat(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_INT(args[1])) {
        runtime_error(interp, "string.repeat(s, n) requires a string and an integer.");
        return make_nil();
    }
    ObjString* s = AS_STRING(args[0]);
    int64_t n = AS_INT(args[1]);
    if (n < 0) {
        runtime_error(interp, "string.repeat() count cannot be negative.");
        return make_nil();
    }
    if (n == 0 || s->length == 0) {
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }
    
    size_t new_len = s->length * n;
    // Basic overflow check
    if (n > 0 && new_len / n != s->length) {
        runtime_error(interp, "string.repeat() result is too large.");
        return make_nil();
    }

    char* result_chars = PRATT_MALLOC(new_len + 1);
    if (!result_chars) {
        runtime_error(interp, "Out of memory for string repeat.");
        return make_nil();
    }
    
    char* p = result_chars;
    for (int64_t i = 0; i < n; i++) {
        memcpy(p, s->chars, s->length);
        p += s->length;
    }
    *p = '\0';
    
    ObjString* result_str = make_heap_string(interp, result_chars, new_len);
    PRATT_FREE(result_chars);
    return make_obj((Obj*)result_str);
}

static Value pad_helper(Interpreter* interp, size_t argc, Value* args, bool is_start) {
    if ((argc < 2 || argc > 3) || !IS_STRING(args[0]) || !IS_INT(args[1])) {
        runtime_error(interp, "pad function requires a string and an integer length.");
        return make_nil();
    }
    ObjString* s = AS_STRING(args[0]);
    int64_t target_len = AS_INT(args[1]);
    
    if ((int64_t)s->length >= target_len) {
        return args[0];
    }

    ObjString* pad_str;
    if (argc == 3) {
        if (!IS_STRING(args[2])) {
            runtime_error(interp, "pad function pad string must be a string.");
            return make_nil();
        }
        pad_str = AS_STRING(args[2]);
        if (pad_str->length == 0) return args[0]; // Can't pad with empty string
    } else {
        pad_str = make_heap_string(interp, " ", 1); // Default is space
        push_root(interp, (Obj*)pad_str); // Protect default pad string
    }

    size_t pad_needed = target_len - s->length;
    char* result_chars = PRATT_MALLOC(target_len + 1);
    if (!result_chars) {
        if (argc < 3) pop_root(interp);
        runtime_error(interp, "Out of memory for string padding.");
        return make_nil();
    }

    char* p = result_chars;
    if (is_start) {
        for (size_t i = 0; i < pad_needed; i++) {
            *p++ = pad_str->chars[i % pad_str->length];
        }
        memcpy(p, s->chars, s->length);
    } else {
        memcpy(p, s->chars, s->length);
        p += s->length;
        for (size_t i = 0; i < pad_needed; i++) {
            *p++ = pad_str->chars[i % pad_str->length];
        }
    }
    result_chars[target_len] = '\0';

    if (argc < 3) pop_root(interp); // Pop default pad string if it was created
    ObjString* result_str = make_heap_string(interp, result_chars, target_len);
    PRATT_FREE(result_chars);
    return make_obj((Obj*)result_str);
}

static Value builtin_string_padStart(Interpreter* interp, size_t argc, Value* args) {
    return pad_helper(interp, argc, args, true);
}

static Value builtin_string_padEnd(Interpreter* interp, size_t argc, Value* args) {
    return pad_helper(interp, argc, args, false);
}


// --- Array Object Implementations ---

static Value builtin_array_map(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_ARRAY(args[0]) || (!IS_FUNCTION(args[1]) && !IS_BUILTIN(args[1]))) {
        runtime_error(interp, "array.map(arr, fn) requires an array and a function.");
        return make_nil();
    }
    ObjArray* src = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    ObjArray* result_array = new_array(interp);
    push_root(interp, (Obj*)result_array); // Protect result
    if (IS_OBJ(fn)) push_root(interp, AS_OBJ(fn)); // Protect callback

    for (int i = 0; i < src->count; i++) {
        Value elem = src->values[i];
        if (IS_OBJ(elem)) push_root(interp, AS_OBJ(elem)); // Protect element
        
        ExecResult map_res = call_value(interp, fn, 1, &elem);
        
        if (IS_OBJ(elem)) pop_root(interp); // Un-root element

        if (map_res.status != EXEC_OK || interp->had_error) {
            if (IS_OBJ(fn)) pop_root(interp);
            pop_root(interp);
            return make_nil(); // Error already set
        }
        array_write(interp, result_array, map_res.value);
    }

    if (IS_OBJ(fn)) pop_root(interp);
    pop_root(interp);
    return make_obj((Obj*)result_array);
}

static Value builtin_array_filter(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_ARRAY(args[0]) || (!IS_FUNCTION(args[1]) && !IS_BUILTIN(args[1]))) {
        runtime_error(interp, "array.filter(arr, fn) requires an array and a function.");
        return make_nil();
    }
    ObjArray* src = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    ObjArray* result_array = new_array(interp);
    push_root(interp, (Obj*)result_array); // Protect result
    if (IS_OBJ(fn)) push_root(interp, AS_OBJ(fn)); // Protect callback

    for (int i = 0; i < src->count; i++) {
        Value elem = src->values[i];
        if (IS_OBJ(elem)) push_root(interp, AS_OBJ(elem)); // Protect element
        
        ExecResult filter_res = call_value(interp, fn, 1, &elem);
        
        if (IS_OBJ(elem)) pop_root(interp); // Un-root element

        if (filter_res.status != EXEC_OK || interp->had_error) {
            if (IS_OBJ(fn)) pop_root(interp);
            pop_root(interp);
            return make_nil(); // Error already set
        }
        if (is_truthy(filter_res.value)) {
            array_write(interp, result_array, elem);
        }
    }

    if (IS_OBJ(fn)) pop_root(interp);
    pop_root(interp);
    return make_obj((Obj*)result_array);
}

static Value builtin_array_reduce(Interpreter* interp, size_t argc, Value* args) {
    if ((argc < 2 || argc > 3) || !IS_ARRAY(args[0]) || (!IS_FUNCTION(args[1]) && !IS_BUILTIN(args[1]))) {
        runtime_error(interp, "array.reduce(arr, fn, [init]) requires an array and a function.");
        return make_nil();
    }
    ObjArray* src = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    Value accumulator;
    int start_index = 0;
    
    if (argc == 3) {
        accumulator = args[2];
    } else {
        if (src->count == 0) {
            runtime_error(interp, "reduce of empty array with no initial value.");
            return make_nil();
        }
        accumulator = src->values[0];
        start_index = 1;
    }

    if (IS_OBJ(fn)) push_root(interp, AS_OBJ(fn)); // Protect callback

    for (int i = start_index; i < src->count; i++) {
        Value current_val = src->values[i];
        Value call_args[2] = { accumulator, current_val };
        
        if (IS_OBJ(accumulator)) push_root(interp, AS_OBJ(accumulator));
        if (IS_OBJ(current_val)) push_root(interp, AS_OBJ(current_val));
        
        ExecResult reduce_res = call_value(interp, fn, 2, call_args);

        if (IS_OBJ(current_val)) pop_root(interp);
        if (IS_OBJ(accumulator)) pop_root(interp);
        
        if (reduce_res.status != EXEC_OK || interp->had_error) {
            if (IS_OBJ(fn)) pop_root(interp);
            return make_nil();
        }
        accumulator = reduce_res.value;
    }
    
    if (IS_OBJ(fn)) pop_root(interp);
    return accumulator;
}

static Value builtin_array_indexOf(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_ARRAY(args[0])) {
        runtime_error(interp, "array.indexOf(arr, value) requires an array and a value.");
        return make_nil();
    }
    ObjArray* arr = AS_ARRAY(args[0]);
    Value needle = args[1];
    for (int i = 0; i < arr->count; i++) {
        if (values_are_equal(arr->values[i], needle)) {
            return make_int(i);
        }
    }
    return make_int(-1);
}

static Value builtin_array_includes(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_ARRAY(args[0])) {
        runtime_error(interp, "array.includes(arr, value) requires an array and a value.");
        return make_nil();
    }
    ObjArray* arr = AS_ARRAY(args[0]);
    Value needle = args[1];
    for (int i = 0; i < arr->count; i++) {
        if (values_are_equal(arr->values[i], needle)) {
            return make_bool(true);
        }
    }
    return make_bool(false);
}

// Context for qsort comparator
static struct {
    Interpreter* interp;
    Value callback_fn;
} sort_context;

static int sort_compare_wrapper(const void* a, const void* b) {
    Interpreter* interp = sort_context.interp;
    Value val_a = *(const Value*)a;
    Value val_b = *(const Value*)b;
    
    if (interp->had_error) return 0; // Stop sorting if an error occurred

    // Case 1: Use user-provided callback function
    if (!IS_NIL(sort_context.callback_fn)) {
        Value args[2] = { val_a, val_b };
        ExecResult res = call_value(interp, sort_context.callback_fn, 2, args);
        if (res.status != EXEC_OK || interp->had_error) {
            // Propagate error. had_error is set by runtime_error.
            return 0;
        }
        if (!IS_NUMERIC(res.value)) {
            runtime_error(interp, "Sort comparison function must return a number.");
            return 0;
        }
        double d = AS_NUMBER(res.value);
        if (d < 0) return -1;
        if (d > 0) return 1;
        return 0;
    }

    // Case 2: Default comparison
    if (IS_NUMERIC(val_a) && IS_NUMERIC(val_b)) {
        double diff = AS_NUMBER(val_a) - AS_NUMBER(val_b);
        if (diff < 0) return -1;
        if (diff > 0) return 1;
        return 0;
    }

    // Default: Lexicographic (string-based) comparison
    ObjString* s_a = value_to_string(interp, val_a);
    push_root(interp, (Obj*)s_a);
    ObjString* s_b = value_to_string(interp, val_b);
    pop_root(interp);

    int cmp = strcmp(s_a->chars, s_b->chars);
    return cmp;
}

static Value builtin_array_sort(Interpreter* interp, size_t argc, Value* args) {
    if ((argc < 1 || argc > 2) || !IS_ARRAY(args[0])) {
        runtime_error(interp, "array.sort(arr, [fn]) requires an array.");
        return make_nil();
    }
    
    ObjArray* array = AS_ARRAY(args[0]);
    Value callback = make_nil();
    if (argc == 2) {
        if (!IS_FUNCTION(args[1]) && !IS_BUILTIN(args[1]) && !IS_NIL(args[1])) {
             runtime_error(interp, "Sort comparison must be a function or nil.");
             return make_nil();
        }
        callback = args[1];
    }

    sort_context.interp = interp;
    sort_context.callback_fn = callback;

    if (IS_OBJ(callback)) {
        push_root(interp, AS_OBJ(callback));
    }
    
    // The array itself is an argument to the builtin, so it is on the value stack
    // and considered rooted. Thus, we don't need to explicitly root it here.
    qsort(array->values, array->count, sizeof(Value), sort_compare_wrapper);
    
    if (IS_OBJ(callback)) {
        pop_root(interp);
    }
    
    // Check if the comparator set an error
    if (interp->had_error) {
        return make_nil();
    }

    return args[0]; // Return the sorted (in-place) array
}

static Value builtin_array_reverse(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_ARRAY(args[0])) {
        runtime_error(interp, "array.reverse(arr) requires an array.");
        return make_nil();
    }
    ObjArray* array = AS_ARRAY(args[0]);
    if (array->count < 2) return args[0];

    int start = 0;
    int end = array->count - 1;
    while (start < end) {
        Value temp = array->values[start];
        array->values[start] = array->values[end];
        array->values[end] = temp;
        start++;
        end--;
    }
    return args[0];
}

static Value builtin_array_shuffle(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_ARRAY(args[0])) {
        runtime_error(interp, "array.shuffle(arr) requires an array.");
        return make_nil();
    }
    ObjArray* array = AS_ARRAY(args[0]);
    if (array->count < 2) return args[0];
    
    // Fisher-Yates shuffle
    for (int i = array->count - 1; i > 0; i--) {
        int j = (int) (sfc64_next(&interp->rng) % (i + 1));
        Value temp = array->values[i];
        array->values[i] = array->values[j];
        array->values[j] = temp;
    }
    return args[0];
}

static Value builtin_math_abs(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.abs() expects 1 number argument.");
        return make_nil();
    }
    if (IS_INT(args[0])) return make_int(llabs(AS_INT(args[0])));
    return make_double(fabs(AS_DOUBLE(args[0])));
}

static Value builtin_math_floor(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.floor() expects 1 number argument.");
        return make_nil();
    }
    return make_double(floor(AS_NUMBER(args[0])));
}

static Value builtin_math_ceil(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.ceil() expects 1 number argument.");
        return make_nil();
    }
    return make_double(ceil(AS_NUMBER(args[0])));
}

static Value builtin_math_pow(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_NUMERIC(args[0]) || !IS_NUMERIC(args[1])) {
        runtime_error(interp, "math.pow() expects 2 number arguments.");
        return make_nil();
    }
    return make_double(pow(AS_NUMBER(args[0]), AS_NUMBER(args[1])));
}

static Value builtin_math_sqrt(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.sqrt() expects 1 number argument.");
        return make_nil();
    }
    double value = AS_NUMBER(args[0]);
    if (value < 0) {
        runtime_error(interp, "math.sqrt() cannot take negative numbers.");
        return make_nil();
    }
    return make_double(sqrt(value));
}

static Value builtin_math_sin(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.sin() expects 1 number argument.");
        return make_nil();
    }
    return make_double(sin(AS_NUMBER(args[0])));
}

static Value builtin_math_cos(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.cos() expects 1 number argument.");
        return make_nil();
    }
    return make_double(cos(AS_NUMBER(args[0])));
}

static Value builtin_math_tan(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.tan() expects 1 number argument.");
        return make_nil();
    }
    return make_double(tan(AS_NUMBER(args[0])));
}

static Value builtin_math_log(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.log() expects 1 number argument.");
        return make_nil();
    }
    double value = AS_NUMBER(args[0]);
    if (value <= 0) {
        runtime_error(interp, "math.log() cannot take non-positive numbers.");
        return make_nil();
    }
    return make_double(log(value));
}

static Value builtin_math_log10(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_NUMERIC(args[0])) {
        runtime_error(interp, "math.log10() expects 1 number argument.");
        return make_nil();
    }
    double value = AS_NUMBER(args[0]);
    if (value <= 0) {
        runtime_error(interp, "math.log10() cannot take non-positive numbers.");
        return make_nil();
    }
    return make_double(log10(value));
}

static Value builtin_math_random(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 0) {
        runtime_error(interp, "math.random() expects 0 arguments, but got %zu.", argc);
        return make_nil();
    }
    // Pull one uniform [0,1) double from SFC64
    double r = sfc64_double(&interp->rng);
    return make_double(r);
}

static Value builtin_math_random_seed(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_INT(args[0])) {
        runtime_error(interp, "math.seedrandom(seed) expects a single integer argument.");
        return make_nil();
    }
    uint64_t seed = (uint64_t) AS_INT(args[0]);
    sfc64_init(&interp->rng, seed);
    return make_nil();
}

static Value builtin_math_random_int(Interpreter* interp, size_t argc, Value* args) {
    if (argc == 0) {
        // No arguments: return a random 64-bit integer
        int64_t random_value = (int64_t)sfc64_next(&interp->rng);
        return make_int(random_value);
    }
    if (argc == 2 && IS_INT(args[0]) && IS_INT(args[1])) {
        int64_t min = AS_INT(args[0]);
        int64_t max = AS_INT(args[1]);
        if (min >= max) {
            runtime_error(interp, "math.randomInt() requires min < max.");
            return make_nil();
        }
        // Generate a random integer in the range [min, max)
        int64_t range = max - min;
        int64_t random_value = sfc64_next(&interp->rng) % range + min;
        return make_int(random_value);
    }
    runtime_error(interp, "math.randomInt() expects either no arguments or two integer arguments.");
    return make_nil();
}
static Value builtin_math_random_bytes(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_INT(args[0])) {
        runtime_error(interp, "math.randomBytes(len) expects a single integer argument.");
        return make_nil();
    }
    int64_t len = AS_INT(args[0]);
    if (len < 0) {
        runtime_error(interp, "math.randomBytes(len): length cannot be negative.");
        return make_nil();
    }
    if (len == 0) {
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }

    char* buffer = PRATT_MALLOC((size_t)len);
    if (!buffer) {
        runtime_error(interp, "Out of memory in randomBytes.");
        return make_nil();
    }

    int64_t full_chunks = len / 8;
    int64_t remaining = len % 8;
    int64_t offset = 0;

    for (int64_t i = 0; i < full_chunks; i++) {
        uint64_t rnd = sfc64_next(&interp->rng);
        memcpy(buffer + offset, &rnd, 8);
        offset += 8;
    }
    if (remaining > 0) {
        uint64_t rnd = sfc64_next(&interp->rng);
        memcpy(buffer + offset, &rnd, remaining);
    }

    ObjString* result = make_heap_string(interp, buffer, (size_t)len);
    PRATT_FREE(buffer);
    return make_obj((Obj*)result);
}

static Value builtin_string_split(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "string.split() expects a string to split and a separator string.");
        return make_nil();
    }
    ObjString* str = AS_STRING(args[0]);
    ObjString* sep = AS_STRING(args[1]);

    ObjArray* result_array = new_array(interp);
    push_root(interp, (Obj*)result_array); // Protect array from GC

    if (sep->length == 0) { // Split into characters
        for (size_t i = 0; i < str->length; i++) {
            array_write(interp, result_array, make_obj((Obj*)make_heap_string(interp, &str->chars[i], 1)));
        }
    } else {
        const char* start = str->chars;
        const char* found;
        while ((found = strstr(start, sep->chars)) != NULL) {
            array_write(interp, result_array, make_obj((Obj*)make_heap_string(interp, start, found - start)));
            start = found + sep->length;
        }
        array_write(interp, result_array, make_obj((Obj*)make_heap_string(interp, start, strlen(start))));
    }
    
    pop_root(interp);
    return make_obj((Obj*)result_array);
}

static Value builtin_string_trim(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "string.trim() expects 1 string argument.");
        return make_nil();
    }
    ObjString* str = AS_STRING(args[0]);
    
    const char* start = str->chars;
    while (*start && isspace((unsigned char)*start)) {
        start++;
    }

    if (*start == '\0') { // String is all whitespace
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }

    const char* end = str->chars + str->length - 1;
    while (end > start && isspace((unsigned char)*end)) {
        end--;
    }
    
    size_t new_len = end - start + 1;
    return make_obj((Obj*)make_heap_string(interp, start, new_len));
}

static Value builtin_array_slice(Interpreter* interp, size_t argc, Value* args) {
    if ((argc != 2 && argc != 3) || !IS_ARRAY(args[0]) || !IS_INT(args[1]) || (argc == 3 && !IS_INT(args[2]))) {
        runtime_error(interp, "array.slice(array, start, [end]) expects an array and integer indices.");
        return make_nil();
    }
    ObjArray* src_array = AS_ARRAY(args[0]);
    int64_t start = AS_INT(args[1]);
    int64_t end = src_array->count;
    if (argc == 3) {
        end = AS_INT(args[2]);
    }

    // Handle negative indices
    if (start < 0) start = src_array->count + start;
    if (end < 0) end = src_array->count + end;

    // Clamp to bounds
    if (start < 0) start = 0;
    if (end > src_array->count) end = src_array->count;
    if (start > src_array->count) start = src_array->count;

    ObjArray* result_array = new_array(interp);
    push_root(interp, (Obj*)result_array);

    if (start < end) {
        for (int64_t i = start; i < end; i++) {
            array_write(interp, result_array, src_array->values[i]);
        }
    }
    
    pop_root(interp);
    return make_obj((Obj*)result_array);
}

static Value builtin_array_join(Interpreter* interp, size_t argc, Value* args) {
    if ((argc != 1 && argc != 2) || !IS_ARRAY(args[0]) || (argc == 2 && !IS_STRING(args[1]))) {
        runtime_error(interp, "array.join(array, [separator]) expects an array and an optional string separator.");
        return make_nil();
    }
    ObjArray* array = AS_ARRAY(args[0]);
    ObjString* separator = (argc == 2) ? AS_STRING(args[1]) : NULL;
    size_t sep_len = (separator != NULL) ? separator->length : 0;
    
    if (array->count == 0) {
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }
    
    // --- Phase 1: Convert all elements to strings and calculate total length ---
    // We must protect these new strings from GC. A temporary C array holds them.
    ObjString** strings = PRATT_MALLOC(sizeof(ObjString*) * array->count);
    if (!strings) { runtime_error(interp, "Out of memory."); return make_nil(); }
    
    size_t total_len = 0;
    for (int i = 0; i < array->count; i++) {
        // value_to_string might trigger GC, so we must protect all previously created strings.
        // The safest way is to root each as it's created.
        if (IS_OBJ(array->values[i])) push_root(interp, AS_OBJ(array->values[i]));
        strings[i] = value_to_string(interp, array->values[i]);
        if (IS_OBJ(array->values[i])) pop_root(interp);

        push_root(interp, (Obj*)strings[i]); // Protect this new string
        total_len += strings[i]->length;
    }
    if (array->count > 1 && separator != NULL) {
        total_len += sep_len * (array->count - 1);
    }
    
    // --- Phase 2: Build the final string ---
    char* result_chars = PRATT_MALLOC(total_len + 1);
    if (!result_chars) {
        for(int i=0; i < array->count; i++) pop_root(interp); // Cleanup roots
        PRATT_FREE(strings);
        runtime_error(interp, "Out of memory.");
        return make_nil();
    }
    
    char* p = result_chars;
    for (int i = 0; i < array->count; i++) {
        memcpy(p, strings[i]->chars, strings[i]->length);
        p += strings[i]->length;
        if (separator != NULL && i < array->count - 1) {
            memcpy(p, separator->chars, sep_len);
            p += sep_len;
        }
    }
    *p = '\0';
    
    // --- Phase 3: Clean up and return ---
    for(int i=0; i < array->count; i++) pop_root(interp); // Pop all the strings we rooted
    PRATT_FREE(strings);
    
    ObjString* result_str = make_heap_string(interp, result_chars, total_len);
    PRATT_FREE(result_chars);
    return make_obj((Obj*)result_str);
}


// --- FS Object ---

static Value builtin_fs_readFile(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 1 || argc > 2 || !IS_STRING(args[0])) {
        runtime_error(interp, "fs.readFile(path, [options]) expects a string path.");
        return make_nil();
    }

    // --- Default options ---
    const char* mode_str = "text";

    // --- Parse options ---
    if (argc == 2) {
        Value options_val = args[1];
        if (IS_OBJECT(options_val)) {
            ObjObject* options_obj = AS_OBJECT(options_val);
            Value opt_val;

            // Check for 'mode' property: "text" or "binary"
            ObjString* mode_key = interpreter_intern_string(interp, "mode", 4);
            if (map_get(&options_obj->map, mode_key, &opt_val) && IS_STRING(opt_val)) {
                mode_str = AS_CSTRING(opt_val);
            }
        } else if (IS_STRING(options_val)) {
            // second argument is an encoding string
            mode_str = AS_CSTRING(options_val);
        } else if (!IS_NIL(options_val)) {
            runtime_error(interp, "fs.readFile() second argument must be an options object, a string, or nil.");
            return make_nil();
        }
    }

    // --- Validate options and determine fopen mode ---
    bool is_binary;
    if (strcmp(mode_str, "binary") == 0) {
        is_binary = true;
    } else if (strcmp(mode_str, "text") == 0) {
        is_binary = false;
    } else {
        runtime_error(interp, "Invalid mode '%s'. Must be 'text' or 'binary'.", mode_str);
        return make_nil();
    }
    
    const char* fopen_mode = is_binary ? "rb" : "r";
    const char* path = AS_CSTRING(args[0]);

    struct stat stat_buffer;
    if (stat(path, &stat_buffer) != 0) {
        runtime_error(interp, "Could not open file '%s': %s", path, strerror(errno));
        return make_nil();
    }
    
    // --- Read file ---
    FILE* file = fopen(path, fopen_mode);
    if (file == NULL) {
        runtime_error(interp, "Could not open file '%s': %s", path, strerror(errno));
        return make_nil();
    }

    long file_size = stat_buffer.st_size;

    char* buffer = PRATT_MALLOC(file_size);
    if (buffer == NULL) {
        fclose(file);
        runtime_error(interp, "Not enough memory to read file '%s'", path);
        return make_nil();
    }
    
    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read < (size_t)file_size && ferror(file)) {
        PRATT_FREE(buffer);
        fclose(file);
        runtime_error(interp, "Could not read entire file '%s'", path);
        return make_nil();
    }
    
    fclose(file);

    ObjString* result = make_heap_string(interp, buffer, bytes_read);
    PRATT_FREE(buffer);
    
    return make_obj((Obj*)result);
}

static Value builtin_fs_writeFile(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 2 || argc > 3 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "fs.writeFile(path, data, [options]) expects string path and data.");
        return make_nil();
    }
    
    // --- Default options ---
    const char* mode_str = "text";
    bool append = false;

    // --- Parse options ---
    if (argc == 3) {
        Value options_val = args[2];
        if (IS_OBJECT(options_val)) {
            ObjObject* options_obj = AS_OBJECT(options_val);
            Value opt_val;
            
            // Check for 'mode'
            ObjString* mode_key = interpreter_intern_string(interp, "mode", 4);
            if (map_get(&options_obj->map, mode_key, &opt_val) && IS_STRING(opt_val)) {
                mode_str = AS_CSTRING(opt_val);
            }
            
            // Check for 'append'
            ObjString* append_key = interpreter_intern_string(interp, "append", 6);
            if (map_get(&options_obj->map, append_key, &opt_val) && IS_BOOL(opt_val)) {
                append = AS_BOOL(opt_val);
            }

        } else if (IS_BOOL(options_val)) {
            // third argument is a boolean for append
            append = AS_BOOL(options_val);
        } else if (!IS_NIL(options_val)) {
            runtime_error(interp, "fs.writeFile() third argument must be an options object, a boolean, or nil.");
            return make_nil();
        }
    }
    
    // --- Validate options and determine fopen mode ---
    bool is_binary;
    if (strcmp(mode_str, "binary") == 0) {
        is_binary = true;
    } else if (strcmp(mode_str, "text") == 0) {
        is_binary = false;
    } else {
        runtime_error(interp, "Invalid mode '%s'. Must be 'text' or 'binary'.", mode_str);
        return make_nil();
    }
    
    const char* fopen_mode;
    if (is_binary) {
        fopen_mode = append ? "ab" : "wb";
    } else {
        fopen_mode = append ? "a" : "w";
    }

    const char* path = AS_CSTRING(args[0]);
    ObjString* data = AS_STRING(args[1]);

    // --- Write to file ---
    FILE* file = fopen(path, fopen_mode);
    if (file == NULL) {
        runtime_error(interp, "Could not open file '%s' for writing: %s", path, strerror(errno));
        return make_bool(false);
    }
    
    size_t bytes_written = fwrite(data->chars, 1, data->length, file);
    fclose(file);
    
    if (bytes_written < data->length) {
        runtime_error(interp, "Could not write all data to file '%s'", path);
        return make_bool(false);
    }
    
    return make_bool(true);
}

static Value builtin_fs_exists(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "fs.exists(path) expects a string path.");
        return make_nil();
    }
    const char* path = AS_CSTRING(args[0]);
    struct stat buffer;
    return make_bool(stat(path, &buffer) == 0);
}

static Value builtin_fs_listDir(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "fs.listDir(path) expects a string path.");
        return make_nil();
    }
    const char* path = AS_CSTRING(args[0]);
    ObjArray* dir_list = new_array(interp);
    push_root(interp, (Obj*)dir_list); // Protect from GC

#ifdef _WIN32
    WIN32_FIND_DATAA find_data;
    char search_path[MAX_PATH];
    snprintf(search_path, MAX_PATH, "%s\\*", path);
    HANDLE h_find = FindFirstFileA(search_path, &find_data);

    if (h_find == INVALID_HANDLE_VALUE) {
        pop_root(interp);
        runtime_error(interp, "Could not open directory '%s'.", path);
        return make_nil();
    }

    do {
        // Skip "." and ".."
        if (strcmp(find_data.cFileName, ".") != 0 && strcmp(find_data.cFileName, "..") != 0) {
            size_t len = strlen(find_data.cFileName);
            ObjString* name = make_heap_string(interp, find_data.cFileName, len);
            array_write(interp, dir_list, make_obj((Obj*)name));
        }
    } while (FindNextFileA(h_find, &find_data) != 0);

    FindClose(h_find);
#else
    DIR* d = opendir(path);
    if (d == NULL) {
        pop_root(interp);
        runtime_error(interp, "Could not open directory '%s': %s", path, strerror(errno));
        return make_nil();
    }

    struct dirent* dir;
    while ((dir = readdir(d)) != NULL) {
        // Skip "." and ".."
        if (strcmp(dir->d_name, ".") != 0 && strcmp(dir->d_name, "..") != 0) {
            size_t len = strlen(dir->d_name);
            ObjString* name = make_heap_string(interp, dir->d_name, len);
            array_write(interp, dir_list, make_obj((Obj*)name));
        }
    }
    closedir(d);
#endif

    pop_root(interp);
    return make_obj((Obj*)dir_list);
}

static Value builtin_fs_remove(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "fs.remove(path) expects a string path.");
        return make_nil();
    }
    const char* path = AS_CSTRING(args[0]);

#ifdef _WIN32
    struct stat stat_buf;
    if (stat(path, &stat_buf) != 0) {
        // File/dir does not exist, but let the remove functions handle the error.
    }
    if (S_ISDIR(stat_buf.st_mode)) {
        if (RemoveDirectoryA(path)) {
            return make_bool(true);
        }
    } else {
        if (DeleteFileA(path)) {
            return make_bool(true);
        }
    }
#else
    if (unlink(path) == 0) {
        return make_bool(true);
    }
    // If unlink fails, it might be a directory.
    if (errno == EISDIR) {
        if (rmdir(path) == 0) {
            return make_bool(true);
        }
    }
#endif

    // If we get here, the operation failed.
    runtime_error(interp, "Could not remove '%s': %s", path, strerror(errno));
    return make_bool(false);
}

static const char* FILE_RESOURCE_TYPE_NAME = "core.file";

static void file_resource_finalize(void* context) {
    if (context) {
        fclose((FILE*)context);
    }
}

static const ResourceVTable g_file_resource_vtable = {
    .type_name = "core.file", // Namespaced for clarity and safety
    .finalize = file_resource_finalize,
};

// --- End File Resource Implementation ---
static Value builtin_fs_open(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "fs.open(path, mode) expects two string arguments.");
        return make_nil();
    }

    const char* path = AS_CSTRING(args[0]);
    const char* mode = AS_CSTRING(args[1]);

    FILE* file = fopen(path, mode);
    if (file == NULL) {
        return make_nil(); // Return nil on failure, don't raise an error
    }

    ObjResource* resource = new_resource(interp, file, &g_file_resource_vtable);

    return make_obj((Obj*)resource);
}

static Value builtin_fs_read(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_RESOURCE(args[0]) || !IS_INT(args[1])) {
        runtime_error(interp, "fs.read(resource, numBytes) expects a resource and an integer.");
        return make_nil();
    }

    ObjResource* res = AS_RESOURCE(args[0]);
    if (res->vtable != &g_file_resource_vtable) {
        runtime_error(interp, "fs.read() requires a file resource, but got a '%s' resource.", res->vtable->type_name);
        return make_nil();
    }
    if (res->is_finalized) {
        runtime_error(interp, "Cannot read from a closed file resource.");
        return make_nil();
    }

    int64_t num_bytes = AS_INT(args[1]);
    if (num_bytes < 0) {
        runtime_error(interp, "Number of bytes to read cannot be negative.");
        return make_nil();
    }
    if (num_bytes == 0) {
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }

    char* buffer = PRATT_MALLOC(num_bytes);
    if (buffer == NULL) {
        runtime_error(interp, "Out of memory allocating read buffer.");
        return make_nil();
    }

    FILE* file = (FILE*)res->context;
    size_t bytes_read = fread(buffer, 1, (size_t)num_bytes, file);

    ObjString* result = make_heap_string(interp, buffer, bytes_read);
    PRATT_FREE(buffer);

    return make_obj((Obj*)result);
}

static Value builtin_fs_write(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_RESOURCE(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "fs.write(resource, data) expects a resource and a string.");
        return make_nil();
    }

    ObjResource* res = AS_RESOURCE(args[0]);
    if (res->vtable != &g_file_resource_vtable) {
        runtime_error(interp, "fs.write() requires a file resource, but got a '%s' resource.", res->vtable->type_name);
        return make_nil();
    }
    if (res->is_finalized) {
        runtime_error(interp, "Cannot write to a closed file resource.");
        return make_nil();
    }

    ObjString* data = AS_STRING(args[1]);
    FILE* file = (FILE*)res->context;
    size_t bytes_written = fwrite(data->chars, 1, data->length, file);
    fflush(file); // Ensure data is written

    return make_int((int64_t)bytes_written);
}

// --- Generic Resource Builtins ---

static Value builtin_resource_close(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_RESOURCE(args[0])) {
        runtime_error(interp, "resource.close() expects a resource argument.");
        return make_nil();
    }

    ObjResource* res = AS_RESOURCE(args[0]);
    if (res->is_finalized) {
        return make_bool(true); // Idempotent
    }

    if (res->vtable->finalize) {
        res->vtable->finalize(res->context);
    }
    res->is_finalized = true;
    res->context = NULL; // Prevent use after close

    return make_bool(true);
}

static Value builtin_resource_type(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_RESOURCE(args[0])) {
        runtime_error(interp, "resource.type() expects a resource argument.");
        return make_nil();
    }
    ObjResource* res = AS_RESOURCE(args[0]);
   const char* type_name = res->vtable->type_name;
    return make_obj((Obj*)make_heap_string(interp, type_name, strlen(type_name)));
}

static Value builtin_resource_is_closed(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_RESOURCE(args[0])) {
        runtime_error(interp, "resource.isClosed() expects a resource argument.");
       return make_nil();
    }
    ObjResource* res = AS_RESOURCE(args[0]);
    return make_bool(res->is_finalized);
}

// --- Path Object ---

static Value builtin_path_join(Interpreter* interp, size_t argc, Value* args) {
#ifdef _WIN32
    const char sep = '\\';
    const char alt_sep = '/';
#else
    const char sep = '/';
    const char alt_sep = '\\'; // Less common, but good to handle
#endif

    if (argc == 0) {
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }

    size_t total_len = 0;
    for (size_t i = 0; i < argc; i++) {
        if (!IS_STRING(args[i])) {
            runtime_error(interp, "path.join() all arguments must be strings.");
            return make_nil();
        }
        total_len += AS_STRING(args[i])->length;
    }
    total_len += argc; // Generous allocation for separators and null terminator

    char* buffer = PRATT_MALLOC(total_len + 1);
    if (!buffer) {
        runtime_error(interp, "Out of memory for path.join().");
        return make_nil();
    }
    char* p = buffer;
    *p = '\0';

    for (size_t i = 0; i < argc; i++) {
        ObjString* part = AS_STRING(args[i]);
        if (part->length == 0) continue;

        if (p > buffer) { // If not the first part
            char last_char = *(p - 1);
            if (last_char != sep && last_char != alt_sep) {
                *(p++) = sep;
            }
        }

        const char* part_chars = part->chars;
        size_t part_len = part->length;

        // Skip leading separators on subsequent parts
        if (p > buffer) {
            while (part_len > 0 && (*part_chars == sep || *part_chars == alt_sep)) {
                part_chars++;
                part_len--;
            }
        }
        
        if (part_len > 0) {
            memcpy(p, part_chars, part_len);
            p += part_len;
        }
    }
    *p = '\0';

    // Normalize separators
    for (char* c = buffer; *c; ++c) {
        if (*c == alt_sep) {
            *c = sep;
        }
    }

    size_t final_len = p - buffer;
    ObjString* result = make_heap_string(interp, buffer, final_len);
    PRATT_FREE(buffer);
    return make_obj((Obj*)result);
}

static Value builtin_path_basename(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "path.basename(path) expects a string path.");
        return make_nil();
    }
    ObjString* path = AS_STRING(args[0]);
    if (path->length == 0) {
        return make_obj((Obj*)make_heap_string(interp, "", 0));
    }

    const char* p = path->chars + path->length - 1;
    // Handle trailing slashes
    while (p > path->chars && (*p == '/' || *p == '\\')) {
        p--;
    }

    const char* end = p;
    while (p > path->chars) {
        if (*p == '/' || *p == '\\') {
            p++;
            break;
        }
        p--;
    }

    size_t len = end - p + 1;
    return make_obj((Obj*)make_heap_string(interp, p, len));
}

static Value builtin_path_dirname(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "path.dirname(path) expects a string path.");
        return make_nil();
    }
    ObjString* path = AS_STRING(args[0]);
    if (path->length == 0) {
        return make_obj((Obj*)make_heap_string(interp, ".", 1));
    }

    const char* end = path->chars + path->length - 1;
    // Handle trailing slashes
    while (end > path->chars && (*end == '/' || *end == '\\')) {
        end--;
    }

    const char* p = end;
    while (p > path->chars) {
        if (*p == '/' || *p == '\\') {
            break;
        }
        p--;
    }

    if (p == path->chars && *p != '/' && *p != '\\') {
        return make_obj((Obj*)make_heap_string(interp, ".", 1));
    }

    // Handle root path like "/"
    if (p == path->chars && (*p == '/' || *p == '\\')) {
        return make_obj((Obj*)make_heap_string(interp, path->chars, 1));
    }
    
    // Skip trailing slashes on the dirname
    while (p > path->chars && (*p == '/' || *p == '\\')) {
        p--;
    }

    size_t len = p - path->chars + 1;
    return make_obj((Obj*)make_heap_string(interp, path->chars, len));
}

static Value builtin_path_extname(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "path.extname(path) expects a string path.");
        return make_nil();
    }

    Value basename_val = builtin_path_basename(interp, argc, args);
    if (interp->had_error) return make_nil();
    ObjString* basename = AS_STRING(basename_val);
    
    const char* p = basename->chars + basename->length - 1;
    while (p >= basename->chars) {
        if (*p == '.') {
            size_t len = (basename->chars + basename->length) - p;
            return make_obj((Obj*)make_heap_string(interp, p, len));
        }
        p--;
    }
    
    return make_obj((Obj*)make_heap_string(interp, "", 0));
}


// --- OS Object ---


static Value builtin_os_exec(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "os.exec(cmd) expects a string command.");
        return make_nil();
    }
    
    const char* cmd = AS_CSTRING(args[0]);
    int result = system(cmd);
    
#ifdef _WIN32
    return make_int(result);
#else
    if (WIFEXITED(result)) {
        return make_int(WEXITSTATUS(result));
    }
    // Command terminated abnormally
    return make_int(-1);
#endif
}

static Value builtin_os_platform(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 0) {
        runtime_error(interp, "os.platform() expects 0 arguments.");
        return make_nil();
    }

#if defined(_WIN32)
    const char* platform = "windows";
#elif defined(__linux__)
    const char* platform = "linux";
#elif defined(__APPLE__) && defined(__MACH__)
    const char* platform = "darwin";
#elif defined(__FreeBSD__)
    const char* platform = "freebsd";
#else
    const char* platform = "unknown";
#endif

    return make_obj((Obj*)make_heap_string(interp, platform, strlen(platform)));
}

static Value builtin_os_getenv(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "os.getenv(name) expects a single string argument.");
        return make_nil();
    }
    
    const char* name = AS_CSTRING(args[0]);
    const char* value = getenv(name);
    
    if (value != NULL) {
        return make_obj((Obj*)make_heap_string(interp, value, strlen(value)));
    }
    
    return make_nil(); // Return nil if not found
}

static Value builtin_os_setenv(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "os.setenv(name, value) expects two string arguments.");
        return make_nil();
    }

    const char* name = AS_CSTRING(args[0]);
    const char* value = AS_CSTRING(args[1]);
    int result;

#ifdef _WIN32
    result = _putenv_s(name, value);
#else
    result = setenv(name, value, 1); // 1 to overwrite
#endif

    if (result != 0) {
        runtime_error(interp, "Could not set environment variable '%s': %s", name, strerror(errno));
        return make_nil();
    }

    return make_bool(true);
}

static Value builtin_os_unsetenv(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "os.unsetenv(name) expects a single string argument.");
        return make_nil();
    }

    const char* name = AS_CSTRING(args[0]);
    int result;

#ifdef _WIN32
    // Windows unsets by setting to an empty string.
    result = _putenv_s(name, "");
#else
    result = unsetenv(name);
#endif
    
    if (result != 0) {
        runtime_error(interp, "Could not unset environment variable '%s': %s", name, strerror(errno));
        return make_nil();
    }

    return make_bool(true);
}

static Value builtin_os_getcwd(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 0) {
        runtime_error(interp, "os.getcwd() expects 0 arguments.");
        return make_nil();
    }
    
    char buffer[1024]; // A reasonable fixed size buffer
#ifdef _WIN32
    if (_getcwd(buffer, sizeof(buffer)) == NULL) {
        runtime_error(interp, "Could not get current working directory: %s", strerror(errno));
        return make_nil();
    }
#else
    if (getcwd(buffer, sizeof(buffer)) == NULL) {
        runtime_error(interp, "Could not get current working directory: %s", strerror(errno));
        return make_nil();
    }
#endif
    return make_obj((Obj*)make_heap_string(interp, buffer, strlen(buffer)));
}

static Value builtin_os_setcwd(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "os.setcwd(path) expects a single string argument.");
        return make_nil();
    }

    const char* path = AS_CSTRING(args[0]);
    int result;
#ifdef _WIN32
    result = _chdir(path);
#else
    result = chdir(path);
#endif

    if (result != 0) {
        runtime_error(interp, "Could not change directory to '%s': %s", path, strerror(errno));
        return make_nil();
    }

    return make_bool(true);
}

// --- Date Object ---

static Value builtin_date_now(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 0) {
        runtime_error(interp, "date.now() expects 0 arguments.");
        return make_nil();
    }
    return make_int((int64_t)time(NULL));
}

static Value builtin_date_format(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 1 || argc > 2 || !IS_INT(args[0])) {
        runtime_error(interp, "date.format(timestamp, [format]) expects an integer timestamp.");
        return make_nil();
    }
    
    time_t ts = (time_t)AS_INT(args[0]);
    const char* fmt = "%Y-%m-%d %H:%M:%S";
    if (argc == 2) {
        if (!IS_STRING(args[1])) {
            runtime_error(interp, "date.format() format argument must be a string.");
            return make_nil();
        }
        fmt = AS_CSTRING(args[1]);
    }
    
    struct tm* timeinfo = localtime(&ts);
    char buffer[256];
    size_t len = strftime(buffer, sizeof(buffer), fmt, timeinfo);
    
    return make_obj((Obj*)make_heap_string(interp, buffer, len));
}

static Value builtin_date_parse(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 2 || !IS_STRING(args[0]) || !IS_STRING(args[1])) {
        runtime_error(interp, "date.parse(str, fmt) expects two string arguments.");
        return make_nil();
    }
    
#ifdef HAVE_STRPTIME
    const char* str = AS_CSTRING(args[0]);
    const char* fmt = AS_CSTRING(args[1]);
    struct tm tm = {0};
    
    char* unparsed = strptime(str, fmt, &tm);
    if (unparsed == NULL || *unparsed != '\0') {
        runtime_error(interp, "String '%s' does not match format '%s'.", str, fmt);
        return make_nil();
    }
    
    time_t t = mktime(&tm);
    if (t == -1) {
        runtime_error(interp, "Could not convert parsed time to a timestamp.");
        return make_nil();
    }
    
    return make_int((int64_t)t);
#else
    runtime_error(interp, "date.parse() is not supported on this platform.");
    return make_nil();
#endif
}

static Value tm_to_object(Interpreter* interp, const struct tm* t) {
    ObjObject* obj = new_object(interp);
    push_root(interp, (Obj*)obj);

    map_set(interp, &obj->map, interpreter_intern_string(interp, "year", 4), make_int(t->tm_year + 1900));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "month", 5), make_int(t->tm_mon + 1));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "day", 3), make_int(t->tm_mday));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "hour", 4), make_int(t->tm_hour));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "min", 3), make_int(t->tm_min));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "sec", 3), make_int(t->tm_sec));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "yday", 4), make_int(t->tm_yday + 1));
    map_set(interp, &obj->map, interpreter_intern_string(interp, "wday", 4), make_int(t->tm_wday));

    pop_root(interp);
    return make_obj((Obj*)obj);
}

static Value builtin_date_local(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_INT(args[0])) {
        runtime_error(interp, "date.local(timestamp) expects an integer timestamp.");
        return make_nil();
    }
    time_t ts = (time_t)AS_INT(args[0]);
    struct tm* timeinfo = localtime(&ts);
    if (!timeinfo) return make_nil();
    struct tm copy = *timeinfo; // Copy from static buffer
    return tm_to_object(interp, &copy);
}

static Value builtin_date_utc(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_INT(args[0])) {
        runtime_error(interp, "date.utc(timestamp) expects an integer timestamp.");
        return make_nil();
    }
    time_t ts = (time_t)AS_INT(args[0]);
    struct tm* timeinfo = gmtime(&ts);
    if (!timeinfo) return make_nil();
    struct tm copy = *timeinfo; // Copy from static buffer
    return tm_to_object(interp, &copy);
}

// --- JSON Built-in Implementations ---

// Forward declaration for recursive conversion
static Value convert_cjson_to_value(Interpreter* interp, cJSON* item);

static Value convert_cjson_to_value(Interpreter* interp, cJSON* item) {
    if (cJSON_IsInvalid(item)) {
        runtime_error(interp, "Invalid cJSON item during conversion.");
        return make_nil();
    }
    if (cJSON_IsNull(item))    return make_nil();
    if (cJSON_IsTrue(item))    return make_bool(true);
    if (cJSON_IsFalse(item))   return make_bool(false);
    if (cJSON_IsString(item))  {
        return make_obj((Obj*)make_heap_string(interp, item->valuestring, strlen(item->valuestring)));
    }
    if (cJSON_IsNumber(item))  {
        double num = item->valuedouble;
        if (num == (int64_t)num) {
            return make_int((int64_t)num);
        }
        return make_double(num);
    }
    if (cJSON_IsArray(item)) {
        ObjArray* array = new_array(interp);
        push_root(interp, (Obj*)array); // Protect array from GC during population

        cJSON* element;
        cJSON_ArrayForEach(element, item) {
            Value val = convert_cjson_to_value(interp, element);
            if (interp->had_error) {
                pop_root(interp); // Clean up root stack on error
                return make_nil();
            }
            array_write(interp, array, val);
        }

        pop_root(interp);
        return make_obj((Obj*)array);
    }
    if (cJSON_IsObject(item)) {
        ObjObject* object = new_object(interp);
        push_root(interp, (Obj*)object); // Protect object from GC during population

        cJSON* element;
        cJSON_ArrayForEach(element, item) {
            ObjString* key = interpreter_intern_string(interp, element->string, strlen(element->string));
            push_root(interp, (Obj*)key); // Protect key from GC while value is converted

            Value val = convert_cjson_to_value(interp, element);
            pop_root(interp); // Key is safe now
            
            if (interp->had_error) {
                pop_root(interp); // Clean up object root on error
                return make_nil();
            }

            map_set(interp, &object->map, key, val);
        }

        pop_root(interp);
        return make_obj((Obj*)object);
    }

    runtime_error(interp, "Unknown cJSON type encountered during conversion.");
    return make_nil();
}

static Value builtin_json_parse(Interpreter* interp, size_t argc, Value* args) {
    if (argc != 1 || !IS_STRING(args[0])) {
        runtime_error(interp, "json.parse() expects one string argument.");
        return make_nil();
    }
    ObjString* json_string = AS_STRING(args[0]);

    cJSON* root = cJSON_ParseWithLengthOpts(json_string->chars, json_string->length + 1, NULL, 0);
    
    // Check for a parse error.
    if (root == NULL) {
        const char* error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            runtime_error(interp, "JSON parse error near: '%.20s'", error_ptr);
        } else {
            runtime_error(interp, "JSON parse error (null result).");
        }
        return make_nil();
    }

    // On success, convert the cJSON structure to PrattScript values.
    Value result = convert_cjson_to_value(interp, root);

    // Clean up the cJSON structure.
    cJSON_Delete(root);

    return result;
}

// Forward declaration for recursive conversion
static cJSON* convert_value_to_cjson(Interpreter* interp, Value value, PrintVisitor* visitor);

static cJSON* convert_value_to_cjson(Interpreter* interp, Value value, PrintVisitor* visitor) {
    switch (value.type) {
        case VAL_NIL:     return cJSON_CreateNull();
        case VAL_BOOL:    return cJSON_CreateBool(AS_BOOL(value));
        case VAL_INT:     return cJSON_CreateNumber((double)AS_INT(value));
        case VAL_DOUBLE:  return cJSON_CreateNumber(AS_DOUBLE(value));
        case VAL_OBJ:
            if (visitor_contains(visitor, AS_OBJ(value))) {
                return cJSON_CreateNull(); // Represent cycles as null
            }
            
            if (IS_ARRAY(value) || IS_OBJECT(value)) {
                visitor_push(visitor, AS_OBJ(value));
            }

            cJSON* result_json = NULL;

            switch (OBJ_TYPE(value)) {
                case OBJ_STRING:
                    result_json = cJSON_CreateString(AS_CSTRING(value));
                    break;
                case OBJ_ARRAY: {
                    ObjArray* array = AS_ARRAY(value);
                    cJSON* json_array = cJSON_CreateArray();
                    for (int i = 0; i < array->count; i++) {
                        cJSON* item = convert_value_to_cjson(interp, array->values[i], visitor);
                        if (item == NULL) item = cJSON_CreateNull(); 
                        cJSON_AddItemToArray(json_array, item);
                    }
                    result_json = json_array;
                    break;
                }
                case OBJ_OBJECT: {
                    ObjObject* obj = AS_OBJECT(value);
                    cJSON* json_object = cJSON_CreateObject();
                    for (int i = 0; i < obj->map.capacity; i++) {
                        Entry* entry = &obj->map.entries[i];
                        if (entry->key != NULL) {
                            cJSON* item = convert_value_to_cjson(interp, entry->value, visitor);
                            // Unstringifiable values become null. Unlike JS, we don't omit the key.
                            if (item == NULL) item = cJSON_CreateNull();
                            cJSON_AddItemToObject(json_object, entry->key->chars, item);
                        }
                    }
                    result_json = json_object;
                    break;
                }
                default: // Functions, environments, etc., are not representable.
                    result_json = cJSON_CreateNull();
                    break;
            }

            if (IS_ARRAY(value) || IS_OBJECT(value)) {
                visitor_pop(visitor);
            }
            return result_json;

        default: // Builtins are not representable
            return NULL;
    }
}

static Value builtin_json_stringify(Interpreter* interp, size_t argc, Value* args) {
    if (argc < 1 || argc > 2) {
        runtime_error(interp, "json.stringify() expects 1 or 2 arguments, but got %zu.", argc);
        return make_nil();
    }
    
    bool pretty = false;
    if (argc == 2) {
        if (!IS_BOOL(args[1])) {
            runtime_error(interp, "json.stringify() second argument must be a boolean.");
            return make_nil();
        }
        pretty = AS_BOOL(args[1]);
    }
    
    // Use the existing cycle detection mechanism from print_value
    PrintVisitor visitor;
    visitor_init(&visitor);
    
    cJSON* root = convert_value_to_cjson(interp, args[0], &visitor);
    
    visitor_free(&visitor);
    
    // If the top-level value is unstringifiable (e.g., a function), return "null".
    if (root == NULL) {
        root = cJSON_CreateNull();
    }

    char* c_string = pretty ? cJSON_Print(root) : cJSON_PrintUnformatted(root);
    cJSON_Delete(root);

    if (c_string == NULL) {
        runtime_error(interp, "Failed to stringify value (out of memory).");
        return make_nil();
    }

    ObjString* result = make_heap_string(interp, c_string, strlen(c_string));
    
    // cJSON's print functions use malloc, so we must free the string with free().
    free(c_string); 

    return make_obj((Obj*)result);
}
