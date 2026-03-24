#ifndef RUNTIME_CORE_H
#define RUNTIME_CORE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>

typedef enum {
    VAL_NIL,
    VAL_BOOL,
    VAL_INT,
    VAL_DOUBLE,
    VAL_BUILTIN,
    VAL_OBJ,
    VAL_TOMBSTONE
} ValueType;

typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct ObjFunction ObjFunction;
typedef struct ObjClosure ObjClosure;
typedef struct ObjUpvalue ObjUpvalue;
typedef struct ObjArray ObjArray;
typedef struct ObjObject ObjObject;
typedef struct ObjEnv ObjEnv;
typedef struct ObjResource ObjResource;
typedef struct Value Value;

typedef Value (*BuiltinFn)(void *ctx, size_t argc, Value *args);

struct Value {
    ValueType type;
    union {
        bool boolean;
        int64_t integer;
        double number;
        BuiltinFn builtin;
        Obj *obj;
    } as;
};

typedef enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_CLOSURE,
    OBJ_UPVALUE,
    OBJ_ARRAY,
    OBJ_OBJECT,
    OBJ_ENV,
    OBJ_RESOURCE
} ObjType;

struct Obj {
    ObjType type;
    bool is_marked;
    Obj *next;
};

struct ObjString {
    Obj obj;
    uint32_t length;
    uint32_t hash;
    char chars[];
};

typedef struct {
    ObjString *key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry *entries;
    uint32_t version;
} Table;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_INT(value) ((value).type == VAL_INT)
#define IS_DOUBLE(value) ((value).type == VAL_DOUBLE)
#define IS_NUMERIC(value) (IS_INT(value) || IS_DOUBLE(value))
#define IS_NUMBER(value) IS_NUMERIC(value)
#define IS_BUILTIN(value) ((value).type == VAL_BUILTIN)
#define IS_OBJ(value) ((value).type == VAL_OBJ)
#define IS_TOMBSTONE(value) ((value).type == VAL_TOMBSTONE)

#define AS_BOOL(value) ((value).as.boolean)
#define AS_INT(value) ((value).as.integer)
#define AS_DOUBLE(value) ((value).as.number)
#define AS_NUMBER(value) (IS_INT(value) ? (double)AS_INT(value) : AS_DOUBLE(value))
#define AS_BUILTIN(value) ((value).as.builtin)
#define AS_OBJ(value) ((value).as.obj)

#define OBJ_TYPE(value) (AS_OBJ(value)->type)
#define IS_STRING(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_STRING)
#define IS_FUNCTION(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_FUNCTION)
#define IS_CLOSURE(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_CLOSURE)
#define IS_ARRAY(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_ARRAY)
#define IS_OBJECT(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_OBJECT)
#define IS_ENV(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_ENV)
#define IS_RESOURCE(value) (IS_OBJ(value) && OBJ_TYPE(value) == OBJ_RESOURCE)

#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (AS_STRING(value)->chars)
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_ARRAY(value) ((ObjArray *)AS_OBJ(value))
#define AS_OBJECT(value) ((ObjObject *)AS_OBJ(value))
#define AS_ENV(value) ((ObjEnv *)AS_OBJ(value))
#define AS_RESOURCE(value) ((ObjResource *)AS_OBJ(value))

#define NIL_VAL() ((Value){VAL_NIL, {.integer = 0}})
#define BOOL_VAL(b) ((Value){VAL_BOOL, {.boolean = (b)}})
#define INT_VAL(i) ((Value){VAL_INT, {.integer = (i)}})
#define DOUBLE_VAL(d) ((Value){VAL_DOUBLE, {.number = (d)}})
#define BUILTIN_VAL(f) ((Value){VAL_BUILTIN, {.builtin = (f)}})
#define OBJ_VAL(object_ptr) ((Value){VAL_OBJ, {.obj = (Obj *)(object_ptr)}})
#define TOMBSTONE_VAL() ((Value){VAL_TOMBSTONE, {.integer = 0}})

#endif /* RUNTIME_CORE_H */
