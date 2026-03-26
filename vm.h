// vm.h - Virtual Machine Header
#ifndef VM_H
#define VM_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#include <limits.h>
#include <setjmp.h>

#include "runtime_core.h"
#include "sfc64.h"

#define MAX_REGS 256
#define MAX_CALL_FRAMES 1024
#define MAX_CONSTANTS 65536
#define INLINE_CACHE_ENTRIES 4

#define PIC_UNINITIALISED 0xFFFF
#define STACK_MAX (MAX_REGS * MAX_CALL_FRAMES)

#if defined(__GNUC__) || defined(__clang__)
#define USE_COMPUTED_GOTO 1
#else
#define USE_COMPUTED_GOTO 0
#endif

typedef struct VM VM;

#ifndef __ORDER_LITTLE_ENDIAN__
#define __ORDER_LITTLE_ENDIAN__ 1234
#endif

#ifndef __ORDER_BIG_ENDIAN__
#define __ORDER_BIG_ENDIAN__ 4321
#endif

#ifndef __BYTE_ORDER__
#if defined(_WIN32)
#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
#else
#include <endian.h>
#endif
#endif

#if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#error "Big-endian architectures require a different Value layout."
#endif

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
_Static_assert(sizeof(double) == 8, "double must be 64-bit IEEE-754");
_Static_assert(sizeof(int64_t) == 8, "int64_t must be 64-bit");
_Static_assert(sizeof(void*) <= 8, "unexpected pointer width");
_Static_assert(sizeof(Value) == 16, "Value unexpectedly padded");
#endif

struct ObjFunction {
    Obj obj;
    int arity;
    int upvalueCount;
    int regCount;
    uint8_t *code;
    size_t codeSize;
    Value *constants;
    int constantCount;
    ObjString *name;
    int *lines;
    int lineCount;
};

struct ObjClosure {
    Obj obj;
    ObjFunction *function;
    ObjUpvalue **upvalues;
    int upvalueCount;
};

struct ObjUpvalue {
    Obj obj;
    Value *location;
    Value closed;
    ObjUpvalue *next;
};

struct ObjArray {
    Obj obj;
    int count;
    int capacity;
    Value *values;
};

struct ObjObject {
    Obj obj;
    Table map;
};

struct ObjEnv {
    Obj obj;
    ObjEnv *parent;
    Table table;
};

typedef struct ResourceVTable {
    const char *type_name;
    void (*finalize)(void *context);
} ResourceVTable;

struct ObjResource {
    Obj obj;
    void *context;
    const ResourceVTable *vtable;
    bool is_finalized;
};

typedef struct {
    const Table *table;
    Value *location;
    uint32_t version;
} PICEntry;

typedef struct {
    PICEntry entries[INLINE_CACHE_ENTRIES];
    uint8_t count;
} PIC;

typedef struct {
    ObjClosure *closure;
    uint8_t *ip;
    Value *regs;
    uint8_t returnReg;
} CallFrame;

struct VM {
    CallFrame frames[MAX_CALL_FRAMES];
    int frameCount;

    Value *stack;
    Value *stackTop;

    Table globals;
    Table strings;
    ObjUpvalue *openUpvalues;

    size_t bytesAllocated;
    size_t nextGC;
    Obj *objects;

    int grayCount;
    int grayCapacity;
    Obj **grayStack;

    PIC *picCache;
    size_t picCacheSize;
    size_t picCacheCapacity;

    ObjString *initString;

    ObjEnv *env;
    Table interner;
    Obj **root_stack;
    int root_stack_count;
    int root_stack_capacity;
    int had_error;
    char error_message[256];
    jmp_buf oom_jmp;
    int oom_guard_active;
    SFC64Context rng;
};

typedef enum {
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_CONST,
    OP_MOVE,
    OP_LOAD_GLOBAL,
    OP_STORE_GLOBAL,
    OP_LOAD_UPVALUE,
    OP_STORE_UPVALUE,
    OP_CLOSE_UPVALUE,
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_MODULO,
    OP_NEGATE,
    OP_EQUAL,
    OP_NOT_EQUAL,
    OP_GREATER,
    OP_GREATER_EQUAL,
    OP_LESS,
    OP_LESS_EQUAL,
    OP_NOT,
    OP_AND,
    OP_OR,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_JUMP_IF_TRUE,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_RETURN,
    OP_PRINT,
    OP_POP,
    OP_COUNT
} OpCode;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM(VM *vm);
void freeVM(VM *vm);
InterpretResult interpret(VM *vm, ObjFunction *function);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif // VM_H
