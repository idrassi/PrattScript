// vm.c - Virtual Machine Implementation
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include "vm.h"

/*───────────────────────────────────────────────────────────────────────────*
 *  Helper utilities                                                        *
 *───────────────────────────────────────────────────────────────────────────*/

/* 16‑bit big‑endian store, used when we self‑patch a PIC index.             */
static inline void write_u16_be(uint8_t* dst, uint16_t value) {
    dst[0] = (uint8_t)((value >> 8) & 0xFF);
    dst[1] = (uint8_t)(value & 0xFF);
}

// These helpers are provided by the surrounding runtime.
void initTable(Table* table);
void freeTable(VM* vm, Table* table);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableSet(VM* vm, Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, ObjString* key);
Entry* tableFindEntry(Entry* entries, int capacity, ObjString* key);
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);
ObjClosure* newClosure(VM* vm, ObjFunction* function);
ObjUpvalue* newUpvalue(VM* vm, Value* slot);
ObjArray* newArray(VM* vm);
ObjObject* newObject(VM* vm);
ObjEnv* new_env_obj(VM* vm, ObjEnv* parent);
ObjResource* newResource(VM* vm, void* context, const ResourceVTable* vtable);
void printValue(Value value);
void freeObjects(VM* vm);
ObjString* copyString(VM* vm, const char* chars, int length);
static void closeUpvalues(VM* vm, Value* last);
static bool callValue(VM* vm, CallFrame* callerFrame, Value callee, uint8_t argCount, uint8_t firstArgReg, uint8_t returnReg);
static bool call(VM* vm, ObjClosure* closure, uint8_t argCount, uint8_t firstArgReg, uint8_t returnReg);
static ObjUpvalue* captureUpvalue(VM* vm, Value* local);
static bool valuesEqual(Value a, Value b);
static bool isFalsey(Value value);
void mark_table(VM* vm, Table* table);
void env_define(VM* vm, ObjEnv* env, ObjString* name, Value value);
bool env_get(ObjEnv* env, ObjString* key, Value* value);
bool env_assign(VM* vm, ObjEnv* env, ObjString* name, Value value);
static int getLine(ObjFunction* function, size_t instruction);
void table_remove_white(VM* vm, Table* table);
void free_object(VM* vm, Obj* object);
static void mark_roots(VM* vm);
static void trace_references(VM* vm);
static void sweep(VM* vm);
static void mark_object(VM* vm, Obj* object);
static void mark_value(VM* vm, Value value);
static void blacken_object(VM* vm, Obj* object);

// Runtime error handling
static void runtimeError(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);
    
    // Stack trace
    for (int i = vm->frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->code - 1;
        fprintf(stderr, "[line %d] in ", getLine(function, instruction));
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }
    vm->had_error = 1;
}

// Find line number for an instruction offset
static int getLine(ObjFunction* function, size_t instruction) {
    int line = 1; // Default to 1 if no lines
    size_t offset = 0;
    if (function == NULL || function->lines == NULL || function->lineCount < 2) {
        return line;
    }
    if (instruction >= function->codeSize) return line;

    for (int i = 0; i + 1 < function->lineCount; i += 2) {
        offset += (size_t)function->lines[i];
        line = function->lines[i + 1];
        if (instruction < offset) return line;
    }
    return line;
}

static void resetExecutionState(VM* vm) {
    if (vm->stack != NULL && vm->openUpvalues != NULL) {
        closeUpvalues(vm, vm->stack);
    }
    vm->frameCount = 0;
    vm->stackTop = vm->stack;
    vm->openUpvalues = NULL;
    vm->had_error = 0;
    vm->error_message[0] = '\0';
}

// Initialize VM
void initVM(VM* vm) {
    memset(vm, 0, sizeof(VM));

    /* Allocate the register stack on the heap (avoids huge TLS blocks).   */
    vm->stack = (Value*)malloc(sizeof(Value) * STACK_MAX);
    if (!vm->stack) {
        fprintf(stderr, "fatal: unable to allocate VM register stack\n");
        abort();
    }
    resetExecutionState(vm);
    initTable(&vm->globals);
    initTable(&vm->strings);
    
    vm->bytesAllocated = 0;
    vm->nextGC = 1024 * 1024;
    
    vm->grayCount = 0;
    vm->grayCapacity = 0;
    vm->grayStack = NULL;
    
    vm->picCacheSize = 0;
    vm->picCacheCapacity = 0;
    vm->picCache = NULL;
    
    vm->initString = copyString(vm, "init", 4); // Added for "init" string cache
    vm->env = new_env_obj(vm, NULL);
    initTable(&vm->interner);
    vm->root_stack = NULL;
    vm->root_stack_count = 0;
    vm->root_stack_capacity = 0;
    sfc64_init(&vm->rng, (uint64_t)time(NULL));
}

void freeVM(VM* vm) {
    free(vm->stack);
    freeTable(vm, &vm->globals);
    freeTable(vm, &vm->strings);
    freeTable(vm, &vm->interner);
    freeObjects(vm);
    free(vm->picCache);
    free(vm->grayStack);
    free(vm->root_stack);
}

// Stack operations
static inline bool push(VM* vm, Value value) {
    if ((size_t)(vm->stackTop - vm->stack) >= STACK_MAX) {
        runtimeError(vm, "Stack overflow.");
        return false;
    }
    *vm->stackTop = value;
    vm->stackTop++;
    return true;
}

static inline Value pop(VM* vm) {
    vm->stackTop--;
    return *vm->stackTop;
}

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap) * 2)

// PIC operations
static PIC* allocatePIC(VM* vm) {
    if (vm->picCacheSize >= vm->picCacheCapacity) {
        if (vm->picCacheSize == UINT16_MAX) {          /* would overflow 16‑bit index */
            runtimeError(vm, "PIC table exhausted (>65 535 entries).");
            return NULL;
        }
        size_t oldCap = vm->picCacheCapacity;
        size_t newCap = GROW_CAPACITY(oldCap);
        if (newCap > UINT16_MAX) newCap = UINT16_MAX;
        if (newCap == oldCap) { /* No growth possible */
            runtimeError(vm, "PIC table exhausted (>65 535 entries).");
            return NULL;
        }
        /* Prevent size_t overflow in realloc size calculation */
        if (newCap > SIZE_MAX / sizeof(PIC)) {
            runtimeError(vm, "PIC table size would overflow size_t.");
            return NULL;
        }

        void* newPtr = realloc(vm->picCache, sizeof(PIC) * newCap);
        if (!newPtr) {
            runtimeError(vm, "Out of memory");
            return NULL;
        }
        vm->picCache         = newPtr;
        vm->picCacheCapacity = newCap;
    }
    
    PIC* pic = &vm->picCache[vm->picCacheSize++];
    pic->count = 0;
    return pic;
}

static bool picLookup(PIC* pic, const Table* table, uint32_t version, Value** location) {
    for (int i = 0; i < pic->count; i++) {
        if (pic->entries[i].table == table && pic->entries[i].version == version) {
            *location = pic->entries[i].location;
            return true;
        }
    }
    return false;
}

static void picAddLocation(PIC* pic, const Table* table, Value* location, uint32_t version) {
    if (pic->count < INLINE_CACHE_ENTRIES) {
        pic->entries[pic->count].table = table;
        pic->entries[pic->count].location = location;
        pic->entries[pic->count].version = version;
        pic->count++;
    }
}

static PIC* getOrCreatePIC(VM* vm, bool hasPic, uint16_t picIndex, uint8_t* picIndexPtr) {
    if (hasPic) {
        return &vm->picCache[picIndex];
    }

    PIC* pic = allocatePIC(vm);
    if (pic == NULL) {
        return NULL;
    }

    write_u16_be(picIndexPtr, (uint16_t)(vm->picCacheSize - 1));
    return pic;
}

static void cachePropertyLocation(VM* vm,
                                  Table* fields,
                                  ObjString* name,
                                  bool hasPic,
                                  uint16_t picIndex,
                                  uint8_t* picIndexPtr) {
    PIC* pic = getOrCreatePIC(vm, hasPic, picIndex, picIndexPtr);
    if (pic == NULL) {
        return;
    }

    Entry* entry = tableFindEntry(fields->entries, fields->capacity, name);
    if (entry != NULL && entry->key != NULL) {
        picAddLocation(pic, fields, &entry->value, fields->version);
    }
}

static inline void validateReg(VM* vm, CallFrame* frame, uint8_t reg) {
    if (reg >= frame->closure->function->regCount) {
        runtimeError(vm, "Register %d out of bounds", reg);
    }
}

#ifdef DEBUG_VM
    #define VALIDATE_REG(vm, frame, reg) validateReg(vm, frame, reg)
#else
    #define VALIDATE_REG(vm, frame, reg) ((void)0)
#endif

// Main interpreter loop
InterpretResult interpret(VM* vm, ObjFunction* function) {
    resetExecutionState(vm);
    ObjClosure* closure = newClosure(vm, function);
    if (!call(vm, closure, 0, 0, 0)) {
        return INTERPRET_RUNTIME_ERROR;
    }
    
#if USE_COMPUTED_GOTO
    static void* dispatchTable[] = {
        &&op_nil, &&op_true, &&op_false, &&op_const,
        &&op_move, &&op_load_global, &&op_store_global, &&op_load_upvalue, &&op_store_upvalue, &&op_close_upvalue,
        &&op_get_property, &&op_set_property,
        &&op_add, &&op_subtract, &&op_multiply, &&op_divide, &&op_modulo, &&op_negate,
        &&op_equal, &&op_not_equal, &&op_greater, &&op_greater_equal, &&op_less, &&op_less_equal,
        &&op_not, &&op_and, &&op_or,
        &&op_jump, &&op_jump_if_false, &&op_jump_if_true, &&op_loop,
        &&op_call, &&op_closure, &&op_return,
        &&op_print, &&op_pop
    };

    _Static_assert(OP_COUNT ==
                   (sizeof(dispatchTable) / sizeof(dispatchTable[0])),
                   "dispatchTable / OpCode enum mismatch");
    
    #define READ_BYTE() (*frame->ip++)
    #define READ_SHORT()  (frame->ip += 2,                                  \
                        (uint16_t)(((uint16_t)frame->ip[-2] << 8) |      \
                                    (uint16_t)frame->ip[-1]))
    #define READ_CONSTANT() (frame->closure->function->constants[READ_SHORT()])
    #define READ_REG(reg) (VALIDATE_REG(vm, frame, reg), frame->regs[reg])
    #define WRITE_REG(reg, val) (VALIDATE_REG(vm, frame, reg), frame->regs[reg] = val)

    CallFrame* frame = &vm->frames[vm->frameCount - 1];

    #define DISPATCH() goto *dispatchTable[READ_BYTE()]

    DISPATCH();
    
    op_nil: {
        uint8_t reg = READ_BYTE();
        WRITE_REG(reg, NIL_VAL());
        DISPATCH();
    }
    
    op_true: {
        uint8_t reg = READ_BYTE();
        WRITE_REG(reg, BOOL_VAL(true));
        DISPATCH();
    }
    
    op_false: {
        uint8_t reg = READ_BYTE();
        WRITE_REG(reg, BOOL_VAL(false));
        DISPATCH();
    }
    
    op_const: {
        uint8_t reg = READ_BYTE();
        uint16_t constIndex = READ_SHORT();
        if (constIndex >= frame->closure->function->constantCount) {
            runtimeError(vm, "Invalid constant index.");
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(reg, frame->closure->function->constants[constIndex]);
        DISPATCH();
    }
    
    op_move: {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        WRITE_REG(dst, READ_REG(src));
        DISPATCH();
    }
    
    op_load_global: {
        uint8_t reg = READ_BYTE();
        ObjString* name = AS_STRING(READ_CONSTANT());
        Value value;
        if (!tableGet(&vm->globals, name, &value)) {
            runtimeError(vm, "Undefined variable '%s'.", name->chars);
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(reg, value);
        DISPATCH();
    }
    
    op_store_global: {
        ObjString* name = AS_STRING(READ_CONSTANT());
        uint8_t reg = READ_BYTE();
        tableSet(vm, &vm->globals, name, READ_REG(reg));
        DISPATCH();
    }
    
    op_load_upvalue: {
        uint8_t reg = READ_BYTE();
        uint8_t slot = READ_BYTE();
        WRITE_REG(reg, *frame->closure->upvalues[slot]->location);
        DISPATCH();
    }
    
    op_store_upvalue: {
        uint8_t slot = READ_BYTE();
        uint8_t reg = READ_BYTE();
        *frame->closure->upvalues[slot]->location = READ_REG(reg);
        DISPATCH();
    }
    
    op_close_upvalue: {
        uint8_t reg = READ_BYTE();
        closeUpvalues(vm, frame->regs + reg);
        DISPATCH();
    }
    
    op_get_property: {
        uint8_t dst = READ_BYTE();
        uint8_t obj = READ_BYTE();
        uint16_t picIndex = READ_SHORT();
        uint8_t* picIndexPtr = frame->ip - 2;
        ObjString* name = AS_STRING(READ_CONSTANT());

        bool hasPic = picIndex != PIC_UNINITIALISED;
        Value receiver = READ_REG(obj);
        if (!IS_OBJECT(receiver)) {
            runtimeError(vm, "Only objects have properties.");
            return INTERPRET_RUNTIME_ERROR;
        }

        ObjObject* object = AS_OBJECT(receiver);
        Table* fields = &object->map;
        Value value;

        if (hasPic) {
            PIC* pic = &vm->picCache[picIndex];
            Value* location;
            if (picLookup(pic, fields, fields->version, &location)) {
                WRITE_REG(dst, *location);
                DISPATCH();
            }
        }

        // Slow path
        if (tableGet(fields, name, &value)) {
            WRITE_REG(dst, value);
            cachePropertyLocation(vm, fields, name, hasPic, picIndex, picIndexPtr);
            DISPATCH();
        }

        runtimeError(vm, "Undefined property '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
    }
    
    op_set_property: {
        uint8_t obj = READ_BYTE();
        uint16_t picIndex = READ_SHORT();
        uint8_t* picIndexPtr = frame->ip - 2;
        ObjString* name = AS_STRING(READ_CONSTANT());
        uint8_t valReg = READ_BYTE();

        bool hasPic = picIndex != PIC_UNINITIALISED;
        Value receiver = READ_REG(obj);
        if (!IS_OBJECT(receiver)) {
            runtimeError(vm, "Only objects have fields.");
            return INTERPRET_RUNTIME_ERROR;
        }

        ObjObject* object = AS_OBJECT(receiver);
        Table* fields = &object->map;

        if (hasPic) {
            PIC* pic = &vm->picCache[picIndex];
            Value* location;
            if (picLookup(pic, fields, fields->version, &location)) {
                *location = READ_REG(valReg);
                fields->version++;
                DISPATCH();
            }
        }

        // Slow path
        tableSet(vm, fields, name, READ_REG(valReg));
        cachePropertyLocation(vm, fields, name, hasPic, picIndex, picIndexPtr);
        DISPATCH();
    }
    
    op_add: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        Value v2 = READ_REG(src2);
        if (IS_INT(v1) && IS_INT(v2)) {
            int64_t a = AS_INT(v1);
            int64_t b = AS_INT(v2);
            if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
                WRITE_REG(dst, DOUBLE_VAL((double)a + (double)b));
            } else {
                WRITE_REG(dst, INT_VAL(a + b));
            }
        } else if (IS_NUMERIC(v1) && IS_NUMERIC(v2)) {
            double a = AS_NUMBER(v1);
            double b = AS_NUMBER(v2);
            WRITE_REG(dst, DOUBLE_VAL(a + b));
        } else {
            frame->ip -= 3;
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        DISPATCH();
    }
    
    op_subtract: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        Value v2 = READ_REG(src2);
        if (IS_INT(v1) && IS_INT(v2)) {
            int64_t a = AS_INT(v1);
            int64_t b = AS_INT(v2);
            if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b)) {
                WRITE_REG(dst, DOUBLE_VAL((double)a - (double)b));
            } else {
                WRITE_REG(dst, INT_VAL(a - b));
            }
        } else if (IS_NUMERIC(v1) && IS_NUMERIC(v2)) {
            double a = AS_NUMBER(v1);
            double b = AS_NUMBER(v2);
            WRITE_REG(dst, DOUBLE_VAL(a - b));
        } else {
            frame->ip -= 3;
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        DISPATCH();
    }
    
    op_multiply: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        Value v2 = READ_REG(src2);
        if (IS_INT(v1) && IS_INT(v2)) {
            int64_t a = AS_INT(v1);
            int64_t b = AS_INT(v2);
            if (a == 0 || b == 0) {
                WRITE_REG(dst, INT_VAL(0));
            } else if ((a > 0) == (b > 0)) {
                if (a > INT64_MAX / b) goto mul_ovf_label;
            } else {
                if (a < INT64_MIN / b) goto mul_ovf_label;
            }
            WRITE_REG(dst, INT_VAL(a * b));
            goto mul_end_label;
mul_ovf_label:
            WRITE_REG(dst, DOUBLE_VAL((double)a * (double)b));
mul_end_label:
            ;
        } else if (IS_NUMERIC(v1) && IS_NUMERIC(v2)) {
            double a = AS_NUMBER(v1);
            double b = AS_NUMBER(v2);
            WRITE_REG(dst, DOUBLE_VAL(a * b));
        } else {
            frame->ip -= 3;
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        DISPATCH();
    }
    
    op_divide: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        Value v2 = READ_REG(src2);
        if (!IS_NUMERIC(v1) || !IS_NUMERIC(v2)) {
            frame->ip -= 3;
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        double a = AS_NUMBER(v1);
        double b = AS_NUMBER(v2);
        if (b == 0.0) {
            frame->ip -= 3;
            runtimeError(vm, "Division by zero.");
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(dst, DOUBLE_VAL(a / b));
        DISPATCH();
    }
    
    op_modulo: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        Value v2 = READ_REG(src2);
        if (!IS_NUMERIC(v1) || !IS_NUMERIC(v2)) {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        double a = AS_NUMBER(v1);
        double b = AS_NUMBER(v2);
        if (b == 0) {
            runtimeError(vm, "Modulo by zero.");
            return INTERPRET_RUNTIME_ERROR;
        }
        double result = fmod(a, b);
        if (result != 0 && (a < 0) != (b < 0)) {
            result += b;
        }
        WRITE_REG(dst, DOUBLE_VAL(result));
        DISPATCH();
    }
    
    op_negate: {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        Value v = READ_REG(src);
        if (IS_INT(v)) {
            int64_t a = AS_INT(v);
            if (a == INT64_MIN) {
                WRITE_REG(dst, DOUBLE_VAL(-(double)a));
            } else {
                WRITE_REG(dst, INT_VAL(-a));
            }
        } else if (IS_DOUBLE(v)) {
            WRITE_REG(dst, DOUBLE_VAL(-AS_DOUBLE(v)));
        } else {
            runtimeError(vm, "Operand must be a number.");
            return INTERPRET_RUNTIME_ERROR;
        }
        DISPATCH();
    }
    
    op_equal: {
        uint8_t dst = READ_BYTE();
        uint8_t a = READ_BYTE();
        uint8_t b = READ_BYTE();
        WRITE_REG(dst, BOOL_VAL(valuesEqual(READ_REG(a), READ_REG(b))));
        DISPATCH();
    }
    
    op_not_equal: {
        uint8_t dst = READ_BYTE();
        uint8_t a = READ_BYTE();
        uint8_t b = READ_BYTE();
        WRITE_REG(dst, BOOL_VAL(!valuesEqual(READ_REG(a), READ_REG(b))));
        DISPATCH();
    }
    
    op_greater: {
        uint8_t dst = READ_BYTE();
        uint8_t a = READ_BYTE();
        uint8_t b = READ_BYTE();
        Value va = READ_REG(a);
        Value vb = READ_REG(b);
        if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) > AS_NUMBER(vb)));
        DISPATCH();
    }
    
    op_greater_equal: {
        uint8_t dst = READ_BYTE();
        uint8_t a = READ_BYTE();
        uint8_t b = READ_BYTE();
        Value va = READ_REG(a);
        Value vb = READ_REG(b);
        if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) >= AS_NUMBER(vb)));
        DISPATCH();
    }
    
    op_less: {
        uint8_t dst = READ_BYTE();
        uint8_t a = READ_BYTE();
        uint8_t b = READ_BYTE();
        Value va = READ_REG(a);
        Value vb = READ_REG(b);
        if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) < AS_NUMBER(vb)));
        DISPATCH();
    }
    
    op_less_equal: {
        uint8_t dst = READ_BYTE();
        uint8_t a = READ_BYTE();
        uint8_t b = READ_BYTE();
        Value va = READ_REG(a);
        Value vb = READ_REG(b);
        if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
            runtimeError(vm, "Operands must be numbers.");
            return INTERPRET_RUNTIME_ERROR;
        }
        WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) <= AS_NUMBER(vb)));
        DISPATCH();
    }
    
    op_not: {
        uint8_t dst = READ_BYTE();
        uint8_t src = READ_BYTE();
        WRITE_REG(dst, BOOL_VAL(isFalsey(READ_REG(src))));
        DISPATCH();
    }
    
    op_and: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        WRITE_REG(dst, isFalsey(v1) ? v1 : READ_REG(src2));
        DISPATCH();
    }
    
    op_or: {
        uint8_t dst = READ_BYTE();
        uint8_t src1 = READ_BYTE();
        uint8_t src2 = READ_BYTE();
        Value v1 = READ_REG(src1);
        WRITE_REG(dst, isFalsey(v1) ? READ_REG(src2) : v1);
        DISPATCH();
    }
    
    op_jump: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        DISPATCH();
    }
    
    op_jump_if_false: {
        uint16_t offset = READ_SHORT();
        uint8_t reg = READ_BYTE();
        if (isFalsey(READ_REG(reg))) {
            frame->ip += offset;
        }
        DISPATCH();
    }
    
    op_jump_if_true: {
        uint16_t offset = READ_SHORT();
        uint8_t reg = READ_BYTE();
        if (!isFalsey(READ_REG(reg))) {
            frame->ip += offset;
        }
        DISPATCH();
    }
    
    op_loop: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        DISPATCH();
    }
    
    op_call: {
        uint8_t dst = READ_BYTE();
        uint8_t fnReg = READ_BYTE();
        uint8_t argCount = READ_BYTE();
        uint8_t firstArgReg = READ_BYTE();
        
        if (!callValue(vm, frame, READ_REG(fnReg), argCount, firstArgReg, dst)) {
            return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm->frames[vm->frameCount - 1];
        DISPATCH();
    }
    
    op_closure: {
        uint8_t reg = READ_BYTE();
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(vm, function);
        WRITE_REG(reg, OBJ_VAL(closure));
        
        for (int i = 0; i < closure->upvalueCount; i++) {
            uint8_t isLocal = READ_BYTE();
            uint8_t index = READ_BYTE();
            if (isLocal) {
                closure->upvalues[i] = captureUpvalue(vm, &frame->regs[index]);
            } else {
                closure->upvalues[i] = frame->closure->upvalues[index];
            }
        }
        DISPATCH();
    }
    
    op_return: {
        uint8_t reg = READ_BYTE();
        Value result = READ_REG(reg);
        Value* frameRegs = frame->regs;
        
        closeUpvalues(vm, frameRegs);
        uint8_t retReg = frame->returnReg;
        vm->frameCount--;
        vm->stackTop = frameRegs;
        if (vm->frameCount == 0) {
            return INTERPRET_OK;
        }
        
        frame = &vm->frames[vm->frameCount - 1];
        WRITE_REG(retReg, result);
        DISPATCH();
    }
    
    op_print: {
        uint8_t reg = READ_BYTE();
        printValue(READ_REG(reg));
        printf("\n");
        DISPATCH();
    }
    
    op_pop: {
        // No-op in register VM
        DISPATCH();
    }
    
#else
    // Fallback to switch-based dispatch
    
    CallFrame* frame = &vm->frames[vm->frameCount - 1];
    
    /* the fallback path needs its own macro set */
    #define READ_BYTE()   (*frame->ip++)
    #define READ_SHORT()  (frame->ip += 2, \
                           (uint16_t)(((uint16_t)frame->ip[-2] << 8) | \
                                      (uint16_t)frame->ip[-1]))
    #define READ_CONSTANT() (frame->closure->function->constants[READ_SHORT()])
    #define READ_REG(r)   (VALIDATE_REG(vm, frame, r), frame->regs[r])
    #define WRITE_REG(r,v) (VALIDATE_REG(vm, frame, r), frame->regs[r] = (v))

    for (;;) {
        uint8_t instruction = READ_BYTE();
        switch (instruction) {
            case OP_NIL: {
                uint8_t reg = READ_BYTE();
                WRITE_REG(reg, NIL_VAL());
                continue;
            }
            case OP_TRUE: {
                uint8_t reg = READ_BYTE();
                WRITE_REG(reg, BOOL_VAL(true));
                continue;
            }
            case OP_FALSE: {
                uint8_t reg = READ_BYTE();
                WRITE_REG(reg, BOOL_VAL(false));
                continue;
            }
            case OP_CONST: {
                uint8_t reg = READ_BYTE();
                uint16_t constIndex = READ_SHORT();
                if (constIndex >= frame->closure->function->constantCount) {
                    runtimeError(vm, "Invalid constant index.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(reg, frame->closure->function->constants[constIndex]);
                continue;
            }
            case OP_MOVE: {
                uint8_t dst = READ_BYTE();
                uint8_t src = READ_BYTE();
                WRITE_REG(dst, READ_REG(src));
                continue;
            }
            case OP_LOAD_GLOBAL: {
                uint8_t reg = READ_BYTE();
                ObjString* name = AS_STRING(READ_CONSTANT());
                Value value;
                if (!tableGet(&vm->globals, name, &value)) {
                    runtimeError(vm, "Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(reg, value);
                continue;
            }
            case OP_STORE_GLOBAL: {
                ObjString* name = AS_STRING(READ_CONSTANT());
                uint8_t reg = READ_BYTE();
                tableSet(vm, &vm->globals, name, READ_REG(reg));
                continue;
            }
            case OP_LOAD_UPVALUE: {
                uint8_t reg = READ_BYTE();
                uint8_t slot = READ_BYTE();
                WRITE_REG(reg, *frame->closure->upvalues[slot]->location);
                continue;
            }
            case OP_STORE_UPVALUE: {
                uint8_t slot = READ_BYTE();
                uint8_t reg = READ_BYTE();
                *frame->closure->upvalues[slot]->location = READ_REG(reg);
                continue;
            }
            case OP_CLOSE_UPVALUE: {
                uint8_t reg = READ_BYTE();
                closeUpvalues(vm, frame->regs + reg);
                continue;
            }
            case OP_GET_PROPERTY: {
                uint8_t dst = READ_BYTE();
                uint8_t obj = READ_BYTE();
                uint16_t picIndex = READ_SHORT();
                uint8_t* picIndexPtr = frame->ip - 2;
                ObjString* name = AS_STRING(READ_CONSTANT());

                bool hasPic = picIndex != PIC_UNINITIALISED;
                Value receiver = READ_REG(obj);
                if (!IS_OBJECT(receiver)) {
                    runtimeError(vm, "Only objects have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjObject* object = AS_OBJECT(receiver);
                Table* fields = &object->map;
                Value value;

                if (hasPic) {
                    PIC* pic = &vm->picCache[picIndex];
                    Value* location;
                    if (picLookup(pic, fields, fields->version, &location)) {
                        WRITE_REG(dst, *location);
                        continue;
                    }
                }

                if (tableGet(fields, name, &value)) {
                    WRITE_REG(dst, value);
                    cachePropertyLocation(vm, fields, name, hasPic, picIndex, picIndexPtr);
                    continue;
                }

                runtimeError(vm, "Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            case OP_SET_PROPERTY: {
                uint8_t obj = READ_BYTE();
                uint16_t picIndex = READ_SHORT();
                uint8_t* picIndexPtr = frame->ip - 2;
                ObjString* name = AS_STRING(READ_CONSTANT());
                uint8_t valReg = READ_BYTE();

                bool hasPic = picIndex != PIC_UNINITIALISED;
                Value receiver = READ_REG(obj);
                if (!IS_OBJECT(receiver)) {
                    runtimeError(vm, "Only objects have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjObject* object = AS_OBJECT(receiver);
                Table* fields = &object->map;

                if (hasPic) {
                    PIC* pic = &vm->picCache[picIndex];
                    Value* location;
                    if (picLookup(pic, fields, fields->version, &location)) {
                        *location = READ_REG(valReg);
                        fields->version++;
                        continue;
                    }
                }

                tableSet(vm, fields, name, READ_REG(valReg));
                cachePropertyLocation(vm, fields, name, hasPic, picIndex, picIndexPtr);
                continue;
            }
            case OP_ADD: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                Value v2 = READ_REG(src2);
                if (IS_INT(v1) && IS_INT(v2)) {
                    int64_t a = AS_INT(v1);
                    int64_t b = AS_INT(v2);
                    if ((b > 0 && a > INT64_MAX - b) || (b < 0 && a < INT64_MIN - b)) {
                        WRITE_REG(dst, DOUBLE_VAL((double)a + (double)b));
                    } else {
                        WRITE_REG(dst, INT_VAL(a + b));
                    }
                } else if (IS_NUMERIC(v1) && IS_NUMERIC(v2)) {
                    WRITE_REG(dst, DOUBLE_VAL(AS_NUMBER(v1) + AS_NUMBER(v2)));
                } else {
                    frame->ip -= 3;
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                continue;
            }
            case OP_SUBTRACT: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                Value v2 = READ_REG(src2);
                if (IS_INT(v1) && IS_INT(v2)) {
                    int64_t a = AS_INT(v1);
                    int64_t b = AS_INT(v2);
                    if ((b < 0 && a > INT64_MAX + b) || (b > 0 && a < INT64_MIN + b)) {
                        WRITE_REG(dst, DOUBLE_VAL((double)a - (double)b));
                    } else {
                        WRITE_REG(dst, INT_VAL(a - b));
                    }
                } else if (IS_NUMERIC(v1) && IS_NUMERIC(v2)) {
                    WRITE_REG(dst, DOUBLE_VAL(AS_NUMBER(v1) - AS_NUMBER(v2)));
                } else {
                    frame->ip -= 3;
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                continue;
            }
            case OP_MULTIPLY: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                Value v2 = READ_REG(src2);
                if (IS_INT(v1) && IS_INT(v2)) {
                    int64_t a = AS_INT(v1);
                    int64_t b = AS_INT(v2);
                    if (a == 0 || b == 0) {
                        WRITE_REG(dst, INT_VAL(0));
                    } else {
                        bool overflow = false;
                        if ((a > 0) == (b > 0)) {
                            overflow = a > INT64_MAX / b;
                        } else {
                            overflow = a < INT64_MIN / b;
                        }
                        if (overflow) {
                            WRITE_REG(dst, DOUBLE_VAL((double)a * (double)b));
                        } else {
                            WRITE_REG(dst, INT_VAL(a * b));
                        }
                    }
                } else if (IS_NUMERIC(v1) && IS_NUMERIC(v2)) {
                    WRITE_REG(dst, DOUBLE_VAL(AS_NUMBER(v1) * AS_NUMBER(v2)));
                } else {
                    frame->ip -= 3;
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                continue;
            }
            case OP_DIVIDE: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                Value v2 = READ_REG(src2);
                if (!IS_NUMERIC(v1) || !IS_NUMERIC(v2)) {
                    frame->ip -= 3;
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                double a = AS_NUMBER(v1);
                double b = AS_NUMBER(v2);
                if (b == 0.0) {
                    frame->ip -= 3;
                    runtimeError(vm, "Division by zero.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(dst, DOUBLE_VAL(a / b));
                continue;
            }
            case OP_MODULO: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                Value v2 = READ_REG(src2);
                if (!IS_NUMERIC(v1) || !IS_NUMERIC(v2)) {
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                double a = AS_NUMBER(v1);
                double b = AS_NUMBER(v2);
                if (b == 0) {
                    runtimeError(vm, "Modulo by zero.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                double result = fmod(a, b);
                if (result != 0 && (a < 0) != (b < 0)) {
                    result += b;
                }
                WRITE_REG(dst, DOUBLE_VAL(result));
                continue;
            }
            case OP_NEGATE: {
                uint8_t dst = READ_BYTE();
                uint8_t src = READ_BYTE();
                Value v = READ_REG(src);
                if (IS_INT(v)) {
                    int64_t a = AS_INT(v);
                    if (a == INT64_MIN) {
                        WRITE_REG(dst, DOUBLE_VAL(-(double)a));
                    } else {
                        WRITE_REG(dst, INT_VAL(-a));
                    }
                } else if (IS_DOUBLE(v)) {
                    WRITE_REG(dst, DOUBLE_VAL(-AS_DOUBLE(v)));
                } else {
                    runtimeError(vm, "Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                continue;
            }
            case OP_EQUAL: {
                uint8_t dst = READ_BYTE();
                uint8_t a = READ_BYTE();
                uint8_t b = READ_BYTE();
                WRITE_REG(dst, BOOL_VAL(valuesEqual(READ_REG(a), READ_REG(b))));
                continue;
            }
            case OP_NOT_EQUAL: {
                uint8_t dst = READ_BYTE();
                uint8_t a = READ_BYTE();
                uint8_t b = READ_BYTE();
                WRITE_REG(dst, BOOL_VAL(!valuesEqual(READ_REG(a), READ_REG(b))));
                continue;
            }
            case OP_GREATER: {
                uint8_t dst = READ_BYTE();
                uint8_t a = READ_BYTE();
                uint8_t b = READ_BYTE();
                Value va = READ_REG(a);
                Value vb = READ_REG(b);
                if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) > AS_NUMBER(vb)));
                continue;
            }
            case OP_GREATER_EQUAL: {
                uint8_t dst = READ_BYTE();
                uint8_t a = READ_BYTE();
                uint8_t b = READ_BYTE();
                Value va = READ_REG(a);
                Value vb = READ_REG(b);
                if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) >= AS_NUMBER(vb)));
                continue;
            }
            case OP_LESS: {
                uint8_t dst = READ_BYTE();
                uint8_t a = READ_BYTE();
                uint8_t b = READ_BYTE();
                Value va = READ_REG(a);
                Value vb = READ_REG(b);
                if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) < AS_NUMBER(vb)));
                continue;
            }
            case OP_LESS_EQUAL: {
                uint8_t dst = READ_BYTE();
                uint8_t a = READ_BYTE();
                uint8_t b = READ_BYTE();
                Value va = READ_REG(a);
                Value vb = READ_REG(b);
                if (!IS_NUMERIC(va) || !IS_NUMERIC(vb)) {
                    runtimeError(vm, "Operands must be numbers.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                WRITE_REG(dst, BOOL_VAL(AS_NUMBER(va) <= AS_NUMBER(vb)));
                continue;
            }
            case OP_NOT: {
                uint8_t dst = READ_BYTE();
                uint8_t src = READ_BYTE();
                WRITE_REG(dst, BOOL_VAL(isFalsey(READ_REG(src))));
                continue;
            }
            case OP_AND: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                WRITE_REG(dst, isFalsey(v1) ? v1 : READ_REG(src2));
                continue;
            }
            case OP_OR: {
                uint8_t dst = READ_BYTE();
                uint8_t src1 = READ_BYTE();
                uint8_t src2 = READ_BYTE();
                Value v1 = READ_REG(src1);
                WRITE_REG(dst, isFalsey(v1) ? READ_REG(src2) : v1);
                continue;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                continue;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                uint8_t reg = READ_BYTE();
                if (isFalsey(READ_REG(reg))) {
                    frame->ip += offset;
                }
                continue;
            }
            case OP_JUMP_IF_TRUE: {
                uint16_t offset = READ_SHORT();
                uint8_t reg = READ_BYTE();
                if (!isFalsey(READ_REG(reg))) {
                    frame->ip += offset;
                }
                continue;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                continue;
            }
            case OP_CALL: {
                uint8_t dst = READ_BYTE();
                uint8_t fnReg = READ_BYTE();
                uint8_t argCount = READ_BYTE();
                uint8_t firstArgReg = READ_BYTE();

                if (!callValue(vm, frame, READ_REG(fnReg), argCount, firstArgReg, dst)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm->frames[vm->frameCount - 1];
                continue;
            }
            case OP_CLOSURE: {
                uint8_t reg = READ_BYTE();
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(vm, function);
                WRITE_REG(reg, OBJ_VAL(closure));

                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(vm, &frame->regs[index]);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                continue;
            }
            case OP_RETURN: {
                uint8_t reg = READ_BYTE();
                Value result = READ_REG(reg);
                Value* frameRegs = frame->regs;
                
                closeUpvalues(vm, frameRegs);
                uint8_t retReg = frame->returnReg;
                vm->frameCount--;
                vm->stackTop = frameRegs;
                if (vm->frameCount == 0) {
                    return INTERPRET_OK;
                }

                frame = &vm->frames[vm->frameCount - 1];
                WRITE_REG(retReg, result);
                continue;
            }
            case OP_PRINT: {
                uint8_t reg = READ_BYTE();
                printValue(READ_REG(reg));
                printf("\n");
                continue;
            }
            case OP_POP: {
                continue;
            }
            default: {
                runtimeError(vm, "Unknown opcode %u.", instruction);
                return INTERPRET_RUNTIME_ERROR;
            }
        }
    }
    
#endif
    
    #undef READ_BYTE
    #undef READ_SHORT
    #undef READ_CONSTANT
    #undef READ_REG
    #undef WRITE_REG
    #undef DISPATCH
    
    return INTERPRET_OK;
}

// Helper functions
static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value)) ||
           (IS_INT(value) && AS_INT(value) == 0) ||
           (IS_DOUBLE(value) && AS_DOUBLE(value) == 0.0);
}

static bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) {
        if (IS_NUMERIC(a) && IS_NUMERIC(b)) {
            return AS_NUMBER(a) == AS_NUMBER(b);
        }
        return false;
    }
    switch (a.type) {
        case VAL_BOOL: return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL: return true;
        case VAL_INT: return AS_INT(a) == AS_INT(b);
        case VAL_DOUBLE: return AS_DOUBLE(a) == AS_DOUBLE(b);
        case VAL_BUILTIN: return AS_BUILTIN(a) == AS_BUILTIN(b);
        case VAL_OBJ: {
            if (OBJ_TYPE(a) != OBJ_TYPE(b)) return false;
            if (OBJ_TYPE(a) == OBJ_STRING) {
                ObjString* as = AS_STRING(a);
                ObjString* bs = AS_STRING(b);
                return as->length == bs->length && memcmp(as->chars, bs->chars, as->length) == 0;
            }
            return AS_OBJ(a) == AS_OBJ(b);
        }
    }
    return false;
}

static bool call(VM* vm, ObjClosure* closure, uint8_t argCount, uint8_t firstArgReg, uint8_t returnReg) {
    if (argCount != closure->function->arity) {
        runtimeError(vm, "Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm->frameCount == MAX_CALL_FRAMES) {
        runtimeError(vm, "Stack overflow.");
        return false;
    }

    /* precise stack‑usage check */
    size_t used = (size_t)(vm->stackTop - vm->stack);
    if (used + (size_t)closure->function->regCount >
        (size_t)(MAX_REGS * MAX_CALL_FRAMES)) {
        runtimeError(vm, "Stack overflow.");
        return false;
    }

    CallFrame* prevFrame = vm->frameCount > 0 ? &vm->frames[vm->frameCount - 1] : NULL;
    if (prevFrame && (size_t)firstArgReg + argCount >
                     (size_t)prevFrame->closure->function->regCount) {
        runtimeError(vm, "Invalid argument register range.");
        return false;
    }

    CallFrame* frame = &vm->frames[vm->frameCount++];

    frame->closure = closure;
    frame->ip = closure->function->code;
    frame->regs = vm->stackTop;
    frame->returnReg = returnReg;

    for (int i = 0; i < closure->function->regCount; i++) {
        frame->regs[i] = NIL_VAL();
    }
    vm->stackTop += closure->function->regCount;

    if (prevFrame && argCount) {
        memmove(frame->regs,
                prevFrame->regs + firstArgReg,
                argCount * sizeof(Value));
    }

    return true;
}

static bool callValue(VM* vm,
                      CallFrame* callerFrame,
                      Value callee,
                      uint8_t argCount,
                      uint8_t firstArgReg,
                      uint8_t returnReg)
{
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_CLOSURE: {
                return call(vm, AS_CLOSURE(callee), argCount, firstArgReg, returnReg);
            }
            default: break;
        }
    } else if (IS_BUILTIN(callee)) {
        BuiltinFn fn = AS_BUILTIN(callee);
        Value* args = NULL;
        if (argCount > 0) {
            args = malloc(argCount * sizeof(Value));
            if (args == NULL) {
                runtimeError(vm, "Out of memory.");
                return false;
            }
            for (uint8_t i = 0; i < argCount; i++) {
                args[i] = callerFrame->regs[firstArgReg + i];
            }
        }
        Value result = fn(vm, argCount, args);
        free(args);
        if (vm->had_error) {
            return false;
        }
        callerFrame->regs[returnReg] = result;
        return true;
    }
    runtimeError(vm, "Can only call functions and classes.");
    return false;
}

static ObjUpvalue* captureUpvalue(VM* vm, Value* local) {
    ObjUpvalue* prev = NULL;
    ObjUpvalue* upvalue = vm->openUpvalues;
    
    while (upvalue && upvalue->location > local) {
        prev = upvalue;
        upvalue = upvalue->next;
    }
    
    if (upvalue && upvalue->location == local) return upvalue;
    
    ObjUpvalue* created = newUpvalue(vm, local);
    created->next = upvalue;
    
    if (prev == NULL) {
        vm->openUpvalues = created;
    } else {
        prev->next = created;
    }
    
    return created;
}

static void closeUpvalues(VM* vm, Value* last) {
    while (vm->openUpvalues && vm->openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm->openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm->openUpvalues = upvalue->next;
    }
}

static void collect_garbage(VM* vm) {
    mark_roots(vm);
    trace_references(vm);
    table_remove_white(vm, &vm->strings);
    sweep(vm);
    vm->nextGC = vm->bytesAllocated * 2;
}

static void mark_roots(VM* vm) {
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
        mark_value(vm, *slot);
    }

    for (int i = 0; i < vm->frameCount; i++) {
        mark_object(vm, (Obj*)vm->frames[i].closure);
    }

    for (ObjUpvalue* upvalue = vm->openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        mark_object(vm, (Obj*)upvalue);
    }

    mark_table(vm, &vm->globals);
    mark_table(vm, &vm->strings);
    mark_object(vm, (Obj*)vm->env);
    mark_table(vm, &vm->interner);
    for (int i = 0; i < vm->root_stack_count; i++) {
        mark_object(vm, vm->root_stack[i]);
    }
}

static void trace_references(VM* vm) {
    while (vm->grayCount > 0) {
        Obj* object = vm->grayStack[--vm->grayCount];
        blacken_object(vm, object);
    }
}

static void sweep(VM* vm) {
    Obj* previous = NULL;
    Obj* object = vm->objects;
    while (object != NULL) {
        if (object->is_marked) {
            object->is_marked = false;
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm->objects = object;
            }
            free_object(vm, unreached);
        }
    }
}

static void mark_object(VM* vm, Obj* object) {
    if (object == NULL || object->is_marked) return;
    object->is_marked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        vm->grayStack = (Obj**)realloc(vm->grayStack, sizeof(Obj*) * vm->grayCapacity);
        if (vm->grayStack == NULL) exit(1);
    }
    vm->grayStack[vm->grayCount++] = object;
}

static void mark_value(VM* vm, Value value) {
    if (IS_OBJ(value)) mark_object(vm, AS_OBJ(value));
}

static void blacken_object(VM* vm, Obj* object) {
    switch (object->type) {
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            mark_object(vm, (Obj*)function->name);
            for (int i = 0; i < function->constantCount; i++) {
                mark_value(vm, function->constants[i]);
            }
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            mark_object(vm, (Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                mark_object(vm, (Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_UPVALUE: {
            ObjUpvalue* upvalue = (ObjUpvalue*)object;
            mark_value(vm, upvalue->closed);
            break;
        }
        case OBJ_ARRAY: {
            ObjArray* array = (ObjArray*)object;
            for (int i = 0; i < array->count; i++) {
                mark_value(vm, array->values[i]);
            }
            break;
        }
        case OBJ_OBJECT: {
            ObjObject* obj = (ObjObject*)object;
            mark_table(vm, &obj->map);
            break;
        }
        case OBJ_STRING: break;
        case OBJ_ENV: {
            ObjEnv* env = (ObjEnv*)object;
            mark_object(vm, (Obj*)env->parent);
            mark_table(vm, &env->table);
            break;
        }
        case OBJ_RESOURCE: break;
    }
}

