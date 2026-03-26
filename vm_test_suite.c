#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vm.h"

static int tests_run = 0;
static int tests_failed = 0;

#define ASSERT(condition, format, ...)                                      \
    do {                                                                    \
        if (!(condition)) {                                                 \
            fprintf(stderr, "Assertion failed at %s:%d: " format "\n",      \
                    __FILE__, __LINE__, ##__VA_ARGS__);                     \
            return false;                                                   \
        }                                                                   \
    } while (0)

#define RUN_TEST(fn)                                                        \
    do {                                                                    \
        tests_run++;                                                        \
        if (fn()) {                                                         \
            printf("PASS %s\n", #fn);                                       \
        } else {                                                            \
            tests_failed++;                                                 \
            printf("FAIL %s\n", #fn);                                       \
        }                                                                   \
    } while (0)

static Obj* allocObject(VM* vm, size_t size, ObjType type) {
    Obj* object = (Obj*)calloc(1, size);
    if (object == NULL) {
        fprintf(stderr, "Out of memory in test harness.\n");
        abort();
    }
    object->type = type;
    object->next = vm->objects;
    vm->objects = object;
    return object;
}

#include "vm.c"

void initTable(Table* table) {
    memset(table, 0, sizeof(*table));
}

void freeTable(VM* vm, Table* table) {
    (void)vm;
    free(table->entries);
    memset(table, 0, sizeof(*table));
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    (void)table;
    (void)key;
    (void)value;
    return false;
}

bool tableSet(VM* vm, Table* table, ObjString* key, Value value) {
    (void)vm;
    (void)key;
    (void)value;
    table->version++;
    return true;
}

bool tableDelete(Table* table, ObjString* key) {
    (void)table;
    (void)key;
    return false;
}

Entry* tableFindEntry(Entry* entries, int capacity, ObjString* key) {
    (void)entries;
    (void)capacity;
    (void)key;
    return NULL;
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    (void)table;
    (void)chars;
    (void)length;
    (void)hash;
    return NULL;
}

ObjClosure* newClosure(VM* vm, ObjFunction* function) {
    ObjClosure* closure = (ObjClosure*)allocObject(vm, sizeof(ObjClosure), OBJ_CLOSURE);
    closure->function = function;
    closure->upvalueCount = function->upvalueCount;
    if (function->upvalueCount > 0) {
        closure->upvalues = (ObjUpvalue**)calloc((size_t)function->upvalueCount, sizeof(ObjUpvalue*));
        if (closure->upvalues == NULL) {
            fprintf(stderr, "Out of memory in test harness.\n");
            abort();
        }
    }
    return closure;
}

ObjUpvalue* newUpvalue(VM* vm, Value* slot) {
    ObjUpvalue* upvalue = (ObjUpvalue*)allocObject(vm, sizeof(ObjUpvalue), OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->closed = NIL_VAL();
    upvalue->next = NULL;
    return upvalue;
}

ObjArray* newArray(VM* vm) {
    return (ObjArray*)allocObject(vm, sizeof(ObjArray), OBJ_ARRAY);
}

ObjObject* newObject(VM* vm) {
    ObjObject* object = (ObjObject*)allocObject(vm, sizeof(ObjObject), OBJ_OBJECT);
    initTable(&object->map);
    return object;
}

ObjEnv* new_env_obj(VM* vm, ObjEnv* parent) {
    ObjEnv* env = (ObjEnv*)allocObject(vm, sizeof(ObjEnv), OBJ_ENV);
    env->parent = parent;
    initTable(&env->table);
    return env;
}

ObjResource* newResource(VM* vm, void* context, const ResourceVTable* vtable) {
    ObjResource* resource = (ObjResource*)allocObject(vm, sizeof(ObjResource), OBJ_RESOURCE);
    resource->context = context;
    resource->vtable = vtable;
    resource->is_finalized = false;
    return resource;
}

void printValue(Value value) {
    switch (value.type) {
        case VAL_NIL:
            fputs("nil", stdout);
            break;
        case VAL_BOOL:
            fputs(AS_BOOL(value) ? "true" : "false", stdout);
            break;
        case VAL_INT:
            fprintf(stdout, "%" PRId64, AS_INT(value));
            break;
        case VAL_DOUBLE:
            fprintf(stdout, "%g", AS_DOUBLE(value));
            break;
        case VAL_BUILTIN:
            fputs("<builtin>", stdout);
            break;
        case VAL_OBJ:
            fputs("<object>", stdout);
            break;
        case VAL_TOMBSTONE:
            fputs("<tombstone>", stdout);
            break;
    }
}

void free_object(VM* vm, Obj* object) {
    (void)vm;
    switch (object->type) {
        case OBJ_CLOSURE:
            free(((ObjClosure*)object)->upvalues);
            break;
        case OBJ_OBJECT:
            free(((ObjObject*)object)->map.entries);
            break;
        case OBJ_ENV:
            free(((ObjEnv*)object)->table.entries);
            break;
        case OBJ_RESOURCE: {
            ObjResource* resource = (ObjResource*)object;
            if (!resource->is_finalized && resource->vtable != NULL &&
                resource->vtable->finalize != NULL) {
                resource->vtable->finalize(resource->context);
            }
            break;
        }
        default:
            break;
    }
    free(object);
}

void freeObjects(VM* vm) {
    Obj* object = vm->objects;
    while (object != NULL) {
        Obj* next = object->next;
        free_object(vm, object);
        object = next;
    }
    vm->objects = NULL;
}

ObjString* copyString(VM* vm, const char* chars, int length) {
    ObjString* string = (ObjString*)allocObject(vm, sizeof(ObjString) + (size_t)length + 1, OBJ_STRING);
    string->length = (uint32_t)length;
    string->hash = 0;
    memcpy(string->chars, chars, (size_t)length);
    string->chars[length] = '\0';
    return string;
}

void table_remove_white(VM* vm, Table* table) {
    (void)vm;
    (void)table;
}

void mark_table(VM* vm, Table* table) {
    (void)vm;
    (void)table;
}

static ObjFunction makeFunction(uint8_t* code,
                                size_t codeSize,
                                Value* constants,
                                int constantCount,
                                int regCount) {
    ObjFunction function;
    memset(&function, 0, sizeof(function));
    function.obj.type = OBJ_FUNCTION;
    function.arity = 0;
    function.upvalueCount = 0;
    function.regCount = regCount;
    function.code = code;
    function.codeSize = codeSize;
    function.constants = constants;
    function.constantCount = constantCount;
    function.name = NULL;
    function.lines = NULL;
    function.lineCount = 0;
    return function;
}

static bool test_interpret_resets_execution_state_before_run(void) {
    VM vm;
    initVM(&vm);

    vm.had_error = 1;
    memcpy(vm.error_message, "stale", sizeof("stale"));
    vm.stackTop = vm.stack + 5;
    vm.stack[1] = INT_VAL(7);

    ObjUpvalue staleUpvalue;
    memset(&staleUpvalue, 0, sizeof(staleUpvalue));
    staleUpvalue.obj.type = OBJ_UPVALUE;
    staleUpvalue.location = vm.stack + 1;
    vm.openUpvalues = &staleUpvalue;

    uint8_t code[] = { OP_NIL, 0, OP_RETURN, 0 };
    ObjFunction function = makeFunction(code, sizeof(code), NULL, 0, 1);

    ASSERT(interpret(&vm, &function) == INTERPRET_OK, "expected success after reset");
    ASSERT(vm.frameCount == 0, "expected no frames after interpret");
    ASSERT(vm.stackTop == vm.stack, "expected stackTop reset to the base of the stack");
    ASSERT(vm.openUpvalues == NULL, "expected stale open upvalues to be cleared");
    ASSERT(vm.had_error == 0, "expected stale error flag to be cleared");
    ASSERT(vm.error_message[0] == '\0', "expected stale error message to be cleared");
    ASSERT(staleUpvalue.location == &staleUpvalue.closed, "expected stale upvalue to be closed");
    ASSERT(IS_INT(staleUpvalue.closed) && AS_INT(staleUpvalue.closed) == 7,
           "expected stale upvalue to preserve the captured value");

    freeVM(&vm);
    return true;
}

static bool test_root_return_rewinds_stack_after_each_run(void) {
    VM vm;
    initVM(&vm);

    uint8_t code[] = { OP_NIL, 0, OP_RETURN, 0 };
    ObjFunction function = makeFunction(code, sizeof(code), NULL, 0, 1);

    ASSERT(interpret(&vm, &function) == INTERPRET_OK, "first interpret() should succeed");
    ASSERT(vm.stackTop == vm.stack, "first interpret() should leave an empty register stack");
    ASSERT(vm.frameCount == 0, "first interpret() should leave no frames");

    ASSERT(interpret(&vm, &function) == INTERPRET_OK, "second interpret() should also succeed");
    ASSERT(vm.stackTop == vm.stack, "second interpret() should not leak register windows");
    ASSERT(vm.frameCount == 0, "second interpret() should leave no frames");

    freeVM(&vm);
    return true;
}

static bool test_runtime_error_without_line_table_is_reported_safely(void) {
    VM vm;
    initVM(&vm);

    ObjString* name = copyString(&vm, "missing", 7);
    Value constants[] = { OBJ_VAL(name) };
    uint8_t code[] = { OP_LOAD_GLOBAL, 0, 0, 0 };
    ObjFunction function = makeFunction(code, sizeof(code), constants, 1, 1);

    ASSERT(interpret(&vm, &function) == INTERPRET_RUNTIME_ERROR,
           "expected missing global to raise a runtime error");
    ASSERT(vm.had_error == 1, "expected runtime error flag to be set");

    freeVM(&vm);
    return true;
}

static bool test_call_nil_initializes_new_register_window(void) {
    VM vm;
    initVM(&vm);

    uint8_t callerCode[] = { OP_RETURN, 0 };
    ObjFunction callerFunction = makeFunction(callerCode, sizeof(callerCode), NULL, 0, 3);
    ObjClosure* callerClosure = newClosure(&vm, &callerFunction);

    ASSERT(call(&vm, callerClosure, 0, 0, 0), "expected caller frame setup to succeed");
    vm.frames[0].regs[1] = INT_VAL(42);

    for (int i = 0; i < 4; i++) {
        vm.stackTop[i] = TOMBSTONE_VAL();
    }

    uint8_t calleeCode[] = { OP_RETURN, 0 };
    ObjFunction calleeFunction = makeFunction(calleeCode, sizeof(calleeCode), NULL, 0, 4);
    calleeFunction.arity = 1;
    ObjClosure* calleeClosure = newClosure(&vm, &calleeFunction);

    ASSERT(call(&vm, calleeClosure, 1, 1, 0), "expected callee frame setup to succeed");
    ASSERT(IS_INT(vm.frames[1].regs[0]) && AS_INT(vm.frames[1].regs[0]) == 42,
           "expected argument register to be copied into the new frame");
    ASSERT(IS_NIL(vm.frames[1].regs[1]), "expected register 1 to be nil-initialized");
    ASSERT(IS_NIL(vm.frames[1].regs[2]), "expected register 2 to be nil-initialized");
    ASSERT(IS_NIL(vm.frames[1].regs[3]), "expected register 3 to be nil-initialized");

    freeVM(&vm);
    return true;
}

int main(void) {
    RUN_TEST(test_interpret_resets_execution_state_before_run);
    RUN_TEST(test_root_return_rewinds_stack_after_each_run);
    RUN_TEST(test_runtime_error_without_line_table_is_reported_safely);
    RUN_TEST(test_call_nil_initializes_new_register_window);

    printf("%d/%d tests passed\n", tests_run - tests_failed, tests_run);
    return tests_failed == 0 ? 0 : 1;
}
