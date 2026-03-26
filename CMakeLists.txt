cmake_minimum_required(VERSION 3.10)
project(PrattScript C)

enable_testing()

# Create the static library from the source files
add_library(prattlib STATIC
    pratt.c
    pratt_default.c
)

# Specify the include directories for consumers of the library
# (so pratt.h and pratt_default.h are found)
target_include_directories(prattlib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Enforce C99 for the compiler
target_compile_options(prattlib PRIVATE
    $<$<C_COMPILER_ID:GNU,Clang>:-std=c99>
)

# Optionally set C standard
set_target_properties(prattlib PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED YES)

# Create the test suite executable
add_executable(test_suite
    cJSON.c
    interpreter_core.c
    test_suite.c
    pratt_lexer.c
)

# Link the test executable with the library
target_link_libraries(test_suite prattlib)

# Set C standard for test executable
set_target_properties(test_suite PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED YES)

# Create the VM-focused test suite executable
add_executable(vm_test_suite
    vm_test_suite.c
)

# Set C standard for VM test executable
set_target_properties(vm_test_suite PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED YES)
add_test(NAME test_suite COMMAND test_suite)
add_test(NAME vm_test_suite COMMAND vm_test_suite)

# Create the interpreter executable
add_executable(prattscript
    cJSON.c
    interpreter.c
    interpreter_core.c
    pratt_lexer.c
)

target_link_libraries(prattscript prattlib)
if(UNIX OR APPLE)
    target_link_libraries(test_suite m)
    target_link_libraries(vm_test_suite m)
    target_link_libraries(prattscript m)
endif()

# Set C standard for prattscript
set_target_properties(prattscript PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED YES)

# Add CJSON_HIDE_SYMBOLS define to hide cJSON symbols
target_compile_definitions(prattlib PRIVATE CJSON_HIDE_SYMBOLS)
target_compile_definitions(test_suite PRIVATE CJSON_HIDE_SYMBOLS)
target_compile_definitions(prattscript PRIVATE CJSON_HIDE_SYMBOLS)

# Static C runtime linking on Windows
if(WIN32 AND MSVC)
    target_compile_options(prattlib PRIVATE $<$<CONFIG:Debug>:/MTd> $<$<CONFIG:Release>:/MT>)
    target_compile_options(test_suite PRIVATE $<$<CONFIG:Debug>:/MTd> $<$<CONFIG:Release>:/MT>)
    target_compile_options(prattscript PRIVATE $<$<CONFIG:Debug>:/MTd> $<$<CONFIG:Release>:/MT>)
endif()
