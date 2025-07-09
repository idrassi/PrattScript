# PrattScript – A Modern Dynamic Scripting Language with Pratt Parser Core

**PrattScript** is a lightweight, dynamically typed scripting language designed for embedding, rapid prototyping, and language research. It features first-class functions, arrays, objects, and a robust garbage collector. At its heart is a C99 implementation of Pratt's top-down operator-precedence parsing algorithm, enabling expressive and extensible syntax with explicit binding-power rules.

PrattScript is both a language and a library: you get a ready-to-use interpreter and a reusable Pratt parser core for building your own languages or DSLs.

---

## Key Features

- **Dynamic Scripting Language**  
  - First-class functions, closures, arrays, objects, and a flexible type system.
  - Familiar C-like syntax with modern conveniences.
  - REPL and script file execution.

- **Pratt Parser Core**  
  - Powerful, extensible Pratt parser for expressions (operator precedence, associativity, custom operators).
  - Recursive descent for statements, seamlessly integrated with the Pratt parser for expressions.
  - Pluggable lexers and parse tables for custom language extensions.

- **Robust Runtime**  
  - Arena allocator for fast, cache-friendly memory management of ASTs.
  - String interning for efficient string handling.
  - Mark-sweep garbage collector for automatic memory management.
  - Hash maps and dynamic arrays as core types.

- **Embeddable and Extensible**  
  - Written in portable C99, zero external dependencies.
  - Easy to embed in C or C++ projects.
  - Customizable memory allocation and error handling.
  - Attach semantic information to AST nodes and statements for advanced tooling.

- **Comprehensive Test Suite**  
  - 90+ focused tests covering parsing, evaluation, and language features.

- **Permissive License**  
  - MIT license for commercial and open-source use.

---

## Getting Started

### Build and Run the PrattScript Interpreter

```bash
git clone https://github.com/idrassi/PrattScript
cd PrattScript
cmake -B build
cmake --build build
./build/prattscript
```

You'll be greeted by a REPL where you can experiment with the language:

```
PrattScript Interpreter Demo. Enter statements or 'exit'.
> var message = "hello";
> print(upper(message) + " WORLD");
"HELLO WORLD"
> var i = 0;
> while (i < 3) { print(i); i = i + 1; }
0
1
2
> if (i == 3) { print("done"); }
"done"
> exit
```

You can also run PrattScript source files:

```bash
./build/prattscript myscript.pratt
```

Or execute code directly:

```bash
./build/prattscript -c "print(1 + 2 * 3);"
```

---

## Language Overview

- **Variables:** `var x = 42;`
- **Functions:** 
    ```js
    function add(a, b) { return a + b; }
    print(add(2, 3));
    ```
- **Control Flow:** `if`, `else`, `while`, `return`
- **Arrays and Objects:**  
    ```js
    var arr = [1, 2, 3];
    var obj = { name: "Pratt", year: 2025 };
    ```
- **Built-in Functions:** `print`, `println`, `upper`, `lower`, `len`, `push`, `pop`, etc.
- **Garbage Collection:** Automatic, with manual control via the `gc` object.

---

## Embedding and Extending

PrattScript is designed to be embedded in your C/C++ applications. You can:

- Use the Pratt parser core to build your own language or DSL.
- Extend the interpreter with custom built-in functions.
- Integrate with your own memory allocators by defining `PRATT_MALLOC`, `PRATT_REALLOC`, and `PRATT_FREE` in `pratt_config.h`.

See the `pratt.h` and `interpreter_core.h` headers for API details.

---

## FAQ

> **How do statements and expressions work together?**  
> PrattScript uses a hybrid approach: a Pratt parser for expressions (handling operator precedence and associativity) and recursive descent for statements. You can extend both levels for your own needs.

> **Is it thread-safe?**  
> Re-entrant, yes. Thread-safe, no—use one `Parser` per thread or add your own external locking.

> **Can I generate code from the AST?**  
> Yes. The `user_data` field on every `ASTNode` and `Statement` lets you attach semantic information (like types or resolved symbols) during later compiler passes.

---

## License

PrattScript is released under the **MIT License**.  
See [`LICENSE`](./LICENSE) for the full text.

---

© 2025 Mounir IDRASSI <mounir.idrassi@amcrypto.jp>
