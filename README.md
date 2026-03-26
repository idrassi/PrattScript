# PrattScript - A Dynamic Scripting Language with a Pratt Parser Core

**PrattScript** is a lightweight, dynamically typed scripting language written in portable C99. It is designed for embedding, rapid prototyping, tooling experiments, and language research. The language has first-class functions, closures, arrays, objects, garbage collection, and a standard set of practical builtins for strings, arrays, files, paths, dates, JSON, and process interaction.

PrattScript is also a reusable parser/runtime codebase. The interpreter is built on top of a Pratt parser for expressions plus recursive-descent statement parsing, which makes the project useful both as a scripting language and as a reference implementation for building DSLs and custom languages.

---

## Key Features

- Dynamic scripting language with numbers, strings, booleans, `nil`, arrays, objects, functions, and resources
- Lexically scoped variables and closures
- C-like control flow with `if`, `while`, `for`, `break`, `continue`, and `return`
- Arrays and objects with indexing and property access
- Mixed integer/double numeric model
- Mark-sweep garbage collector and string interning
- Cross-platform runtime helpers for file I/O, paths, environment variables, dates, and JSON
- Embeddable C API and reusable Pratt parser core
- 300+ focused tests covering parsing, evaluation, runtime behavior, and builtins

---

## Getting Started

### Build

```bash
git clone https://github.com/idrassi/PrattScript
cd PrattScript
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --config Release
ctest --test-dir build -C Release --output-on-failure
```

### Run the Interpreter

On Linux and macOS:

```bash
./build/prattscript
```

On Windows:

```powershell
.\build\Release\prattscript.exe
```

You can also execute a source file:

```bash
./build/prattscript myscript.pratt
```

Or run code directly from the command line:

```bash
./build/prattscript -c "println(1 + 2 * 3);"
```

### Quick Example

```js
var message = "hello";

function greet(name) {
    return upper(message) + ", " + name;
}

var values = range(1, 5);
println(greet("PrattScript"));
println(values);
println(array.map(values, function (x) { return x * x; }));
```

---

## Language Guide

### Values and Types

PrattScript has the following runtime value categories:

- `number`
- `string`
- `boolean`
- `nil`
- `array`
- `object`
- `function`
- `builtin`
- `resource`

Use `typeof(value)` to inspect the runtime type:

```js
println(typeof(123));       // number
println(typeof("hello"));   // string
println(typeof([1, 2]));    // array
println(typeof({a: 1}));    // object
println(typeof(println));   // builtin
```

### Numbers

PrattScript uses a mixed numeric model:

- Integers are stored as signed 64-bit values when possible.
- Floating-point values use `double`.
- Arithmetic and comparisons work across both numeric forms.
- Integer overflow in some arithmetic operations promotes the result to `double`.

Examples:

```js
println(1 + 2);        // 3
println(1 / 2);        // 0.5
println(2 ** 8);       // 256
println(5 % 2);        // 1
println(1 < 2.5);      // true
```

### Variables and Scope

Variables are declared with `var`:

```js
var answer = 42;
var name = "PrattScript";
var pending;
```

Variables are lexically scoped. Blocks and `for` loops create new scopes:

```js
var i = 99;

for (var i = 0; i < 2; i = i + 1) {
    println("inner: " + i);
}

println("outer: " + i); // still 99
```

### Statements and Control Flow

PrattScript supports:

- Expression statements
- Variable declarations
- Blocks
- `if` / `else`
- `while`
- `for`
- `break`
- `continue`
- `return`
- Function declarations

Examples:

```js
if (score >= 90) {
    println("great");
} else {
    println("keep going");
}

var total = 0;
while (total < 3) {
    println(total);
    total = total + 1;
}

for (var i = 0; i < 5; i = i + 1) {
    if (i == 2) continue;
    if (i == 4) break;
    println(i);
}
```

All `for` clauses are optional:

```js
for (;;) {
    break;
}
```

### Functions and Closures

Named functions:

```js
function add(a, b) {
    return a + b;
}

println(add(2, 3));
```

Anonymous function expressions:

```js
var double = function (x) {
    return x * 2;
};

println(double(21));
```

Functions are first-class values and capture lexical scope:

```js
function makeCounter() {
    var n = 0;
    return function () {
        n = n + 1;
        return n;
    };
}

var next = makeCounter();
println(next()); // 1
println(next()); // 2
```

### Arrays

Create arrays with square brackets:

```js
var items = [10, 20, 30];
println(items[0]);
items[1] = 99;
push(items, 123);
println(pop(items));
```

Notes:

- Array indexes must be integers.
- Out-of-bounds indexing is a runtime error.
- Arrays are mutable.

### Objects

Create objects with braces:

```js
var user = {
    name: "Pratt",
    year: 2025
};

println(user.name);
println(user["year"]);

user["name"] = "PrattScript";
user.version = 1;
```

Notes:

- Object keys are strings.
- Both `obj.key` and `obj["key"]` are supported.
- Reading a missing key returns `nil`.
- Objects are mutable hash maps.

### Truthiness

The following values are falsey:

- `nil`
- `false`
- integer `0`
- floating-point `0.0`

Everything else is truthy, including:

- empty strings
- empty arrays
- empty objects
- functions
- resources

This matters for `if`, `while`, `for`, `assert`, and the ternary operator.

### Operators

Supported operators include:

- Arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical: `&&`, `||`
- Bitwise integer operators: `~`, `&`, `|`, `^`, `<<`, `>>`
- Unary minus: `-x`
- Ternary: `condition ? whenTrue : whenFalse`
- Assignment: `=`
- Indexing: `arr[i]`, `obj["key"]`
- Property access: `obj.key`
- Function calls: `fn(a, b)`

Important behavior:

- `+` performs string concatenation if either operand is a string.
- `&&` and `||` return booleans, not one of their operands.
- Assignment is an expression and evaluates to the assigned value.

### Strings

Strings are immutable and can be concatenated with `+`:

```js
var name = "Pratt" + "Script";
println(lower(name));
println(string.trim("  hello  "));
println(string.split("a,b,c", ","));
```

### Comments

PrattScript uses C-style line comments:

```js
// this is a comment
var x = 1;
```

---

## Builtin Reference

The runtime exposes a small global surface plus several namespace objects.

### Global Builtins

| Builtin | Description |
| --- | --- |
| `print(value)` | Print a value without a trailing newline. |
| `println(value)` | Print a value followed by a newline. |
| `upper(str)` | Return an uppercased copy of a string. |
| `lower(str)` | Return a lowercased copy of a string. |
| `compare(a, b)` | Compare two numbers or two strings. Returns `-1`, `0`, or `1`. |
| `len(value)` | Length of a string, array, or object key set. |
| `push(array, value)` | Append to an array and return the same array. |
| `pop(array)` | Remove and return the last array element. |
| `keys(object)` | Return an array of object keys. |
| `toString(value)` | Convert a value to a string using PrattScript formatting. |
| `toNumber(value)` | Convert strings, booleans, and `nil` to numbers. Invalid strings become `NaN`. |
| `typeof(value)` | Return a runtime type name such as `number`, `string`, `array`, or `resource`. |
| `clock()` | Return process CPU time in seconds as a floating-point number. |
| `time()` | Return the current Unix timestamp in seconds. |
| `exit([code])` | Terminate the interpreter process. |
| `assert(condition, [message])` | Raise a runtime error if the condition is falsey. |
| `range(start, end, [step])` | Create an integer array using an exclusive end bound. |
| `min(a, ...)` | Minimum of one or more numeric arguments. |
| `max(a, ...)` | Maximum of one or more numeric arguments. |
| `round(number, [digits])` | Round a number, optionally keeping decimal digits. |
| `sleep(milliseconds)` | Sleep for a number of milliseconds. |

### `gc`

Garbage collector inspection and control:

| Builtin | Description |
| --- | --- |
| `gc.collect()` | Run a collection cycle immediately. |
| `gc.allocated()` | Return currently allocated bytes tracked by the runtime. |
| `gc.next_gc()` | Return the current GC threshold in bytes. |

### `math`

Numeric helpers and random generation:

| Builtin | Description |
| --- | --- |
| `math.abs(x)` | Absolute value. |
| `math.floor(x)` | Floor. |
| `math.ceil(x)` | Ceiling. |
| `math.pow(x, y)` | Exponentiation. |
| `math.sqrt(x)` | Square root. |
| `math.sin(x)` | Sine. |
| `math.cos(x)` | Cosine. |
| `math.tan(x)` | Tangent. |
| `math.log(x)` | Natural logarithm. |
| `math.log10(x)` | Base-10 logarithm. |
| `math.random()` | Random floating-point value in `[0, 1)`. |
| `math.seedrandom(seed)` | Reset the RNG seed. |
| `math.randomInt()` | Random signed 64-bit integer. |
| `math.randomInt(min, max)` | Random integer in `[min, max)`. |
| `math.randomBytes(len)` | Return `len` random bytes packed into a string value. |

### `string`

String helpers:

| Builtin | Description |
| --- | --- |
| `string.split(str, sep)` | Split a string into an array. An empty separator splits into characters. |
| `string.trim(str)` | Trim leading and trailing whitespace. |
| `string.replace(str, old, new, [count])` | Replace all or the first `count` matches. |
| `string.startsWith(str, prefix)` | Prefix check. |
| `string.endsWith(str, suffix)` | Suffix check. |
| `string.indexOf(str, needle, [from])` | Zero-based index lookup. Returns `-1` when not found. |
| `string.substring(str, start, end)` | Slice by index range. |
| `string.repeat(str, count)` | Repeat a string. |
| `string.padStart(str, length, [pad])` | Left-pad a string. |
| `string.padEnd(str, length, [pad])` | Right-pad a string. |

### `array`

Array helpers:

| Builtin | Description |
| --- | --- |
| `array.slice(arr, start, [end])` | Non-mutating slice with support for negative indexes. |
| `array.join(arr, [sep])` | Join elements into a string. |
| `array.map(arr, fn)` | Return a new array with `fn(value)` applied to each item. |
| `array.filter(arr, fn)` | Return items for which `fn(value)` is truthy. |
| `array.reduce(arr, fn, [init])` | Reduce left-to-right. |
| `array.indexOf(arr, value)` | Return the first matching index or `-1`. |
| `array.includes(arr, value)` | Membership test. |
| `array.sort(arr, [fn])` | Sort in place and return the same array. |
| `array.reverse(arr)` | Reverse in place and return the same array. |
| `array.shuffle(arr)` | Shuffle in place and return the same array. |

Comparator functions passed to `array.sort` should return a negative number, zero, or a positive number.

### `fs`

File-system helpers:

| Builtin | Description |
| --- | --- |
| `fs.readFile(path, [options])` | Read a whole file and return a string. |
| `fs.writeFile(path, data, [options])` | Write a full file. Returns `true` on success. |
| `fs.exists(path)` | Return whether the path exists. |
| `fs.listDir(path)` | Return directory entries as an array of names. |
| `fs.remove(path)` | Remove a file or empty directory. |
| `fs.open(path, mode)` | Open a file and return a resource, or `nil` on failure. |
| `fs.read(resource, numBytes)` | Read a byte count from an open file resource. |
| `fs.write(resource, data)` | Write data to an open file resource and return bytes written. |
| `fs.close(resource)` | Alias for `resource.close(resource)`. |

`fs.readFile` options:

- `"text"` or `"binary"` as the second argument
- `{ mode: "text" }`
- `{ mode: "binary" }`

`fs.writeFile` options:

- `true` as the third argument to append
- `{ append: true }`
- `{ mode: "binary" }`
- `{ mode: "binary", append: true }`

Examples:

```js
fs.writeFile("hello.txt", "hello");
println(fs.readFile("hello.txt"));

var f = fs.open("hello.txt", "r");
println(fs.read(f, 5));
resource.close(f);
```

### `path`

Cross-platform path helpers:

| Builtin | Description |
| --- | --- |
| `path.join(a, b, ...)` | Join path parts using the platform separator. |
| `path.basename(path)` | Last path component. |
| `path.dirname(path)` | Parent directory portion. |
| `path.extname(path)` | Extension, including the leading dot when present. |

### `os`

Process and environment helpers:

| Builtin | Description |
| --- | --- |
| `os.getenv(name)` | Read an environment variable. Returns `nil` if missing. |
| `os.setenv(name, value)` | Set an environment variable. |
| `os.unsetenv(name)` | Remove an environment variable. |
| `os.exec(command)` | Execute a shell command and return its exit code. |
| `os.getcwd()` | Return the current working directory. |
| `os.setcwd(path)` | Change the current working directory. |
| `os.platform()` | Return a platform string such as `windows`, `linux`, or `macos`. |

### `date`

Timestamp helpers:

| Builtin | Description |
| --- | --- |
| `date.now()` | Current Unix timestamp in seconds. |
| `date.format(timestamp, [format])` | Format a timestamp using `strftime` syntax. |
| `date.parse(str, format)` | Parse a timestamp string using `strptime` syntax when available on the platform. |
| `date.utc(timestamp)` | Break a timestamp into a UTC object. |
| `date.local(timestamp)` | Break a timestamp into a local-time object. |

`date.utc` and `date.local` return objects with:

- `year`
- `month`
- `day`
- `hour`
- `min`
- `sec`
- `yday`
- `wday`

### `resource`

Generic resource helpers:

| Builtin | Description |
| --- | --- |
| `resource.close(resource)` | Close the resource. Safe to call more than once. |
| `resource.type(resource)` | Return the resource type name, for example `core.file`. |
| `resource.isClosed(resource)` | Return whether the resource has already been closed. |

### `json`

JSON conversion:

| Builtin | Description |
| --- | --- |
| `json.parse(str)` | Parse JSON into PrattScript arrays, objects, numbers, strings, booleans, and `nil`. |
| `json.stringify(value, [pretty])` | Convert a PrattScript value to JSON text. |

`json.stringify` is intended for JSON-compatible data. Values that are not directly representable in JSON, such as functions and builtins, serialize as `null`.

---

## Embedding and Extending

PrattScript is intended to be embedded in C and C++ applications. You can:

- use the Pratt parser core to build your own language or DSL
- extend the interpreter with additional builtins
- plug in custom allocation macros via `pratt_config.h`
- use the AST and statement data structures for tooling or later compilation passes

Start with:

- `pratt.h`
- `pratt_default.h`
- `interpreter_core.h`

---

## FAQ

### How is PrattScript parsed?

Expressions are parsed with Pratt's top-down operator precedence algorithm. Statements such as `if`, `while`, `for`, blocks, and function declarations are handled with recursive descent on top of that expression parser.

### Is PrattScript thread-safe?

The code is re-entrant at the object level, but the interpreter is not internally synchronized. Use one interpreter instance per thread or add your own locking around shared access.

### Does PrattScript have classes, modules, or static typing?

No. The language is intentionally small. The main structured data types are arrays, objects, functions, and namespace objects exposed by the runtime.

### What should I use for host integration?

For simple scripting, expose C functions as builtins. For larger integrations, treat the runtime objects (`fs`, `os`, `date`, `json`, and so on) as examples for how to add host services.

---

## License

PrattScript is released under the **MIT License**. See [`LICENSE`](./LICENSE) for the full text.

Copyright (c) 2025 Mounir IDRASSI <mounir.idrassi@amcrypto.jp>
