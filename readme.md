# Termis

Termis is a statically typed systems programming language using S-expressions.

It aims to combine a small and expressive syntax with predictable low-level memory semantics and a powerful compile-time environment.

## Features

* S-expression syntax
* Static typing
* C-like memory model
* Product, sum, and union types
* Pattern matching
* Parametric and phantom types
* Explicit pointers
* Compile-time evaluation
* Compile-time macros
* Predictable memory layout
* LLVM IR generation
* No mandatory garbage collector or runtime

## Example

```lisp
(type Option (T)
  (sum
    None
    (Some T)))

(fn unwrap-or
  ((params ((value (Option i32))
            (fallback i32)))
   (return i32)
   (requires ())
   (documentation "Return the contained value or a fallback."))
  (match value
    ((Some x) x)
    (None fallback)))

(fn noop
  ((params ())
   (return unit)
   (requires ()))
  .)

(fn main
  ((params ())
   (return i32)
   (requires ()))
  (unwrap-or (Some 42) 0))
```

## Forms

Termis source is a sequence of forms. Whitespace separates forms and has no
meaning otherwise. Comments are not part of the source language yet.

The reader recognizes exactly these form kinds:

```text
form    ::= atom | list
atom    ::= symbol | integer | floating | data | "."
list    ::= "(" form* ")"
```

Symbols are any non-empty sequence of non-whitespace characters other than
`(`, `)`, `"`, and `.`. A `.` by itself is the unit literal; dots inside symbols
are reserved and rejected.

Integers are signed 64-bit decimal literals:

```lisp
42
-7
+9
```

Floating-point literals are decimal literals with digits on both sides of the
decimal point:

```lisp
1.5
-0.25
```

`.5` and `1.` are invalid. Floating-point literals are read as `f64` values, but
LLVM lowering does not yet emit floating-point expressions.

Data literals are double-quoted and support `\n`, `\t`, `\"`, and `\\` escapes.
A quoted literal has the `Data` type, which is represented as `char *` when
passed to C. Termis keeps `string` as a library-level concept for functions that
operate on `Data`, rather than treating quoted literals as a separate built-in
string type:

```lisp
"hello\n"
```

The semantic layer classifies lists by their first element when that element is
a symbol. These are the language forms currently recognized:

```text
(type name body)
(type name (parameter*) body)

(provide name provided-function+)

(fn name signature body+)
(extern fn name signature)
(extern fn name signature "link-name")

(const name value)

(let ((name value)*) body+)
(do body+)
(match value (pattern expression)+)

(module module/name form*)
(import module/name)

(operator argument*)
()
(list form*)
```

`type`, `provide`, `class`, `implements`, `fn`, `extern`, `const`, `let`, `do`,
and `match` are classified as special forms. Any other list headed by a symbol
is an application. Empty lists, `(list ...)`, and lists whose first element is
not a symbol are list expressions; they are parsed and classified but not yet
lowered to LLVM.

Function signatures are stable declaration lists with named fields:

```lisp
((type-params (type-parameter-name*))
 (params ((parameter-name parameter-type)*))
 (return result-type)
 (requires ((Class type*)*))
 (documentation "optional documentation"))
```

`type-params` and `documentation` are optional. `params`, `return`, and
`requires` are currently required so the declaration shape remains fixed as
requirements evolve.

Provided declarations describe functions and operators supplied by the compiler,
target, or runtime instead of by ordinary Termis code:

```lisp
(provide core/math
  (fn +
    ((type-params (T))
     (params ((left T) (right T)))
     (return T)
     (requires ((Integer T)))
     (documentation "Add two integers."))))
```

The semantic layer collects and validates these signatures. Code generation
currently treats provided declarations as metadata; arithmetic and comparison
operators are still lowered by their existing built-in LLVM paths.

The compiler frontend handles `module` and `import` before semantic analysis.
`(module name ...)` unwraps and contributes its body forms. `(import name)` is
accepted as a module dependency marker and then skipped; module paths supplied
with `-I`/`--module-path` decide which files are loaded.

Code generation currently lowers top-level `type`, `fn`, and `extern fn`
declarations. Top-level `provide`, `class`, and `implements` declarations are
recognized by semantic analysis but are not yet emitted. Top-level expressions
and `const` declarations are recognized by semantic analysis but are not yet
emitted.
Function bodies currently lower:

```text
integer literals
true
false
.
local variable references
(let ((name value)*) body+)
(do body+)
(match value (pattern expression)+)
(+ left right)
(- left right)
(* left right)
(/ left right)
(= left right)
(!= left right)
(< left right)
(<= left right)
(> left right)
(>= left right)
(function-name argument*)
```

Arithmetic currently supports `i32` and `i64` operands. Comparisons require both
operands to have the same lowered type and return `bool`.

Match arms have the form `(pattern expression)`. Lowered patterns are:

```text
_       catch-all
name    bind the scrutinee to name
true
false
integer
.       unit
```

A `match` must be exhaustive. Boolean matches are exhaustive when they contain
both `true` and `false` arms; otherwise the final arm must be a catch-all or
binding pattern.

## Types

Termis uses `type` as the common type declaration mechanism.

```lisp
(type Point
  (product
    (x f32)
    (y f32)))

(type Result (T E)
  (sum
    (Ok T)
    (Error E)))

(type Word
  (union
    (value u32)
    (bytes (array u8 4))))
```

Pointers use `&`:

```lisp
(& Point)
```

Type expressions have these forms:

```text
i8 | i16 | i32 | i64 | isize
u8 | u16 | u32 | u64 | usize
f32 | f64
bool
unit
void

Name
(Name argument*)
(& element-type)
(array element-type size)
(slice element-type)
(fn (argument-type*) result-type)
(product (field-name field-type)*)
(sum alternative*)
(union (field-name field-type)*)
```

`Name` refers to a concrete type declaration or a type parameter. `(Name ...)`
instantiates a generic type declaration. Array sizes are integer literals.

Product and union fields are two-element lists:

```lisp
(field-name field-type)
```

Sum alternatives are either a bare constructor name or a constructor followed by
payload types:

```lisp
None
(Some T)
```

The unit type is `unit`, and its sole value is `.`:

```lisp
(fn noop
  ((params ())
   (return unit)
   (requires ()))
  .)
```

## Compiler

The initial Termis compiler targets LLVM IR.

```text
Termis source
    ↓
S-expression reader
    ↓
module loading
    ↓
semantic analysis
    ↓
layout checking
    ↓
LLVM IR
    ↓
LLVM object emission
    ↓
platform link
```

The initial compiler executable is expected to be called `termisc`.

```console
$ termisc hello.termis -o hello.ll
```

The core language has no mandatory standard library or runtime. Module paths are
loaded explicitly with `-I`/`--module-path`; each path contributes every
`.termis` file it contains.

```lisp
(module my/program
  (import std/data)

  (fn main
    ((params ())
     (return i64)
     (requires ()))
    42))
```

```console
$ termisc -I std program.termis -o program
```

Console output is available through `std.io`:

```lisp
(module examples/hello
  (import std/io)

  (fn main
    ((params ())
     (return i32)
     (requires ()))
    (print-line "Hello from Termis")))
```

Modules wrap their declarations. Imports record module dependencies, but modules
are not bound to file boundaries and may be extended later; current conflict
detection is based on duplicate declarations.

The initial standard-library modules are intentionally small:

```text
std.data
std.string
std.memory
std.io
std.os
std.process
std.time
```

`std.data` defines `Data` as the conventional C-compatible byte/raw-memory
pointer:

```lisp
(type Data (& u8))
```

`Data` covers APIs that operate on C `void *` or `char *` style storage. It does
not imply length, ownership, encoding, or NUL termination.

`std.memory` uses `Data` and target-sized integers for allocation sizes:

```lisp
(extern fn allocate
  ((params ((size usize)))
   (return Data)
   (requires ())
   (documentation "Allocate raw storage."))
  "malloc")

(extern fn free
  ((params ((data Data)))
   (return unit)
   (requires ())
   (documentation "Release raw storage."))
  "free")

(extern fn length-until-zero
  ((params ((data Data)))
   (return usize)
   (requires ())
   (documentation "Count bytes before the first zero byte."))
  "strlen")
```

`Data` represents storage whose higher-level element type may be unknown. It is
not a dynamic `any` type and should be cast to a typed pointer before
dereferencing once casts and dereference operations are available.

`std.string` defines `String` as a higher-level text type backed by `Data` while
the language grows owned values, lengths, and encoding-aware operations:

```lisp
(type String Data)
```

Quoted literals can be passed to `String` parameters. Raw C functions such as
`strlen`, `strcpy`, and `strcmp` are exposed through `std.memory` as
zero-terminated memory operations and do not define the high-level string
surface:

```lisp
(extern fn consume-string ((text String)) unit "consume")

(fn main () unit
  (consume-string "hello"))
```

Termis can declare C functions with `extern fn`. The optional final string names
the linked C symbol; without it, the Termis function name is used as the C symbol.

```lisp
(extern fn c-abs
  ((params ((value i64)))
   (return i64)
   (requires ()))
  "llabs")

(fn main
  ((params ())
   (return i64)
   (requires ()))
  (c-abs -42))
```

```console
$ make examples
```

The `examples` target builds every example program into `build/examples/`.

## Status

Termis is currently in the early design and bootstrap implementation stage.

The language syntax, type system, memory model, and compiler architecture are subject to change.

## License

Termis is released into the public domain under the Unlicense. See `UNLICENSE`.
