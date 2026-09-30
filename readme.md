# Verona

Verona is a statically typed systems language with S-expression syntax. The
compiler reads source into source-aware syntax, sequentially performs
top-level macro expansion, collects declarations, resolves semantic types,
and lowers LLVM-ready concrete calls. It supports compile-time generic
dispatch with exact parameter-type matching.

```text
Verona source → Source → Reader → Syntax → Top-level expansion → Declarations → Compilation unit
```

## Front-end API

For the complete source-language, Common Lisp, LLVM, compiler-driver, and
declarative-build function reference, see [the API reference](docs/API.md).

```lisp
(defparameter *compiler* (verona:make-compiler))

(defparameter *unit*
  (verona:compile-string
   *compiler*
   "(type Point
      (x f32)
      (y f32))

    (function origin () Point unit)"))

(verona:unit-declarations *unit*)
```

`compile-file` accepts a pathname and follows the same pipeline. Every
declaration exposes its original source form and its expanded syntax; the unit
retains ordered declarations, a single declaration namespace, and the
compile-time environment. Duplicate definitions report both source locations.

The reader recognizes symbols, signed integers, decimal f64 literals,
double-quoted data literals, lists, and `unit` as Verona's sole unit notation:
it denotes `UnitType` in type position and its only value in value position.
The public
definition forms (`type`, `function`, `macro`, `constant`, `variable`,
`generic`, and `implementation`) are
top-level macros that expand into compiler definition forms. Discovery is
ordered so macros can affect later source forms, while body analysis is deferred
until all declarations are known.

## Development

Enter the Nix shell, then run:

```sh
make test
```

## Standalone executable

Build a self-contained `verona` command with SBCL's runtime bundled into the
image:

```sh
make build
```

This creates `build/verona`. The build must run from the Verona development
shell because it needs the configured LLVM bindings. The resulting executable
can then be installed on the system path, for example:

```sh
install -m 755 build/verona /usr/local/bin/verona
```

The executable is native to the platform and architecture where it was built.

## Native compiler

The `verona` command is a thin layer over the reusable
`verona.compiler:compiler-driver` API. `make build` produces the standalone
command described above. It resolves an explicit native target before frontend
analysis, verifies the in-memory LLVM module, emits an object, and delegates
final linking or archiving to the host toolchain.

```sh
verona compile src/app.vrn -o app
verona compile src/lib.vrn --emit static-library -o libverona.a
verona compile src/lib.vrn --emit shared-library -o libverona.dylib
```

Executables require `(function main () unit ...)`; the generated platform
wrapper returns status zero. Object files, static libraries, and shared
libraries do not require `main`. `-L`, `-l`, and `--framework` pass native
linker inputs through the driver (frameworks are Darwin-only).

Verona-module visibility remains independent of native visibility. A function
is exposed to C only with an explicit top-level declaration:

```lisp
(function add ((a i32) (b i32)) i32 (+ a b))
(native-export add)             ; optional second argument: "c_symbol_name"
```

Native exports currently accept the scalar and pointer types already supported
by `external-function`; products, sums, and `unit` remain outside the C ABI.

## Declarative builds

`verona.build` describes native artifacts without evaluating Verona code. The
output directory belongs to the invocation, not the build file:

```lisp
(executable app
  (root app.main)
  (module-path "src")
  (optimize 2)
  (library "sqlite3"))
```

```sh
verona build app ./dist/bin
```

Top-level `static-library` and `shared-library` declarations use the same
options. Build files may also declare target triples, native library paths,
and Darwin frameworks. Relative module and library paths are resolved from the
directory containing `verona.build`.

The active tests use FiveAM. The pre-foundation C++ test sources remain in the
repository as historical input material. The Verona programs in
[`examples/`](examples/) use the current front-end syntax and are compiled by
the active test suite.

## Examples

[`examples/`](examples/) contains small, self-contained programs covering the
features implemented today. See [the examples guide](examples/README.md) for
the feature covered by each program. Modules, imports, the standard library,
and C FFI are not yet part of this set because they are not currently supported
by the front end.

## Roadmap

```text
Step 15 — Product types
Step 16 — Sum types + constructor patterns
Step 17 — Repetition: loop / recur
Step 18 — Generics: generic / implementation
Step 19 — Compile-time macros in Verona
Step 20 — Arrays + memory operations
Step 21 — C FFI / ABI
Step 22 — Modules / namespaces
Step 23 — Standard library foundation
Step 24 — Diagnostics + compiler hardening
Step 25 — Self-hosting groundwork, if pursued

Later — Recursive types
        recursive products and sums
        mutual recursion, sizedness, and forward type identities
        LLVM incomplete/identified types
```
