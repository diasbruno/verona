# Termis

Termis is a statically typed systems language with S-expression syntax. The
compiler reads source into source-aware syntax, sequentially performs
top-level macro expansion, collects declarations, resolves semantic types,
and lowers LLVM-ready concrete calls. It supports compile-time generic
dispatch with exact parameter-type matching.

```text
Termis source → Source → Reader → Syntax → Top-level expansion → Declarations → Compilation unit
```

## Front-end API

```lisp
(defparameter *compiler* (termis:make-compiler))

(defparameter *unit*
  (termis:compile-string
   *compiler*
   "(type Point
      (x f32)
      (y f32))

    (function origin () Point unit)"))

(termis:unit-declarations *unit*)
```

`compile-file` accepts a pathname and follows the same pipeline. Every
declaration exposes its original source form and its expanded syntax; the unit
retains ordered declarations, a single declaration namespace, and the
compile-time environment. Duplicate definitions report both source locations.

The reader recognizes symbols, signed integers, decimal f64 literals,
double-quoted data literals, lists, and `unit` as Termis's sole unit notation:
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

The active tests use FiveAM. The pre-foundation C++ test sources remain in the
repository as historical input material. The Termis programs in
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
Step 19 — Compile-time macros in Termis
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
