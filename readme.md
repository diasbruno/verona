# Termis

Termis is a statically typed systems language with S-expression syntax. This
repository has completed compiler foundation step 6: it reads source into
source-aware syntax, sequentially performs top-level macro expansion, and
collects declarations into a compilation unit. Declaration bodies remain
unresolved syntax for later semantic phases.

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
definition forms (`type`, `function`, `macro`, `constant`, and `variable`) are
top-level macros that expand into compiler definition forms. Discovery is
ordered so macros can affect later source forms, while body analysis is deferred
until all declarations are known.

## Development

Enter the Nix shell, then run:

```sh
make test
```

The active tests use FiveAM. The pre-foundation C++ test sources and all Termis
examples remain in the repository as historical input material for future
compiler stages.
