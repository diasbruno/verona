# Termis

Termis is a statically typed systems language with S-expression syntax. This
repository is currently at compiler foundation step 2: it reads a source file
into source-aware syntax forms but deliberately does not evaluate, expand
macros, analyze semantics, or generate code yet.

```text
Termis source → Source → Reader → Syntax → Module
```

## Front-end API

```lisp
(defparameter *compiler* (termis:make-compiler))

(defparameter *module*
  (termis:compile-string
   *compiler*
   "(type Point
      (x f32)
      (y f32))

    (defun origin () Point .)"))

(termis:module-forms *module*)
```

`compile-file` accepts a pathname and follows the same source → reader path.
Each syntax object retains its datum, source, and exclusive start/end locations,
so later stages can produce useful diagnostics without retrofitting locations.

The reader recognizes symbols, signed integers, decimal f64 literals,
double-quoted data literals, lists, and `.` as the unit literal. Forms are
intentionally not interpreted by this stage: `type`, `defun`, and every other
head remain ordinary syntax.

## Development

Enter the Nix shell, then run:

```sh
make test
```

The active tests use FiveAM. The pre-foundation C++ test sources and all Termis
examples remain in the repository as historical input material for future
compiler stages.
