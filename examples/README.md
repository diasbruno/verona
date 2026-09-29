# Current Termis examples

Each file is a self-contained program using syntax and language features
implemented by the current compiler. Every program defines `main` and returns
`42`.

| File | Demonstrates |
| --- | --- |
| `01-arithmetic.termis` | Typed function parameters and integer addition |
| `02-let-bindings.termis` | Typed, sequential local bindings |
| `03-match.termis` | Boolean matching |
| `04-products.termis` | Product types, construction, and field access |
| `05-sum-types.termis` | Sum types, constructors, and constructor patterns |
| `06-generics.termis` | Generic dispatch and a concrete implementation |

`make test` compiles every file, and `make test-llvm` executes every file, so
these examples are kept in step with the supported front end and backend.
Modules, imports, standard-library bindings, and C FFI are intentionally not
represented here because they are not yet available in the current compiler.
