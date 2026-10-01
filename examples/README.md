# Current Verona examples

Each file is a self-contained program using syntax and language features
implemented by the current compiler. Every program defines `main` and returns
`42`.

| File | Demonstrates |
| --- | --- |
| `01-arithmetic.vrn` | Typed function parameters and integer addition |
| `02-let-bindings.vrn` | Typed, sequential local bindings |
| `03-match.vrn` | Boolean matching |
| `04-products.vrn` | Product types, construction, and field access |
| `05-sum-types.vrn` | Sum types, constructors, and constructor patterns |
| `06-generics.vrn` | Generic dispatch and a concrete implementation |
| `07-polymorphism-protocols.vrn` | Parametric specialization and protocol constraints |

`make test` compiles every file, and `make test-llvm` executes every file, so
these examples are kept in step with the supported front end and backend.
Modules, imports, standard-library bindings, and C FFI are intentionally not
represented here because they are not yet available in the current compiler.

Executable entry points return `exit-code`, Verona's alias for the platform
C `int` type (currently `i32`).
