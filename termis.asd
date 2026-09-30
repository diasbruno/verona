(asdf:defsystem #:termis
  :description "The Termis compiler front-end foundation"
  :serial t
  :components ((:file "src/package")
               (:file "src/source")
               (:file "src/syntax")
               (:file "src/reader")
               (:file "src/evaluator")
               (:file "src/semantic")
               (:file "src/compiler")
               (:file "src/resolver")))

(asdf:defsystem #:termis/backend/llvm
  :description "LLVM lowering for LLVM-ready Termis semantic programs"
  :depends-on (#:termis #:llvm)
  :serial t
  :components ((:file "src/backend/llvm/package")
               (:file "src/backend/llvm/target")
               (:file "src/backend/llvm/backend")
               (:file "src/backend/llvm/types")
               (:file "src/backend/llvm/primitives")
               (:file "src/backend/llvm/expressions")
               (:file "src/backend/llvm/functions")
               (:file "src/backend/llvm/module")
               (:file "src/backend/llvm/codegen")))

(asdf:defsystem #:termis/compiler
  :description "Termis compiler driver and native artifact toolchain"
  :depends-on (#:termis/backend/llvm)
  :serial t
  :components ((:file "src/driver/package")
               (:file "src/driver/driver")
               (:file "src/driver/cli")))

(asdf:defsystem #:termis/tests
  :depends-on (#:termis #:fiveam)
  :serial t
  :components ((:file "tests/foundation")
               (:file "tests/examples")
               (:file "tests/modules")))

(asdf:defsystem #:termis/llvm-tests
  :description "FiveAM integration tests for the Termis LLVM backend"
  :depends-on (#:termis/tests #:termis/backend/llvm)
  :serial t
  :components ((:file "tests/llvm-backend")
               (:file "tests/examples-llvm")))

(asdf:defsystem #:termis/compiler-tests
  :description "FiveAM tests for the Termis compiler driver"
  :depends-on (#:termis/llvm-tests #:termis/compiler)
  :serial t
  :components ((:file "tests/compiler-driver")))
