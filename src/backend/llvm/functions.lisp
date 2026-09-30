(in-package #:verona.backend.llvm)

(defun declare-function (backend declaration)
  (let* ((source (semantic-source-binding declaration))
         (function (llvm:add-function
                    (llvm-backend-module backend) (llvm-name source)
                    (lower-type backend (verona:semantic-function-declaration-type declaration)))))
    (setf (backend-binding backend source) function
          (backend-binding backend declaration) function)
    function))

(defun declare-external-function (backend declaration)
  "Declare a C ABI function under its explicit linker name."
  (let* ((source (semantic-source-binding declaration))
         (function (llvm:add-function
                    (llvm-backend-module backend)
                    (verona:semantic-external-function-declaration-external-name declaration)
                    (lower-type backend
                                (verona:semantic-external-function-declaration-type declaration)))))
    (setf (backend-binding backend source) function
          (backend-binding backend declaration) function)
    function))

(defun generic-implementation-llvm-name (declaration)
  "A generic has no public symbol; each selected implementation does."
  (format nil "~A_impl_~{~A~^_~}"
          (llvm-name declaration)
          (mapcar #'verona:verona-type-name
                  (verona:generic-implementation-parameter-types declaration))))

(defun declare-generic-implementation (backend declaration)
  (let ((function (llvm:add-function
                   (llvm-backend-module backend)
                   (generic-implementation-llvm-name declaration)
                   (lower-type backend (verona:semantic-generic-implementation-type declaration)))))
    (setf (backend-binding backend declaration) function)
    function))

(defun declare-global (backend declaration constantp)
  (let* ((source (semantic-source-binding declaration))
         (global (llvm:add-global (llvm-backend-module backend)
                                  (lower-type backend
                                              (if constantp
                                                  (verona:semantic-constant-declaration-type declaration)
                                                  (verona:semantic-variable-declaration-type declaration)))
                                  (llvm-name source))))
    (when constantp
      (setf (llvm:global-constant-p global) t))
    (setf (backend-binding backend source) global
          (backend-binding backend declaration) global)
    global))

(defun define-native-export-wrapper (backend export)
  "Expose one explicit C symbol while retaining Verona ABI internally."
  (let* ((verona-function (backend-binding backend
                                           (verona:native-export-binding-function export)))
         (function-type (verona:semantic-function-declaration-type
                         (verona:native-export-binding-function export)))
         (wrapper (llvm:add-function (llvm-backend-module backend)
                                     (verona:native-export-binding-external-name export)
                                     (lower-type backend function-type)))
         (block (llvm:append-basic-block wrapper "entry" :context (llvm-backend-context backend))))
    ;; Exported functions are the first symbols with an explicit visibility
    ;; contract.  Their Verona ABI implementation is local to this module;
    ;; only the wrapper has the public C symbol.
    (setf (llvm:linkage verona-function) :internal
          (llvm:visibility verona-function) :hidden)
    (setf (llvm:linkage wrapper) :external
          (llvm:visibility wrapper) :default)
    (llvm:position-builder-at-end (llvm-backend-builder backend) block)
    (llvm:build-ret (llvm-backend-builder backend)
                    (llvm:build-call (llvm-backend-builder backend)
                                     verona-function (llvm:params wrapper) "verona.export"))
    wrapper))

(defun emit-global-constant (backend expression)
  "Lower the constant subset permitted in an LLVM global initializer."
  (cond ((typep expression 'verona:unit-expression)
         (llvm:const-int (lower-type backend (verona:expression-type expression)) 0))
        ((typep expression 'verona:boolean-literal)
         (llvm:const-int (lower-type backend (verona:expression-type expression))
                         (if (verona:boolean-literal-value expression) 1 0)))
        ((typep expression 'verona:integer-literal)
         (llvm:const-int (lower-type backend (verona:expression-type expression))
                         (verona:integer-literal-value expression)))
        ((typep expression 'verona:float-literal)
         (llvm:const-real (lower-type backend (verona:expression-type expression))
                          (verona:float-literal-value expression)))
        (t (backend-fail "global initializer ~S is not an LLVM constant" expression))))

(defun define-function (backend declaration)
  (let* ((function (backend-binding backend declaration))
         (entry (llvm:append-basic-block function "entry" :context (llvm-backend-context backend)))
         (parameters (verona:semantic-function-declaration-parameters declaration))
         (llvm-parameters (llvm:params function)))
    (llvm:position-builder-at-end (llvm-backend-builder backend) entry)
    ;; Parameters are writable places.  Keep the calling convention
    ;; values distinct from their allocated semantic storage.
    (loop for parameter in parameters
          for llvm-parameter in llvm-parameters
          do (setf (llvm:value-name llvm-parameter) (llvm-name parameter))
             (let ((address (llvm:build-alloca
                             (llvm-backend-builder backend)
                             (lower-type backend (verona:parameter-binding-type parameter))
                             (format nil "~A.addr" (llvm-name parameter)))))
               (llvm:build-store (llvm-backend-builder backend) llvm-parameter address)
               (setf (backend-binding backend parameter) address)))
    (let ((body (verona:semantic-function-declaration-body declaration)))
      ;; A NeverType body has already emitted its terminator (currently an
      ;; explicit return).  Emitting another instruction would corrupt LLVM.
      (unless (typep (verona:expression-type body) 'verona:never-type)
	(llvm:build-ret (llvm-backend-builder backend) (emit-value backend body))))
    function))

(defun define-generic-implementation (backend declaration)
  (let* ((function (backend-binding backend declaration))
         (entry (llvm:append-basic-block function "entry" :context (llvm-backend-context backend)))
         (parameters (verona:generic-implementation-parameters declaration))
         (llvm-parameters (llvm:params function)))
    (llvm:position-builder-at-end (llvm-backend-builder backend) entry)
    (loop for parameter in parameters
          for llvm-parameter in llvm-parameters
          do (setf (llvm:value-name llvm-parameter) (llvm-name parameter))
             (let ((address (llvm:build-alloca
                             (llvm-backend-builder backend)
                             (lower-type backend (verona:parameter-binding-type parameter))
                             (format nil "~A.addr" (llvm-name parameter)))))
               (llvm:build-store (llvm-backend-builder backend) llvm-parameter address)
               (setf (backend-binding backend parameter) address)))
    (let ((body (verona:generic-implementation-body declaration)))
      (unless (typep (verona:expression-type body) 'verona:never-type)
        (llvm:build-ret (llvm-backend-builder backend) (emit-value backend body))))
    function))
