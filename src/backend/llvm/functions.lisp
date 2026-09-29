(in-package #:termis.backend.llvm)

(defun declare-function (backend declaration)
  (let* ((source (semantic-source-binding declaration))
         (function (llvm:add-function
                    (llvm-backend-module backend) (llvm-name source)
                    (lower-type backend (termis:semantic-function-declaration-type declaration)))))
    (setf (backend-binding backend source) function
          (backend-binding backend declaration) function)
    function))

(defun declare-global (backend declaration constantp)
  (let* ((source (semantic-source-binding declaration))
         (global (llvm:add-global (llvm-backend-module backend)
                                  (lower-type backend
                                              (if constantp
                                                  (termis:semantic-constant-declaration-type declaration)
                                                  (termis:semantic-variable-declaration-type declaration)))
                                  (llvm-name source))))
    (when constantp
      (setf (llvm:global-constant-p global) t))
    (setf (backend-binding backend source) global
          (backend-binding backend declaration) global)
    global))

(defun emit-global-constant (backend expression)
  "Lower the constant subset permitted in an LLVM global initializer."
  (cond ((typep expression 'termis:unit-expression)
         (llvm:const-int (lower-type backend (termis:expression-type expression)) 0))
        ((typep expression 'termis:boolean-literal)
         (llvm:const-int (lower-type backend (termis:expression-type expression))
                         (if (termis:boolean-literal-value expression) 1 0)))
        ((typep expression 'termis:integer-literal)
         (llvm:const-int (lower-type backend (termis:expression-type expression))
                         (termis:integer-literal-value expression)))
        ((typep expression 'termis:float-literal)
         (llvm:const-real (lower-type backend (termis:expression-type expression))
                          (termis:float-literal-value expression)))
        (t (backend-fail "global initializer ~S is not an LLVM constant" expression))))

(defun define-function (backend declaration)
  (let* ((function (backend-binding backend declaration))
         (entry (llvm:append-basic-block function "entry" :context (llvm-backend-context backend)))
         (parameters (termis:semantic-function-declaration-parameters declaration))
         (llvm-parameters (llvm:params function)))
    (llvm:position-builder-at-end (llvm-backend-builder backend) entry)
    ;; Step 10 parameters are writable places.  Keep the calling convention
    ;; values distinct from their allocated semantic storage.
    (loop for parameter in parameters
          for llvm-parameter in llvm-parameters
          do (setf (llvm:value-name llvm-parameter) (llvm-name parameter))
             (let ((address (llvm:build-alloca
                             (llvm-backend-builder backend)
                             (lower-type backend (termis:parameter-binding-type parameter))
                             (format nil "~A.addr" (llvm-name parameter)))))
               (llvm:build-store (llvm-backend-builder backend) llvm-parameter address)
               (setf (backend-binding backend parameter) address)))
    (let ((body (termis:semantic-function-declaration-body declaration)))
      ;; A NeverType body has already emitted its terminator (currently an
      ;; explicit return).  Emitting another instruction would corrupt LLVM.
      (unless (typep (termis:expression-type body) 'termis:never-type)
	(llvm:build-ret (llvm-backend-builder backend) (emit-value backend body))))
    function))
