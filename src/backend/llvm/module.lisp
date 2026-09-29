(in-package #:termis.backend.llvm)

(defun semantic-declarations (program)
  (mapcar #'cdr (termis:semantic-program-declarations program)))

(defun declare-all (backend program)
  (dolist (declaration (semantic-declarations program))
    (cond ((typep declaration 'termis:semantic-function-declaration)
           (declare-function backend declaration))
          ((typep declaration 'termis:semantic-generic-implementation)
           (declare-generic-implementation backend declaration))
          ((typep declaration 'termis:semantic-variable-declaration)
           (declare-global backend declaration nil))
          ((typep declaration 'termis:semantic-constant-declaration)
           (declare-global backend declaration t)))))

(defun define-all (backend program)
  (dolist (declaration (semantic-declarations program))
    (cond ((typep declaration 'termis:semantic-function-declaration)
           (define-function backend declaration))
          ((typep declaration 'termis:semantic-generic-implementation)
           (define-generic-implementation backend declaration))
          ((typep declaration 'termis:semantic-variable-declaration)
           (setf (llvm:initializer (backend-binding backend declaration))
                 (emit-global-constant
                  backend (termis:semantic-variable-declaration-initializer declaration))))
          ((typep declaration 'termis:semantic-constant-declaration)
           (setf (llvm:initializer (backend-binding backend declaration))
                 (emit-global-constant
                  backend (termis:semantic-constant-declaration-initializer declaration)))))))

(defun verify-llvm-module (backend)
  (handler-case
      (progn (llvm:verify-module (llvm-backend-module backend)) backend)
    (error (condition)
      (backend-fail "LLVM verifier rejected generated module: ~A" condition))))

(defun print-llvm-module (backend)
  (llvm:print-module-to-string (llvm-backend-module backend)))

(defun generate-llvm (program &key (module-name "termis")
                                    (target-configuration (make-target-configuration))
                                    (optimization-level :none))
  "Lower an LLVM-ready semantic PROGRAM and verify the resulting module."
  (check-type program termis:semantic-program)
  (termis:validate-for-backend program)
  (let ((backend (make-llvm-backend :module-name module-name
                                    :target-configuration target-configuration
                                    :optimization-level optimization-level)))
    (setf (backend-type-context backend) (termis:semantic-program-type-context program))
    (unless (= (llvm-backend-pointer-width backend)
               (termis:type-context-pointer-width (backend-type-context backend)))
      (backend-fail "Termis pointer width ~D disagrees with LLVM target width ~D"
                    (termis:type-context-pointer-width (backend-type-context backend))
                    (llvm-backend-pointer-width backend)))
    ;; Pass one installs every callable/global identity before body lowering.
    (declare-all backend program)
    ;; Pass two only fills initializers and bodies.
    (define-all backend program)
    (verify-llvm-module backend)
    backend))
