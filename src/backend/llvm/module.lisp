(in-package #:verona.backend.llvm)

(defun semantic-declarations (program)
  (mapcar #'cdr (verona:semantic-program-declarations program)))

(defun declare-all (backend program)
  (dolist (declaration (semantic-declarations program))
    (cond ((typep declaration 'verona:semantic-function-declaration)
           (declare-function backend declaration))
	  ((typep declaration 'verona:semantic-external-function-declaration)
	   (declare-external-function backend declaration))
          ((typep declaration 'verona:semantic-generic-implementation)
           (declare-generic-implementation backend declaration))
          ((typep declaration 'verona:semantic-variable-declaration)
           (declare-global backend declaration nil))
          ((typep declaration 'verona:semantic-constant-declaration)
           (declare-global backend declaration t)))))

(defun define-all (backend program)
  (dolist (declaration (semantic-declarations program))
    (cond ((typep declaration 'verona:semantic-function-declaration)
           (define-function backend declaration))
          ((typep declaration 'verona:semantic-generic-implementation)
           (define-generic-implementation backend declaration))
          ((typep declaration 'verona:semantic-variable-declaration)
           (setf (llvm:initializer (backend-binding backend declaration))
                 (emit-global-constant
                  backend (verona:semantic-variable-declaration-initializer declaration))))
          ((typep declaration 'verona:semantic-constant-declaration)
           (setf (llvm:initializer (backend-binding backend declaration))
                 (emit-global-constant
                  backend (verona:semantic-constant-declaration-initializer declaration)))))))

(defun verify-llvm-module (backend)
  (handler-case
      (progn (llvm:verify-module (llvm-backend-module backend)) backend)
    (error (condition)
      (backend-fail "LLVM verifier rejected generated module: ~A" condition))))

(defun hide-verona-symbols (backend program)
  "Apply the driver-facing visibility boundary after all wrappers exist.

ExternalFunction declarations retain their foreign linkage.  Source-defined
functions and globals instead become module-local implementation details;
native-export wrappers and the platform entry wrapper are the only public
symbols created by the compiler driver."
  (dolist (declaration (semantic-declarations program))
    (when (or (typep declaration 'verona:semantic-function-declaration)
              (typep declaration 'verona:semantic-generic-implementation)
              (typep declaration 'verona:semantic-variable-declaration)
              (typep declaration 'verona:semantic-constant-declaration))
      (let ((value (backend-binding backend declaration)))
        (setf (llvm:linkage value) :internal
              (llvm:visibility value) :hidden))))
  backend)

(defun print-llvm-module (backend)
  (llvm:print-module-to-string (llvm-backend-module backend)))

(defun generate-llvm (program &key (module-name "verona")
                                    (target-configuration (make-target-configuration))
                                    (optimization-level :none))
  "Lower an LLVM-ready semantic PROGRAM and verify the resulting module."
  (check-type program verona:semantic-program)
  (verona:validate-for-backend program)
  (let ((backend (make-llvm-backend :module-name module-name
                                    :target-configuration target-configuration
                                    :optimization-level optimization-level)))
    (setf (backend-type-context backend) (verona:semantic-program-type-context program))
    (unless (= (llvm-backend-pointer-width backend)
               (verona:type-context-pointer-width (backend-type-context backend)))
      (backend-fail "Verona pointer width ~D disagrees with LLVM target width ~D"
                    (verona:type-context-pointer-width (backend-type-context backend))
                    (llvm-backend-pointer-width backend)))
    ;; Pass one installs every callable/global identity before body lowering.
    (declare-all backend program)
    ;; Pass two only fills initializers and bodies.
    (define-all backend program)
    ;; Native exports are C ABI wrappers around hidden Verona ABI functions.
    (dolist (export (verona:semantic-program-native-exports program))
      (define-native-export-wrapper backend export))
    (verify-llvm-module backend)
    backend))
