(in-package #:termis)

;;; This file is the syntax-to-semantics boundary.  It intentionally performs
;;; no type compatibility checks: a resolved reference records *which* entity
;;; a name denotes, not whether that use is valid.

(defclass builtin-binding (semantic-binding) ())
(defclass builtin-type-binding (builtin-binding) ())
(defclass builtin-intrinsic-binding (builtin-binding) ())

(defclass parameter-binding (semantic-binding)
  ((syntax :initarg :syntax :reader parameter-binding-syntax)
   (type-syntax :initarg :type-syntax :reader parameter-binding-type-syntax)
   (type-reference :initform nil :accessor parameter-binding-type-reference)))

(defclass semantic-program ()
  ((bootstrap-scope :initarg :bootstrap-scope
                    :reader semantic-program-bootstrap-scope)
   (module-scope :initarg :module-scope :reader semantic-program-module-scope)
   ;; Entries map source declarations to their resolved counterpart.  Macro
   ;; declarations deliberately have no entry: they belong to expansion.
   (declarations :initform '() :accessor semantic-program-declarations)))

(defclass semantic-declaration ()
  ((source-declaration :initarg :source-declaration
                       :reader semantic-declaration-source-declaration)))

(defclass semantic-type-declaration (semantic-declaration) ())

(defclass semantic-constant-declaration (semantic-declaration)
  ((type-reference :initform nil
                   :accessor semantic-constant-declaration-type-reference)
   (initializer :initform nil
                :accessor semantic-constant-declaration-initializer)))

(defclass semantic-variable-declaration (semantic-declaration)
  ((type-reference :initform nil
                   :accessor semantic-variable-declaration-type-reference)
   (initializer :initform nil
                :accessor semantic-variable-declaration-initializer)))

(defclass semantic-function-declaration (semantic-declaration)
  ((scope :initform nil :accessor semantic-function-declaration-scope)
   (parameters :initform '() :accessor semantic-function-declaration-parameters)
   (return-type-reference :initform nil
                          :accessor semantic-function-declaration-return-type-reference)
   (body :initform nil :accessor semantic-function-declaration-body)))

(defclass semantic-expression ()
  ((syntax :initarg :syntax :reader semantic-expression-syntax)))

(defclass semantic-literal (semantic-expression) ())

(defclass semantic-reference (semantic-expression)
  ((name :initarg :name :reader semantic-reference-name)
   (binding :initarg :binding :reader semantic-reference-binding)))

(defclass semantic-call (semantic-expression)
  ((callee :initarg :callee :reader semantic-call-callee)
   (arguments :initarg :arguments :reader semantic-call-arguments)))

(defun semantic-program-declaration (program declaration)
  "Return DECLARATION's resolved semantic node, or NIL for macro declarations."
  (check-type program semantic-program)
  (check-type declaration declaration)
  (cdr (assoc declaration (semantic-program-declarations program) :test #'eq)))

(defun make-bootstrap-semantic-scope ()
  "Create compiler-provided semantic bindings, independent of evaluation."
  (let ((scope (make-semantic-scope)))
    (dolist (name '("i8" "i16" "i32" "i64"
                    "u8" "u16" "u32" "u64" "f32" "f64"))
      (semantic-scope-bind scope (make-termis-name name)
                           (make-instance 'builtin-type-binding
                                          :name (make-termis-name name))))
    ;; Their meaning is deferred to a later semantic/type phase.  They are
    ;; still normal bindings here, so call syntax never compares text to "+".
    (dolist (name '("+" "-" "*" "/"))
      (semantic-scope-bind scope (make-termis-name name)
                           (make-instance 'builtin-intrinsic-binding
                                          :name (make-termis-name name))))
    scope))

(defun resolve-name (scope syntax)
  "Resolve a name SYNTAX into a semantic reference in SCOPE."
  (let ((name (syntax-datum syntax)))
    (unless (termis-name-p name)
      (error 'semantic-error :syntax syntax :message "expected a Termis name"))
    (multiple-value-bind (binding foundp) (semantic-scope-find scope name)
      (unless foundp
        (error 'unresolved-name-error :name name :syntax syntax))
      (make-instance 'semantic-reference :syntax syntax :name name :binding binding))))

(defun build-semantic-expression (scope syntax)
  "Build a resolved expression from syntax in SCOPE.

This is intentionally small: atoms become literals, names become references,
and lists become calls.  Future expression forms can introduce child scopes
without changing the scope or binding model established here."
  (check-type scope semantic-scope)
  (check-type syntax syntax)
  (let ((datum (syntax-datum syntax)))
    (cond ((termis-name-p datum) (resolve-name scope syntax))
          ((termis-list-p datum)
           (let ((elements (termis-list-elements datum)))
             (unless elements
               (error 'semantic-error :syntax syntax
                      :message "an empty list is not an expression"))
             (make-instance 'semantic-call
                            :syntax syntax
                            :callee (build-semantic-expression scope (first elements))
                            :arguments (mapcar (lambda (element)
                                                 (build-semantic-expression scope element))
                                               (rest elements)))))
          (t (make-instance 'semantic-literal :syntax syntax)))))

(defun resolve-type-syntax (scope syntax)
  "Resolve a declaration-interface type name without assigning it a type."
  (resolve-name scope syntax))

(defun parse-parameter (function parameter-syntax)
  "Create a parameter entity from one (name type) syntax form."
  (unless (termis-list-p (syntax-datum parameter-syntax))
    (error 'semantic-error :syntax parameter-syntax
           :message "function parameter must be a (name type) list"))
  (let ((elements (termis-list-elements (syntax-datum parameter-syntax))))
    (unless (= (length elements) 2)
      (error 'semantic-error :syntax parameter-syntax
             :message "function parameter must contain a name and type"))
    (let ((name (syntax-datum (first elements))))
      (unless (termis-name-p name)
        (error 'semantic-error :syntax (first elements)
               :message "function parameter name must be a Termis name"))
      (make-instance 'parameter-binding
                     :name name :syntax (first elements) :type-syntax (second elements)))))

(defun resolve-function-signature (program semantic-declaration)
  (let* ((declaration (semantic-declaration-source-declaration semantic-declaration))
         (module-scope (semantic-program-module-scope program))
         (parameters-syntax (function-declaration-parameters declaration)))
    (unless (termis-list-p (syntax-datum parameters-syntax))
      (error 'semantic-error :syntax parameters-syntax
             :message "function parameters must be a list"))
    (let ((scope (semantic-scope-child module-scope))
          (parameters
            (mapcar (lambda (syntax) (parse-parameter declaration syntax))
                    (termis-list-elements (syntax-datum parameters-syntax)))))
      ;; Parameter types are interface names, and so resolve in the module
      ;; scope.  Binding parameters afterward avoids accidental access to a
      ;; preceding parameter while interpreting this still-untyped syntax.
      (dolist (parameter parameters)
        (setf (parameter-binding-type-reference parameter)
              (resolve-type-syntax module-scope
                                   (parameter-binding-type-syntax parameter))))
      (dolist (parameter parameters)
        (multiple-value-bind (existing foundp)
            (semantic-scope-local-find scope (semantic-binding-name parameter))
          (when foundp
            (error 'duplicate-local-binding-error
                   :syntax (parameter-binding-syntax parameter)
                   :name (semantic-binding-name parameter) :existing existing))
          (semantic-scope-bind scope (semantic-binding-name parameter) parameter)))
      (setf (semantic-function-declaration-scope semantic-declaration) scope
            (semantic-function-declaration-parameters semantic-declaration) parameters
            (semantic-function-declaration-return-type-reference semantic-declaration)
            (resolve-type-syntax module-scope
                                 (function-declaration-return-type declaration))))))

(defun make-semantic-declaration (declaration)
  (cond ((typep declaration 'type-declaration)
         (make-instance 'semantic-type-declaration :source-declaration declaration))
        ((typep declaration 'constant-declaration)
         (make-instance 'semantic-constant-declaration :source-declaration declaration))
        ((typep declaration 'variable-declaration)
         (make-instance 'semantic-variable-declaration :source-declaration declaration))
        ((typep declaration 'function-declaration)
         (make-instance 'semantic-function-declaration :source-declaration declaration))
        ;; Macro declarations have already been handled by the evaluator.
        ((typep declaration 'macro-declaration) nil)
        (t (error "Unknown Termis declaration ~S" declaration))))

(defun resolve-declaration-signature (program semantic-declaration)
  (let ((declaration (semantic-declaration-source-declaration semantic-declaration))
        (scope (semantic-program-module-scope program)))
    (cond ((typep semantic-declaration 'semantic-function-declaration)
           (resolve-function-signature program semantic-declaration))
          ((typep semantic-declaration 'semantic-constant-declaration)
           (setf (semantic-constant-declaration-type-reference semantic-declaration)
                 (resolve-type-syntax scope (constant-declaration-type declaration))))
          ((typep semantic-declaration 'semantic-variable-declaration)
           (setf (semantic-variable-declaration-type-reference semantic-declaration)
                 (resolve-type-syntax scope (variable-declaration-type declaration)))))))

(defun resolve-declaration-body (program semantic-declaration)
  (let ((declaration (semantic-declaration-source-declaration semantic-declaration)))
    (cond ((typep semantic-declaration 'semantic-function-declaration)
           (setf (semantic-function-declaration-body semantic-declaration)
                 (build-semantic-expression
                  (semantic-function-declaration-scope semantic-declaration)
                  (function-declaration-body declaration))))
          ((typep semantic-declaration 'semantic-constant-declaration)
           (setf (semantic-constant-declaration-initializer semantic-declaration)
                 (build-semantic-expression
                  (semantic-program-module-scope program)
                  (constant-declaration-value declaration))))
          ((typep semantic-declaration 'semantic-variable-declaration)
           (setf (semantic-variable-declaration-initializer semantic-declaration)
                 (build-semantic-expression
                  (semantic-program-module-scope program)
                  (variable-declaration-initializer declaration)))))))

(defun resolve-compilation-unit (unit)
  "Resolve UNIT after all declarations have been collected.

The two passes are intentional: the first registers every runtime/type
declaration and resolves interfaces; the second resolves executable bodies.
Thus ordinary declaration order has no effect on name visibility."
  (check-type unit compilation-unit)
  (let* ((bootstrap (make-bootstrap-semantic-scope))
         (module-scope (semantic-scope-child bootstrap))
         (program (make-instance 'semantic-program :bootstrap-scope bootstrap
                                  :module-scope module-scope)))
    (dolist (declaration (unit-declarations unit))
      (unless (typep declaration 'macro-declaration)
        (semantic-scope-bind module-scope (declaration-name declaration) declaration)
        (let ((semantic-declaration (make-semantic-declaration declaration)))
          (push (cons declaration semantic-declaration)
                (semantic-program-declarations program)))))
    (setf (semantic-program-declarations program)
          (nreverse (semantic-program-declarations program)))
    (dolist (entry (semantic-program-declarations program))
      (resolve-declaration-signature program (cdr entry)))
    (dolist (entry (semantic-program-declarations program))
      (resolve-declaration-body program (cdr entry)))
    (setf (compilation-unit-semantic-program unit) program)
    program))
