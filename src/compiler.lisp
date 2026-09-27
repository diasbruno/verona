(in-package #:termis)

(defclass compiler () ())

(defun make-compiler ()
  (make-instance 'compiler))

(defclass compilation-unit ()
  ((source :initarg :source
           :reader compilation-unit-source
           :reader module-source)
   ;; FORMS preserves the source program independently of declaration
   ;; discovery.  Later phases may decide what to do with non-definition
   ;; top-level forms without losing the original syntax.
   (forms :initarg :forms
          :reader compilation-unit-forms
          :reader module-forms)
   (declarations :initform '()
                 :accessor compilation-unit-declarations
                 :accessor module-declarations)
   ;; This alist is deliberately separate from DECLARATIONS: registration
   ;; order is meaningful, while lookup needs a single namespace.
   (namespace :initform '()
              :accessor compilation-unit-namespace
              :accessor module-namespace)
   (environment :initarg :environment
                :reader compilation-unit-environment
                :reader compilation-unit-compile-time-environment
                :reader module-environment)))

;; MODULE was the name used by the preceding foundation stages.  Keep the
;; legacy class as a compatibility subclass while new callers use
;; COMPILATION-UNIT, which does not prematurely imply package or import
;; semantics.
(defclass module (compilation-unit) ())

(defclass declaration ()
  ((name :initarg :name :reader declaration-name)
   ;; SOURCE is the original complete top-level form, not a resolved compiler
   ;; type or value.  All declaration-specific content remains source-aware.
   (source :initarg :source :reader declaration-source)
   ;; The primitive definition syntax produced by top-level expansion.  It is
   ;; intentionally distinct from SOURCE when a macro produced the definition.
   (expanded-syntax :initarg :expanded-syntax :reader declaration-expanded-syntax)
   (module :initarg :module
           :reader declaration-module
           :reader declaration-compilation-unit)))

(defclass type-declaration (declaration)
  ((body :initarg :body :reader type-declaration-body)))

(defclass function-declaration (declaration)
  ((parameters :initarg :parameters :reader function-declaration-parameters)
   (return-type :initarg :return-type :reader function-declaration-return-type)
   (body :initarg :body :reader function-declaration-body)))

(defclass macro-declaration (declaration)
  ((parameters :initarg :parameters :reader macro-declaration-parameters)
   (body :initarg :body :reader macro-declaration-body)))

(defclass constant-declaration (declaration)
  ((type :initarg :type :reader constant-declaration-type)
   (value :initarg :value :reader constant-declaration-value)))

(defclass variable-declaration (declaration)
  ((type :initarg :type :reader variable-declaration-type)
   (initializer :initarg :initializer :reader variable-declaration-initializer)))

(define-condition definition-error (error)
  ((syntax :initarg :syntax :reader definition-error-syntax)
   (message :initarg :message :reader definition-error-message))
  (:report (lambda (condition stream)
             (let* ((syntax (definition-error-syntax condition))
                    (location (syntax-start syntax)))
               (format stream "~A:~D:~D: ~A"
                       (source-name (syntax-source syntax))
                       (source-location-line location)
                       (source-location-column location)
                       (definition-error-message condition))))))

(define-condition duplicate-declaration-error (definition-error)
  ((name :initarg :name :reader duplicate-declaration-error-name)
   (existing :initarg :existing :reader duplicate-declaration-error-existing))
  (:report (lambda (condition stream)
             (let* ((syntax (definition-error-syntax condition))
                    (location (syntax-start syntax))
                    (existing-source
                      (declaration-source
                       (duplicate-declaration-error-existing condition)))
                    (existing-location (syntax-start existing-source)))
               (format stream "~A:~D:~D: duplicate definition `~A`~%~%previous definition:~%~A:~D:~D"
                       (source-name (syntax-source syntax))
                       (source-location-line location)
                       (source-location-column location)
                       (termis-name-value (duplicate-declaration-error-name condition))
                       (source-name (syntax-source existing-source))
                       (source-location-line existing-location)
                       (source-location-column existing-location))))))

(define-condition non-definition-top-level-error (definition-error) ())

(defstruct (top-level-expansion-result
            (:constructor make-top-level-expansion-result (definitions)))
  "The unambiguous, internal result of expanding one top-level source form.

DEFINITIONS is a list of primitive definition syntax objects.  A distinct
result object avoids treating an ordinary list expression as several forms."
  (definitions '() :type list))

(defparameter +definition-form-names+
  '("%type" "%function" "%macro" "%constant" "%variable"))

(defun definition-head-name (syntax)
  "Return SYNTAX's definition-form name, or NIL when it is not one."
  (let ((datum (syntax-datum syntax)))
    (when (termis-list-p datum)
      (let ((elements (termis-list-elements datum)))
        (when elements
          (let ((head (syntax-datum (first elements))))
            (and (termis-name-p head)
                 (find (termis-name-value head) +definition-form-names+
                       :test #'string=))))))))

(defun definition-form-p (syntax)
  "Whether SYNTAX has one of the primitive top-level definition heads."
  (check-type syntax syntax)
  (not (null (definition-head-name syntax))))

(defun definition-fail (syntax control &rest arguments)
  (error 'definition-error
         :syntax syntax
         :message (apply #'format nil control arguments)))

(defun definition-name (syntax name-syntax)
  (let ((name (syntax-datum name-syntax)))
    (unless (termis-name-p name)
      (definition-fail syntax "definition name must be a Termis name"))
    name))

(defun definition-elements (syntax expected-name minimum-arguments)
  "Return definition arguments after validating the primitive form's arity."
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (when (< (length arguments) minimum-arguments)
      (definition-fail syntax "%~A requires at least ~D argument~:P"
                       expected-name minimum-arguments))
    arguments))

(defun find-declaration (unit name)
  "Look up NAME in MODULE's declaration namespace.

The primary value is the declaration (or NIL); the secondary value says
whether the name was present, so a future NIL-valued representation remains
unambiguous."
  (check-type unit compilation-unit)
  (check-type name termis-name)
  (let ((binding (assoc name (compilation-unit-namespace unit) :test #'termis-name=)))
    (values (cdr binding) (not (null binding)))))

(defun module-lookup (module name)
  "Compatibility name for FIND-DECLARATION."
  (find-declaration module name))

(defun unit-declarations (unit)
  "Return UNIT's declarations in source discovery order."
  (check-type unit compilation-unit)
  (compilation-unit-declarations unit))

(defun register-declaration (unit declaration)
  (let ((name (declaration-name declaration)))
    (multiple-value-bind (existing foundp) (find-declaration unit name)
      (when foundp
        (error 'duplicate-declaration-error
               :syntax (declaration-source declaration)
               :name name
               :existing existing))
      ;; APPEND preserves program order; the namespace is an implementation
      ;; detail optimized for the tiny front end, not the ordered API.
      (setf (compilation-unit-declarations unit)
            (append (compilation-unit-declarations unit) (list declaration)))
      (push (cons name declaration) (compilation-unit-namespace unit))
      declaration)))

(defun macro-parameter-names (definition parameters)
  "Extract macro parameter names while retaining the parameter syntax itself."
  (unless (termis-list-p (syntax-datum parameters))
    (definition-fail definition "%macro parameters must be a list"))
  (mapcar (lambda (parameter)
            (let ((name (syntax-datum parameter)))
              (unless (termis-name-p name)
                (definition-fail definition "%macro parameters must be Termis names"))
              name))
          (termis-list-elements (syntax-datum parameters))))

(defun declaration-macro (definition parameter-names body environment)
  "Construct the compile-time macro represented by a %MACRO declaration.

Macro bodies are evaluated only when the macro is invoked.  Discovery itself
never evaluates a declaration body."
  (make-termis-macro
   (lambda (&rest arguments)
     (unless (= (length arguments) (length parameter-names))
       (definition-fail definition
                        "%macro expected ~D argument~:P, received ~D"
                        (length parameter-names) (length arguments)))
     (let ((macro-environment (environment-child environment)))
       (loop for name in parameter-names
             for argument in arguments
             do (environment-bind macro-environment name argument))
       (let ((result (evaluate body macro-environment)))
         (unless (or (typep result 'syntax)
                     (typep result 'top-level-expansion-result))
           (definition-fail definition
                            "%macro body must evaluate to syntax or top-level definitions"))
         result)))))

(defun process-definition (context unit source &optional (expanded-syntax source))
  "Turn EXPANDED-SYNTAX into a declaration, retaining its original SOURCE.

CONTEXT is the compile-time evaluator environment.  The evaluator only
expands syntax; this processor is the boundary that creates compiler objects."
  (check-type context environment)
  (check-type unit compilation-unit)
  (check-type source syntax)
  (check-type expanded-syntax syntax)
  (let ((head (definition-head-name expanded-syntax)))
    (unless head
      (error 'non-definition-top-level-error
             :syntax source
             :message "top-level expansion must produce a definition"))
    (flet ((make-declaration (class name &rest initargs)
             (register-declaration
              unit
              (apply #'make-instance class
                     :name name :source source :expanded-syntax expanded-syntax
                     :module unit initargs))))
      (cond
            ((string= head "%type")
             (let* ((arguments (definition-elements expanded-syntax "type" 2))
                    (name (definition-name expanded-syntax (first arguments))))
               (make-declaration 'type-declaration name :body (rest arguments))))
            ((string= head "%function")
             (let ((arguments (definition-elements expanded-syntax "function" 4)))
              (unless (= (length arguments) 4)
                 (definition-fail expanded-syntax "%function requires a name, parameters, return type, and body"))
               (make-declaration 'function-declaration
                                 (definition-name expanded-syntax (first arguments))
                                 :parameters (second arguments)
                                 :return-type (third arguments)
                                 :body (fourth arguments))))
            ((string= head "%macro")
             (let ((arguments (definition-elements expanded-syntax "macro" 3)))
              (unless (= (length arguments) 3)
                 (definition-fail expanded-syntax "%macro requires a name, parameters, and body"))
               (let* ((name (definition-name expanded-syntax (first arguments)))
                      (parameters (second arguments))
                      (body (third arguments))
                      (parameter-names (macro-parameter-names expanded-syntax parameters))
                      (declaration (make-declaration 'macro-declaration name
                                                     :parameters parameters :body body)))
                 ;; Bind only after successful registration so a duplicate
                 ;; definition cannot overwrite the existing macro.
                 (environment-bind context name
                                   (declaration-macro expanded-syntax parameter-names body context))
                 declaration)))
            ((string= head "%constant")
             (let ((arguments (definition-elements expanded-syntax "constant" 3)))
              (unless (= (length arguments) 3)
                 (definition-fail expanded-syntax "%constant requires a name, type, and value"))
               (make-declaration 'constant-declaration
                                 (definition-name expanded-syntax (first arguments))
                                 :type (second arguments) :value (third arguments))))
            ((string= head "%variable")
             (let ((arguments (definition-elements expanded-syntax "variable" 3)))
              (unless (= (length arguments) 3)
                 (definition-fail expanded-syntax "%variable requires a name, type, and initializer"))
               (make-declaration 'variable-declaration
                                 (definition-name expanded-syntax (first arguments))
                                 :type (second arguments) :initializer (third arguments))))))))

(defun expand-top-level (syntax environment)
  "Expand SYNTAX into a TOP-LEVEL-EXPANSION-RESULT.

Unlike ordinary EXPAND, this protocol permits a macro to return an explicit
TOP-LEVEL-EXPANSION-RESULT containing zero or more definition forms."
  (check-type syntax syntax)
  (check-type environment environment)
  (labels ((expand-one (form)
             (check-type form syntax)
             (let ((macro (macro-at-head form environment)))
               (if (not macro)
                   (list form)
                   (let ((result (let ((*macro-expansion-syntax* form))
                                   (apply (termis-macro-implementation macro)
                                          (rest (termis-list-elements
                                                 (syntax-datum form)))))))
                     (cond ((typep result 'syntax) (expand-one result))
                           ((typep result 'top-level-expansion-result)
                            (mapcan #'expand-one
                                    (top-level-expansion-result-definitions result)))
                           (t
                            (error 'invalid-macro-result-error :value result))))))))
    (make-top-level-expansion-result (expand-one syntax))))

(defun make-compilation-environment ()
  "Create the compile-time environment used while constructing one unit."
  (let ((environment (make-bootstrap-environment)))
    ;; This internal helper is deliberately available only during compilation.
    ;; It gives source-defined macros a precise way to emit zero or more
    ;; definitions without giving an ordinary Termis list a second meaning.
    (environment-bind
     environment (make-termis-name "definitions")
     (make-termis-function
      (lambda (&rest definitions)
        (dolist (definition definitions)
          (check-type definition syntax))
        (make-top-level-expansion-result definitions))))
    environment))

(defun compile-source (source)
  (let* ((forms (read-source source))
         (environment (make-compilation-environment))
         (unit (make-instance 'module :source source :forms forms
                                      :environment environment)))
    ;; Only top-level forms reach the definition processor.  Expansion is
    ;; sequential because a preceding %MACRO can affect a following form.
    (dolist (form forms unit)
      (dolist (expanded-syntax
               (top-level-expansion-result-definitions
                (expand-top-level form environment)))
        (process-definition environment unit form expanded-syntax)))))

(defun compile-string (compiler contents &key (name "<string>"))
  "Read and discover primitive top-level declarations in CONTENTS."
  (check-type compiler compiler)
  (compile-source (make-source name contents)))

(defun compile-file (compiler pathname)
  "Read and discover primitive top-level declarations in PATHNAME."
  (check-type compiler compiler)
  (compile-source (source-from-file pathname)))
