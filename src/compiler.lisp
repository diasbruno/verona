(in-package #:termis)

(defclass compiler () ())

(defun make-compiler ()
  (make-instance 'compiler))

(defclass module ()
  ((source :initarg :source :reader module-source)
   ;; FORMS preserves the source program independently of declaration
   ;; discovery.  Later phases may decide what to do with non-definition
   ;; top-level forms without losing the original syntax.
   (forms :initarg :forms :reader module-forms)
   (declarations :initform '() :accessor module-declarations)
   ;; This alist is deliberately separate from DECLARATIONS: registration
   ;; order is meaningful, while lookup needs a single namespace.
   (namespace :initform '() :accessor module-namespace)
   (environment :initarg :environment :reader module-environment)))

(defclass declaration ()
  ((name :initarg :name :reader declaration-name)
   ;; SOURCE is the complete definition form, not a resolved compiler type or
   ;; value.  All declaration-specific content remains source-aware syntax.
   (source :initarg :source :reader declaration-source)
   (module :initarg :module :reader declaration-module)))

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
                    (location (syntax-start syntax)))
               (format stream "~A:~D:~D: duplicate declaration of ~A"
                       (source-name (syntax-source syntax))
                       (source-location-line location)
                       (source-location-column location)
                       (termis-name-value (duplicate-declaration-error-name condition)))))))

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

(defun module-lookup (module name)
  "Look up NAME in MODULE's declaration namespace.

The primary value is the declaration (or NIL); the secondary value says
whether the name was present, so a future NIL-valued representation remains
unambiguous."
  (check-type module module)
  (check-type name termis-name)
  (let ((binding (assoc name (module-namespace module) :test #'termis-name=)))
    (values (cdr binding) (not (null binding)))))

(defun register-declaration (module declaration)
  (let ((name (declaration-name declaration)))
    (multiple-value-bind (existing foundp) (module-lookup module name)
      (when foundp
        (error 'duplicate-declaration-error
               :syntax (declaration-source declaration)
               :name name
               :existing existing))
      ;; APPEND preserves program order; the namespace is an implementation
      ;; detail optimized for the tiny front end, not the ordered API.
      (setf (module-declarations module)
            (append (module-declarations module) (list declaration)))
      (push (cons name declaration) (module-namespace module))
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
         (unless (typep result 'syntax)
           (definition-fail definition "%macro body must evaluate to syntax"))
         result)))))

(defun process-definition (context module syntax)
  "Turn a primitive top-level definition SYNTAX into a registered declaration.

CONTEXT is the compile-time evaluator environment.  The evaluator only
expands syntax; this processor is the boundary that creates compiler objects."
  (check-type context environment)
  (check-type module module)
  (check-type syntax syntax)
  (let ((head (definition-head-name syntax)))
    (when head
      (let ((elements (termis-list-elements (syntax-datum syntax))))
        (flet ((make-declaration (class name &rest initargs)
                 (register-declaration
                  module
                  (apply #'make-instance class
                         :name name :source syntax :module module initargs))))
          (cond
            ((string= head "%type")
             (let* ((arguments (definition-elements syntax "type" 2))
                    (name (definition-name syntax (first arguments))))
               (make-declaration 'type-declaration name :body (rest arguments))))
            ((string= head "%function")
             (let ((arguments (definition-elements syntax "function" 4)))
               (unless (= (length arguments) 4)
                 (definition-fail syntax "%function requires a name, parameters, return type, and body"))
               (make-declaration 'function-declaration
                                 (definition-name syntax (first arguments))
                                 :parameters (second arguments)
                                 :return-type (third arguments)
                                 :body (fourth arguments))))
            ((string= head "%macro")
             (let ((arguments (definition-elements syntax "macro" 3)))
               (unless (= (length arguments) 3)
                 (definition-fail syntax "%macro requires a name, parameters, and body"))
               (let* ((name (definition-name syntax (first arguments)))
                      (parameters (second arguments))
                      (body (third arguments))
                      (parameter-names (macro-parameter-names syntax parameters))
                      (declaration (make-declaration 'macro-declaration name
                                                     :parameters parameters :body body)))
                 ;; Bind only after successful registration so a duplicate
                 ;; definition cannot overwrite the existing macro.
                 (environment-bind context name
                                   (declaration-macro syntax parameter-names body context))
                 declaration)))
            ((string= head "%constant")
             (let ((arguments (definition-elements syntax "constant" 3)))
               (unless (= (length arguments) 3)
                 (definition-fail syntax "%constant requires a name, type, and value"))
               (make-declaration 'constant-declaration
                                 (definition-name syntax (first arguments))
                                 :type (second arguments) :value (third arguments))))
            ((string= head "%variable")
             (let ((arguments (definition-elements syntax "variable" 3)))
               (unless (= (length arguments) 3)
                 (definition-fail syntax "%variable requires a name, type, and initializer"))
               (make-declaration 'variable-declaration
                                 (definition-name syntax (first arguments))
                                 :type (second arguments) :initializer (third arguments))))))))))

(defun compile-source (source)
  (let* ((forms (read-source source))
         (environment (make-bootstrap-environment))
         (module (make-instance 'module :source source :forms forms
                                         :environment environment)))
    ;; Only top-level forms reach the definition processor.  Expansion is
    ;; sequential because a preceding %MACRO can affect a following form.
    (dolist (form forms module)
      (process-definition environment module (expand form environment)))))

(defun compile-string (compiler contents &key (name "<string>"))
  "Read and discover primitive top-level declarations in CONTENTS."
  (check-type compiler compiler)
  (compile-source (make-source name contents)))

(defun compile-file (compiler pathname)
  "Read and discover primitive top-level declarations in PATHNAME."
  (check-type compiler compiler)
  (compile-source (source-from-file pathname)))
