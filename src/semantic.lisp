(in-package #:termis)

;;; The compile-time ENVIRONMENT and the semantic objects in this file are
;;; deliberately unrelated.  ENVIRONMENT contains evaluator values and
;;; macros; SEMANTIC-SCOPE contains entities in the runtime/type namespace.

(defclass semantic-binding ()
  ((name :initarg :name
         :reader semantic-binding-name
         ;; Declarations are semantic bindings too.  Preserve the established
         ;; public accessor while giving all bindings a common name protocol.
         :reader declaration-name)))

(defclass semantic-scope ()
  ((parent :initarg :parent :initform nil :reader semantic-scope-parent)
   ;; The program is stored at the root scope and inherited by lookup.  It
   ;; lets references retain source-declaration identity while still finding
   ;; the declaration's resolved type.
   (program :initarg :program :initform nil :accessor semantic-scope-program)
   (type-context :initarg :type-context :initform nil
                 :accessor semantic-scope-type-context)
   ;; Function ownership is inherited in the same way as the type context.
   ;; RETURN consults this semantic context; case scopes retain it naturally.
   (function :initarg :function :initform nil
             :accessor semantic-scope-function)
   ;; Module ownership is semantic context, distinct from lexical parents.
   (module :initarg :module :initform nil :accessor semantic-scope-module)
   (bindings :initform '() :accessor semantic-scope-bindings)))

(defun make-semantic-scope (&optional parent)
  "Create a semantic scope nested below PARENT, when supplied."
  (when parent
    (check-type parent semantic-scope))
  (make-instance 'semantic-scope :parent parent))

(defun semantic-scope-child (scope)
  "Create a lexical semantic child of SCOPE."
  (check-type scope semantic-scope)
  (make-semantic-scope scope))

(defun semantic-scope-owning-program (scope)
  (loop for current = scope then (semantic-scope-parent current)
        while current
        for program = (semantic-scope-program current)
        when program return program))

(defun semantic-scope-owning-type-context (scope)
  "Find the canonical type context inherited by SCOPE."
  (loop for current = scope then (semantic-scope-parent current)
        while current
        for context = (semantic-scope-type-context current)
        when context return context))

(defun semantic-scope-owning-function (scope)
  "Return the enclosing semantic function for SCOPE, when there is one."
  (loop for current = scope then (semantic-scope-parent current)
        while current
        for function = (semantic-scope-function current)
        when function return function))

(defun semantic-scope-local-find (scope name)
  "Return the binding local to SCOPE for NAME, plus a presence flag."
  (check-type scope semantic-scope)
  (check-type name termis-name)
  (let ((entry (assoc name (semantic-scope-bindings scope)
                      :test #'termis-name=)))
    (values (cdr entry) (not (null entry)))))

(defun semantic-scope-find (scope name)
  "Find NAME in SCOPE or one of its semantic parents."
  (check-type scope semantic-scope)
  (check-type name termis-name)
  (loop for current = scope then (semantic-scope-parent current)
        while current
        do (multiple-value-bind (binding foundp)
               (semantic-scope-local-find current name)
             (when foundp
               (return (values binding t))))
        finally (return (values nil nil))))

(defun semantic-scope-bind (scope name binding)
  "Bind NAME to BINDING in SCOPE.

The scope itself permits replacement so clients that need a different
shadowing policy can implement it explicitly.  The resolver rejects duplicate
parameter bindings before calling this operation."
  (check-type scope semantic-scope)
  (check-type name termis-name)
  (let ((entry (assoc name (semantic-scope-bindings scope)
                      :test #'termis-name=)))
    (if entry
        (setf (cdr entry) binding)
        (push (cons name binding) (semantic-scope-bindings scope)))
    binding))

(defun semantic-scope-lookup (scope name)
  "Resolve NAME through SCOPE and its parents, signalling when absent."
  (multiple-value-bind (binding foundp) (semantic-scope-find scope name)
    (if foundp
        binding
        (error 'unresolved-name-error :name name :syntax nil))))

(define-condition semantic-error (error)
  ((syntax :initarg :syntax :initform nil :reader semantic-error-syntax)
   (message :initarg :message :reader semantic-error-message))
  (:report (lambda (condition stream)
             (let ((syntax (semantic-error-syntax condition)))
               (if syntax
                   (let ((location (syntax-start syntax)))
                     (format stream "~A:~D:~D: ~A"
                             (source-name (syntax-source syntax))
                             (source-location-line location)
                             (source-location-column location)
                             (semantic-error-message condition)))
                   (format stream "~A" (semantic-error-message condition)))))))

(define-condition unresolved-name-error (semantic-error)
  ((name :initarg :name :reader unresolved-name-error-name))
  (:report (lambda (condition stream)
             (let ((syntax (semantic-error-syntax condition)))
               (if syntax
                   (let ((location (syntax-start syntax)))
                     (format stream "~A:~D:~D: unknown name `~A`"
                             (source-name (syntax-source syntax))
                             (source-location-line location)
                             (source-location-column location)
                             (termis-name-value
                              (unresolved-name-error-name condition))))
                   (format stream "unknown name `~A`"
                           (termis-name-value
                            (unresolved-name-error-name condition))))))))

(define-condition duplicate-local-binding-error (semantic-error)
  ((name :initarg :name :reader duplicate-local-binding-error-name)
   (existing :initarg :existing :reader duplicate-local-binding-error-existing))
  (:report (lambda (condition stream)
             (let ((syntax (semantic-error-syntax condition)))
               (if syntax
                   (let ((location (syntax-start syntax)))
                     (format stream "~A:~D:~D: duplicate local binding `~A`"
                             (source-name (syntax-source syntax))
                             (source-location-line location)
                             (source-location-column location)
                             (termis-name-value
                              (duplicate-local-binding-error-name condition))))
                   (format stream "duplicate local binding `~A`"
                           (termis-name-value
                           (duplicate-local-binding-error-name condition))))))))

;; Declarations are collected by the compilation-unit processor.  This keeps
;; an accidental declaration in executable code distinct from lexical binding.
(define-condition invalid-definition-context-error (semantic-error) ())

(define-condition non-exhaustive-match-error (semantic-error)
  ((uncovered :initarg :uncovered :reader non-exhaustive-match-error-uncovered))
  (:report (lambda (condition stream)
             (format stream "non-exhaustive match; uncovered: ~A"
                     (non-exhaustive-match-error-uncovered condition)))))

(define-condition unreachable-pattern-error (semantic-error)
  ((covering-pattern :initarg :covering-pattern
                     :reader unreachable-pattern-error-covering-pattern)))

(define-condition unreachable-expression-error (semantic-error) ())

(define-condition return-outside-function-error (semantic-error) ())
