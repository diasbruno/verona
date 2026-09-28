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
