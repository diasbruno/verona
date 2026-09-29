(in-package #:termis)

(defclass termis-callable () ())

(defun termis-callable-p (object)
  (typep object 'termis-callable))

(defclass termis-function (termis-callable)
  ((implementation :initarg :implementation :reader termis-function-implementation)))

(defun termis-function-p (object)
  (typep object 'termis-function))

(defun make-termis-function (implementation)
  "Wrap IMPLEMENTATION as a callable that receives evaluated Termis values."
  (check-type implementation function)
  (make-instance 'termis-function :implementation implementation))

(defclass termis-macro (termis-callable)
  ((implementation :initarg :implementation :reader termis-macro-implementation)))

(defun termis-macro-p (object)
  (typep object 'termis-macro))

(defun make-termis-macro (implementation)
  "Wrap IMPLEMENTATION as a callable that receives unevaluated SYNTAX arguments.

IMPLEMENTATION must return one SYNTAX object."
  (check-type implementation function)
  (make-instance 'termis-macro :implementation implementation))

;; Macro implementations receive their arguments as syntax objects.  Retaining
;; the enclosing form during expansion lets the bootstrap definition macros
;; replace only their head while preserving the complete source span.
(defvar *macro-expansion-syntax* nil)

(define-condition unbound-name-error (error)
  ((name :initarg :name :reader unbound-name-error-name))
  (:report (lambda (condition stream)
             (format stream "Unbound Termis name ~S"
                     (termis-name-value (unbound-name-error-name condition))))))

(define-condition not-callable-error (error)
  ((value :initarg :value :reader not-callable-error-value))
  (:report (lambda (condition stream)
             (format stream "Termis value ~S is not callable"
                     (not-callable-error-value condition)))))

(define-condition invalid-macro-result-error (error)
  ((value :initarg :value :reader invalid-macro-result-error-value))
  (:report (lambda (condition stream)
             (format stream "A Termis macro returned ~S, not syntax"
                     (invalid-macro-result-error-value condition)))))

(defclass environment ()
  ((parent :initarg :parent :initform nil :reader environment-parent)
   ;; An alist makes name comparison explicit instead of relying on a host
   ;; language hash-table equality predicate.
   (bindings :initform '() :accessor environment-bindings)))

(defun make-environment (&optional parent)
  "Create an environment optionally nested beneath PARENT."
  (when parent
    (check-type parent environment))
  (make-instance 'environment :parent parent))

(defun environment-bind (environment name value)
  "Bind NAME to VALUE in ENVIRONMENT, replacing its local binding if present."
  (check-type environment environment)
  (check-type name termis-name)
  (let ((binding (assoc name (environment-bindings environment)
                        :test #'termis-name=)))
    (if binding
        (setf (cdr binding) value)
        (push (cons name value) (environment-bindings environment)))
    value))

(defun environment-find (environment name)
  "Return VALUE and a found flag for NAME, searching lexical parents."
  (loop for current = environment then (environment-parent current)
        while current
        for binding = (assoc name (environment-bindings current)
                             :test #'termis-name=)
        when binding
          do (return (values (cdr binding) t))
        finally (return (values nil nil))))

(defun environment-lookup (environment name)
  "Resolve NAME through ENVIRONMENT and its lexical parents."
  (check-type environment environment)
  (check-type name termis-name)
  (multiple-value-bind (value foundp) (environment-find environment name)
    (if foundp
        value
        (error 'unbound-name-error :name name))))

(defun environment-child (environment)
  "Create a new lexical child of ENVIRONMENT."
  (check-type environment environment)
  (make-environment environment))

(defun macro-at-head (syntax environment)
  "Return the macro bound by SYNTAX's list head, if it has one."
  (let ((datum (syntax-datum syntax)))
    (when (termis-list-p datum)
      (let ((elements (termis-list-elements datum)))
        (when elements
          (let ((head (syntax-datum (first elements))))
            (when (termis-name-p head)
              (multiple-value-bind (value foundp)
                  (environment-find environment head)
                (and foundp (termis-macro-p value) value)))))))))

(defun expand (syntax environment)
  "Recursively expand a macro in SYNTAX's outermost position.

Expansion intentionally stops once the outer form is not a macro; definition
forms such as %FUNCTION are therefore left as Termis syntax for later processing."
  (check-type syntax syntax)
  (check-type environment environment)
  (let ((macro (macro-at-head syntax environment)))
    (if macro
        (let* ((arguments (rest (termis-list-elements (syntax-datum syntax))))
               (result (let ((*macro-expansion-syntax* syntax))
                         (apply (termis-macro-implementation macro) arguments))))
          (unless (typep result 'syntax)
            (error 'invalid-macro-result-error :value result))
          (expand result environment))
        syntax)))

(defun evaluate-list (syntax environment)
  (let* ((elements (termis-list-elements (syntax-datum syntax)))
         (head (first elements)))
    (unless head
      (error 'not-callable-error :value (syntax-datum syntax)))
    (let ((callable (evaluate head environment)))
      (unless (termis-function-p callable)
        (error 'not-callable-error :value callable))
      (apply (termis-function-implementation callable)
             (mapcar (lambda (argument) (evaluate argument environment))
                     (rest elements))))))

(defun evaluate (syntax environment)
  "Evaluate source-aware Termis SYNTAX in ENVIRONMENT and return a value."
  (check-type syntax syntax)
  (check-type environment environment)
  (let ((datum (syntax-datum syntax)))
    (cond ((termis-name-p datum)
           (environment-lookup environment datum))
          ((termis-list-p datum)
           (let ((expanded (expand syntax environment)))
             (if (eq expanded syntax)
                 (evaluate-list syntax environment)
                 (evaluate expanded environment))))
          ;; Unit, booleans, numbers, and strings are self-evaluating values.
          (t datum))))

(defun bootstrap-definition-macro (primitive-name)
  "Make a surface definition macro that mechanically produces PRIMITIVE-NAME.

The arguments remain their original syntax objects: this layer deliberately
does not inspect, evaluate, or otherwise interpret declaration contents."
  (make-termis-macro
   (lambda (&rest arguments)
     (let* ((form *macro-expansion-syntax*)
            (elements (termis-list-elements (syntax-datum form)))
            (head (syntax-with-datum (first elements)
                                     (make-termis-name primitive-name))))
       (syntax-with-datum form
                          (apply #'make-termis-list head arguments))))))

(defun make-bootstrap-environment ()
  "Create the evaluator environment and its standard Termis definition macros."
  (let ((environment (make-environment)))
    ;; This primitive exists solely to prove ordinary and nested calls.  Its
    ;; binding key is a Termis name, never the host's CL:+ symbol.
    (environment-bind environment (make-termis-name "+")
                      (make-termis-function #'+))
    ;; The compiler recognizes only the %... forms.  The ordinary declaration
    ;; vocabulary belongs to this Termis-level environment instead.
    (dolist (definition '( ("type" . "%type")
                           ("function" . "%function")
                           ("macro" . "%macro")
                           ("constant" . "%constant")
                           ("variable" . "%variable")
                           ("generic" . "%generic")
                           ("implementation" . "%implementation")))
      (environment-bind environment (make-termis-name (car definition))
                        (bootstrap-definition-macro (cdr definition))))
    environment))
