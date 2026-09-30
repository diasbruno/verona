(in-package #:verona)

(defclass syntax ()
  ((datum :initarg :datum :reader syntax-datum)
   (source :initarg :source :reader syntax-source)
   (start :initarg :start :reader syntax-start)
   (end :initarg :end :reader syntax-end)
   ;; NIL means reader-originated syntax.  Non-NIL is an EXPANSION-ORIGIN
   ;; chain; its type is intentionally not constrained here because syntax is
   ;; loaded before evaluator.lisp defines the provenance structure.
   (expansion-origin :initarg :expansion-origin :initform nil
                     :reader syntax-expansion-origin)))

(defun make-syntax (datum source start end &key expansion-origin)
  "Create source-aware Verona syntax for DATUM."
  (make-instance 'syntax :datum datum :source source :start start :end end
                 :expansion-origin expansion-origin))

(defun syntax-with-datum (syntax datum)
  "Reuse SYNTAX's source span for a replacement DATUM."
  (check-type syntax syntax)
  (make-syntax datum
               (syntax-source syntax)
               (syntax-start syntax)
               (syntax-end syntax)
               :expansion-origin (syntax-expansion-origin syntax)))

(defmethod print-object ((object syntax) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((start (syntax-start object)))
      (format stream "~S at ~A:~D:~D"
              (syntax-datum object)
              (source-name (syntax-source object))
              (source-location-line start)
              (source-location-column start)))))

(defstruct (verona-name (:constructor make-verona-name (value)))
  "A case-sensitive Verona identifier, independent of Common Lisp symbols."
  (value "" :type string))

(defstruct (module-name (:constructor %make-module-name (components)))
  "The semantic identity of a Verona module.

COMPONENTS are Verona names, never host symbols or filesystem pathnames."
  (components '() :type list))

(defun make-module-name (&rest components)
  (dolist (component components)
    (check-type component verona-name))
  (when (null components)
    (error "a module name requires at least one component"))
  (%make-module-name components))

(defun module-name= (left right)
  (and (module-name-p left) (module-name-p right)
       (= (length (module-name-components left))
          (length (module-name-components right)))
       (every #'verona-name= (module-name-components left)
              (module-name-components right))))

(defun module-name-string (name)
  (check-type name module-name)
  (format nil "~{~A~^.~}" (mapcar #'verona-name-value
                                   (module-name-components name))))

(defstruct (qualified-name (:constructor make-qualified-name (qualifier name)))
  "A structured MODULE:NAME reference; it is deliberately not a Verona name."
  (qualifier (error "qualified name needs a qualifier") :type module-name)
  (name (error "qualified name needs a name") :type verona-name))

(defun qualified-name-string (name)
  (check-type name qualified-name)
  (format nil "~A:~A" (module-name-string (qualified-name-qualifier name))
          (verona-name-value (qualified-name-name name))))

(defun verona-name= (left right)
  "Whether LEFT and RIGHT denote the same case-sensitive Verona name."
  (and (verona-name-p left)
       (verona-name-p right)
       (string= (verona-name-value left) (verona-name-value right))))

;; These aliases preserve the earlier reader API.
;; New compiler code must use VERONA-NAME rather than the misleading
;; VERONA-SYMBOL name.
(deftype verona-symbol () 'verona-name)

(defun verona-symbol-p (object)
  (verona-name-p object))

(defun verona-symbol-name (symbol)
  (verona-name-value symbol))

(defstruct unit-literal)

;; Keep boolean spelling distinct from names before semantic analysis.  This
;; avoids accidentally resolving TRUE or FALSE through a lexical scope.
(defstruct (verona-boolean-literal
            (:constructor make-verona-boolean-literal (value)))
  (value nil :type boolean))

(defstruct (verona-list (:constructor %make-verona-list (elements)))
  "A Verona list value.  Its elements are source-aware SYNTAX objects."
  (elements '() :type list))

(defun make-verona-list (&rest elements)
  (dolist (element elements)
    (check-type element syntax))
  (%make-verona-list elements))
