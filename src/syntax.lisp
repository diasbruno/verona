(in-package #:termis)

(defclass syntax ()
  ((datum :initarg :datum :reader syntax-datum)
   (source :initarg :source :reader syntax-source)
   (start :initarg :start :reader syntax-start)
   (end :initarg :end :reader syntax-end)))

(defun make-syntax (datum source start end)
  "Create source-aware Termis syntax for DATUM."
  (make-instance 'syntax :datum datum :source source :start start :end end))

(defun syntax-with-datum (syntax datum)
  "Reuse SYNTAX's source span for a replacement DATUM."
  (check-type syntax syntax)
  (make-syntax datum
               (syntax-source syntax)
               (syntax-start syntax)
               (syntax-end syntax)))

(defmethod print-object ((object syntax) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((start (syntax-start object)))
      (format stream "~S at ~A:~D:~D"
              (syntax-datum object)
              (source-name (syntax-source object))
              (source-location-line start)
              (source-location-column start)))))

(defstruct (termis-name (:constructor make-termis-name (value)))
  "A case-sensitive Termis identifier, independent of Common Lisp symbols."
  (value "" :type string))

(defun termis-name= (left right)
  "Whether LEFT and RIGHT denote the same case-sensitive Termis name."
  (and (termis-name-p left)
       (termis-name-p right)
       (string= (termis-name-value left) (termis-name-value right))))

;; These aliases preserve the reader API from the previous foundation step.
;; New compiler code must use TERMIS-NAME rather than the misleading
;; TERMIS-SYMBOL name.
(deftype termis-symbol () 'termis-name)

(defun termis-symbol-p (object)
  (termis-name-p object))

(defun termis-symbol-name (symbol)
  (termis-name-value symbol))

(defstruct unit-literal)

;; Keep boolean spelling distinct from names before semantic analysis.  This
;; avoids accidentally resolving TRUE or FALSE through a lexical scope.
(defstruct (termis-boolean-literal
            (:constructor make-termis-boolean-literal (value)))
  (value nil :type boolean))

(defstruct (termis-list (:constructor %make-termis-list (elements)))
  "A Termis list value.  Its elements are source-aware SYNTAX objects."
  (elements '() :type list))

(defun make-termis-list (&rest elements)
  (dolist (element elements)
    (check-type element syntax))
  (%make-termis-list elements))
