(in-package #:termis)

(defclass syntax ()
  ((datum :initarg :datum :reader syntax-datum)
   (source :initarg :source :reader syntax-source)
   (start :initarg :start :reader syntax-start)
   (end :initarg :end :reader syntax-end)))

(defmethod print-object ((object syntax) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (let ((start (syntax-start object)))
      (format stream "~S at ~A:~D:~D"
              (syntax-datum object)
              (source-name (syntax-source object))
              (source-location-line start)
              (source-location-column start)))))

(defstruct termis-symbol
  (name "" :type string))

(defstruct unit-literal)
