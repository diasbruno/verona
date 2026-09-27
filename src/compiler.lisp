(in-package #:termis)

(defclass compiler () ())

(defun make-compiler ()
  (make-instance 'compiler))

(defclass module ()
  ((source :initarg :source :reader module-source)
   (forms :initarg :forms :reader module-forms)))

(defun compile-string (compiler contents &key (name "<string>"))
  "Load CONTENTS into a module.  This step only reads forms."
  (check-type compiler compiler)
  (let ((source (make-source name contents)))
    (make-instance 'module :source source :forms (read-source source))))

(defun compile-file (compiler pathname)
  "Load PATHNAME into a module.  This step only reads forms."
  (check-type compiler compiler)
  (let ((source (source-from-file pathname)))
    (make-instance 'module :source source :forms (read-source source))))
