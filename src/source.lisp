(in-package #:termis)

(define-condition source-error (error)
  ((message :initarg :message :reader source-error-message))
  (:report (lambda (condition stream)
             (write-string (source-error-message condition) stream))))

(defclass source ()
  ((name :initarg :name :reader source-name)
   (contents :initarg :contents :reader source-contents)))

(defun make-source (name contents)
  "Create source owned by the compiler front end."
  (check-type name string)
  (check-type contents string)
  (make-instance 'source :name name :contents contents))

(defun source-from-file (pathname)
  "Load PATHNAME into a source, retaining its namestring for diagnostics."
  (let ((path (pathname pathname)))
    (handler-case
        (with-open-file (stream path :direction :input :element-type 'character)
          (make-source (namestring path)
                       (with-output-to-string (contents)
                         (loop for character = (read-char stream nil nil)
                               while character
                               do (write-char character contents)))))
      (file-error (condition)
        (error 'source-error
               :message (format nil "Unable to read source file ~A: ~A" path condition))))))

(defstruct source-location
  (offset 0 :type (integer 0 *))
  (line 1 :type (integer 1 *))
  (column 1 :type (integer 1 *)))

(defun source-location-at (source offset)
  (let ((line 1)
        (column 1)
        (contents (source-contents source)))
    (loop for index below offset
          for character = (char contents index)
          do (if (char= character #\Newline)
                 (setf line (1+ line)
                       column 1)
                 (incf column)))
    (make-source-location :offset offset :line line :column column)))
