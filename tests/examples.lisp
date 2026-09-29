(in-package #:termis/tests)

(in-suite :termis)

(defparameter +current-example-files+
  '("01-arithmetic.termis"
    "02-let-bindings.termis"
    "03-match.termis"
    "04-products.termis"
    "05-sum-types.termis"
    "06-generics.termis"))

(defun current-example-pathname (name)
  (merge-pathnames (format nil "examples/~A" name)
                   (asdf:system-source-directory :termis)))

(test compiles-current-examples
  (dolist (name +current-example-files+)
    (let ((unit (compile-file (make-compiler) (current-example-pathname name))))
      (is (typep unit 'compilation-unit) name))))
