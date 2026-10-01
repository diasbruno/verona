(in-package #:verona/tests)

(in-suite :verona)

(defparameter +current-example-files+
  '("01-arithmetic.vrn"
    "02-let-bindings.vrn"
    "03-match.vrn"
    "04-products.vrn"
    "05-sum-types.vrn"
    "06-generics.vrn"
    "07-polymorphism-protocols.vrn"))

(defun current-example-pathname (name)
  (merge-pathnames (format nil "examples/~A" name)
                   (asdf:system-source-directory :verona)))

(test compiles-current-examples
  (dolist (name +current-example-files+)
    (let ((unit (compile-file (make-compiler) (current-example-pathname name))))
      (is (typep unit 'compilation-unit) name))))
