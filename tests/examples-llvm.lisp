(in-package #:termis/tests)

(in-suite :termis)

(test executes-current-examples
  (dolist (name +current-example-files+)
    (is (= 42
           (compile-and-run-native
            (uiop:read-file-string (current-example-pathname name))))
        name)))
