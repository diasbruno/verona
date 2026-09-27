(defpackage #:termis/tests
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:termis #:compile-file)
  (:import-from #:termis
                #:compile-string #:make-compiler #:make-source
                #:module-forms #:module-source #:read-source #:source-contents #:source-location-offset
                #:source-location-column #:source-location-line #:source-name
                #:syntax-datum #:syntax-end #:syntax-source #:syntax-start
                #:termis-read-error #:termis-symbol #:termis-symbol-name #:unit-literal-p))

(in-package #:termis/tests)

(def-suite :termis)
(in-suite :termis)

(test reads-atoms
  (let ((forms (read-source (make-source "atoms.termis" "foo 42 -42 3.14 \"hello\" ."))))
    (is (= 6 (length forms)))
    (is (string= "foo" (termis-symbol-name (syntax-datum (first forms)))))
    (is (= 42 (syntax-datum (second forms))))
    (is (= -42 (syntax-datum (third forms))))
    (is (= 3.14d0 (syntax-datum (fourth forms))))
    (is (string= "hello" (syntax-datum (fifth forms))))
    (is (unit-literal-p (syntax-datum (sixth forms))))))

(test reads-nested-lists-with-spans
  (let* ((source (make-source "nested.termis" (format nil "(foo~%  (bar 10)~%  baz)")))
         (form (first (read-source source)))
         (nested (second (syntax-datum form))))
    (is (eq source (syntax-source form)))
    (is (= 1 (source-location-line (syntax-start form))))
    (is (= 1 (source-location-column (syntax-start form))))
    (is (= (length (source-contents source)) (source-location-offset (syntax-end form))))
    (is (= 3 (source-location-line (syntax-end form))))
    (is (= 7 (source-location-column (syntax-end form))))
    (is (= 7 (source-location-offset (syntax-start nested))))
    (is (= 15 (source-location-offset (syntax-end nested))))))

(test rejects-dot-prefixed-floats
  (signals termis-read-error
    (read-source (make-source "invalid.termis" ".5"))))

(test reads-multiple-top-level-forms-without-interpreting-them
  (let ((module (compile-string
                 (make-compiler)
                 (format nil "(type Point (x f32) (y f32))~%(defun origin () Point .)")
                 :name "repl.termis")))
    (is (string= "repl.termis" (source-name (module-source module))))
    (is (= 2 (length (module-forms module))))
    (is (string= "type" (termis-symbol-name
                           (syntax-datum (first (syntax-datum (first (module-forms module))))))))))

(test compiles-a-file-to-a-module
  (let ((module (compile-file (make-compiler) #P"examples/hello.termis")))
    (is (search "examples/hello.termis" (source-name (module-source module))))
    (is (= 1 (length (module-forms module))))))

(defun run-tests ()
  (run! :termis))
