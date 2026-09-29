(in-package #:termis/tests)

(in-suite :termis)

(test compiles-a-qualified-module-graph
  (let* ((root (merge-pathnames "tests/modules/" (asdf:system-source-directory :termis)))
         (compiler (make-compiler :search-paths (list root)))
         (entry (compile-module compiler "app"))
         (program (compilation-unit-semantic-program entry)))
    (is (typep entry 'module))
    (is (string= "app" (module-name-string (module-name entry))))
    (is (= 3 (length (program-modules program))))))

(test reader-preserves-qualified-name-structure
  (let* ((form (first (read-source (make-source "qualified.termis" "math:square"))))
         (name (syntax-datum form)))
    (is (qualified-name-p name))
    (is (string= "math" (module-name-string (qualified-name-qualifier name))))
    (is (string= "square" (termis-name-value (qualified-name-name name))))))
