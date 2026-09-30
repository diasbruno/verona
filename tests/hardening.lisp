(in-package #:verona/tests)

(in-suite :verona)

(defun captured-diagnostic (thunk)
  "Return the structured diagnostic signalled by THUNK, or fail the test." 
  (handler-case
      (progn (funcall thunk) (error "expected a Verona diagnostic"))
    (verona:user-compilation-error (condition)
      (verona:diagnostic-for-condition condition))))

(defun invalid-fixture (relative-path)
  (uiop:read-file-string
   (merge-pathnames relative-path (asdf:system-source-directory :verona))))

(test diagnostics-have-stable-codes-and-source-ranges
  (let ((diagnostic
          (captured-diagnostic
           (lambda ()
             (verona:read-source (verona:make-source "broken.verona" "("))))))
    (is (string= "E0001" (verona:diagnostic-code diagnostic)))
    (let* ((range (verona:diagnostic-primary-location diagnostic))
           (start (verona:source-range-start range)))
      (is (string= "broken.verona"
                   (verona:source-name (verona:source-location-source start))))
      (is (= 1 (verona:source-location-line start)))
      (is (= 1 (verona:source-location-column start)))))
  (let ((diagnostic
          (captured-diagnostic
           (lambda ()
             (verona:compile-string
              (verona:make-compiler)
              "(function f () i32 missing)" :name "unknown.verona")))))
    (is (string= "E0201" (verona:diagnostic-code diagnostic)))
    (is (typep (verona:diagnostic-primary-location diagnostic) 'verona:source-range))))

(test duplicate-diagnostics-preserve-both-declaration-locations
  (let ((diagnostic
          (captured-diagnostic
           (lambda ()
             (verona:compile-string
              (verona:make-compiler)
              (format nil "(constant answer i32 1)~%(constant answer i32 2)"))))))
    (is (string= "E0101" (verona:diagnostic-code diagnostic)))
    (is (= 1 (length (verona:diagnostic-secondary-locations diagnostic))))
    (let ((previous (first (verona:diagnostic-secondary-locations diagnostic))))
      (is (= 1 (verona:source-location-line
                (verona:source-range-start previous)))))))

(test type-diagnostics-expose-structured-expected-and-actual-values
  (let ((diagnostic
          (captured-diagnostic
           (lambda ()
              (verona:compile-string
              (verona:make-compiler)
              "(function f () i64 (+ 1 2))")))))
    (is (string= "E0401" (verona:diagnostic-code diagnostic)))
    (is (getf (verona:diagnostic-data diagnostic) :expected-type))
    (is (getf (verona:diagnostic-data diagnostic) :actual-type))))

(test macro-expansion-retains-provenance-and-has-a-limit
  (let* ((source (verona:make-source "macro.verona" "(again)"))
         (form (first (verona:read-source source)))
         (environment (verona:make-environment)))
    (verona:environment-bind
     environment (verona:make-verona-name "again")
     (verona:make-verona-macro (lambda (&rest arguments)
                                 (declare (ignore arguments)) form)))
    (let ((verona:*macro-expansion-depth-limit* 2))
      (signals verona:macro-expansion-limit-error
        (verona:expand form environment)))
    ;; A non-recursive expansion carries invocation provenance as a proper
    ;; syntax chain, independently from the source span it reused.
    (verona:environment-bind
     environment (verona:make-verona-name "once")
     (verona:make-verona-macro
      (lambda (&rest arguments)
        (declare (ignore arguments))
        (verona:syntax-with-datum form (verona:make-verona-name "value")))))
    (let* ((once (first (verona:read-source (verona:make-source "macro.verona" "(once)"))))
           (expanded (verona:expand once environment))
           (origin (verona:syntax-expansion-origin expanded)))
      (is (not (null origin)))
      (is (eq once (verona:expansion-origin-invocation origin))))))

(test reader-and-compiler-observability-apis-are-independent
  (let ((verona:*reader-nesting-depth-limit* 1))
    (signals verona:verona-read-error
      (verona:read-verona "((unit))")))
  (let ((compiler (verona:make-compiler :phase-timing-p t)))
    (is (typep (verona:typecheck-verona "(function f () i32 1)")
               'verona:semantic-program))
    (verona:compile-string compiler "(function f () i32 1)")
    (is (equal '(:read :declarations :resolve-and-typecheck)
               (mapcar #'car (verona:compiler-phase-timings compiler))))))

(test invalid-program-corpus-uses-real-verona-files
  (dolist (fixture-and-code
           '(("tests/invalid/reader/unclosed-list.verona" "E0001")
             ("tests/invalid/names/unresolved-name.verona" "E0201")
             ("tests/invalid/types/wrong-return.verona" "E0401")
             ("tests/invalid/types/unknown-type.verona" "E0301")
             ("tests/invalid/match/non-exhaustive-bool.verona" "E0601")))
    (destructuring-bind (path expected-code) fixture-and-code
      (let ((diagnostic
              (captured-diagnostic
               (lambda ()
                 (verona:compile-string (verona:make-compiler)
                                        (invalid-fixture path) :name path)))))
        (is (string= expected-code (verona:diagnostic-code diagnostic)))))))
