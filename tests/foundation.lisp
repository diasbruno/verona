(defpackage #:termis/tests
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:termis #:compile-file)
  (:import-from #:termis
                #:compile-string #:make-compiler #:make-source
                #:module-forms #:module-source #:module-declarations #:module-environment #:module-lookup
                #:read-source #:source-contents #:source-location-offset
                #:source-location-column #:source-location-line #:source-name
                #:syntax-datum #:syntax-end #:syntax-source #:syntax-start
                #:syntax-with-datum #:termis-read-error
                #:termis-name #:termis-name-p #:termis-name-value #:termis-name=
                #:termis-symbol-name #:termis-list-p #:termis-list-elements
                #:make-termis-list #:make-termis-name #:make-termis-function #:make-termis-macro
                #:make-bootstrap-environment #:make-environment #:environment-bind
                #:environment-child #:environment-lookup #:unbound-name-error
                #:evaluate #:expand #:unit-literal-p
                #:declaration-name #:declaration-source #:declaration-module
                #:type-declaration #:type-declaration-body
                #:function-declaration #:function-declaration-parameters
                #:function-declaration-return-type #:function-declaration-body
                #:macro-declaration #:macro-declaration-parameters #:macro-declaration-body
                #:constant-declaration #:constant-declaration-type #:constant-declaration-value
                #:variable-declaration #:variable-declaration-type #:variable-declaration-initializer
                #:duplicate-declaration-error #:termis-macro-p))

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
         (nested (second (termis-list-elements (syntax-datum form)))))
    (is (termis-list-p (syntax-datum form)))
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
                           (syntax-datum
                            (first (termis-list-elements
                                    (syntax-datum (first (module-forms module)))))))))))

(test compiles-a-file-to-a-module
  (let ((module (compile-file (make-compiler) #P"examples/hello.termis")))
    (is (search "examples/hello.termis" (source-name (module-source module))))
    (is (= 1 (length (module-forms module))))))

(test names-are-case-sensitive-and-independent-of-cl-symbols
  (let* ((forms (read-source (make-source "names.termis" "Foo foo")))
         (upper (syntax-datum (first forms)))
         (lower (syntax-datum (second forms))))
    (is (termis-name-p upper))
    (is (not (symbolp upper)))
    (is (string= "Foo" (termis-name-value upper)))
    (is (not (termis-name= upper lower)))
    (is (not (termis-name= (make-termis-name "foo") 'termis/tests::foo)))))

(test resolves-bindings-through-lexical-environments
  (let* ((global (make-environment))
         (child (environment-child global))
         (name (make-termis-name "answer")))
    (environment-bind global name 42)
    (is (= 42 (environment-lookup child (make-termis-name "answer"))))
    (environment-bind child (make-termis-name "answer") 7)
    (is (= 7 (environment-lookup child name)))
    (is (= 42 (environment-lookup global name)))
    (signals unbound-name-error
      (environment-lookup child (make-termis-name "missing")))))

(test evaluates-literals-names-and-nested-calls
  (let* ((environment (make-bootstrap-environment))
         (forms (read-source (make-source "evaluate.termis" "10 (+ 1 (+ 2 3))"))))
    (is (= 10 (evaluate (first forms) environment)))
    (is (= 6 (evaluate (second forms) environment)))))

(test expands-macros-with-unevaluated-syntax-and-recursion
  (let* ((environment (make-environment))
         (received nil)
         (forms (read-source (make-source "macro.termis" "(example foo 42)"))))
    (environment-bind
     environment (make-termis-name "example")
     (make-termis-macro
      (lambda (&rest arguments)
        (setf received arguments)
        (let ((head (syntax-with-datum (first arguments)
                                       (make-termis-name "intermediate"))))
          (syntax-with-datum (first arguments)
                             (apply #'make-termis-list head arguments))))))
    (environment-bind
     environment (make-termis-name "intermediate")
     (make-termis-macro
      (lambda (&rest arguments)
        (let ((head (syntax-with-datum (first arguments)
                                       (make-termis-name "%test-definition"))))
          (syntax-with-datum (first arguments)
                             (apply #'make-termis-list head arguments))))))
    (let* ((expanded (expand (first forms) environment))
           (elements (termis-list-elements (syntax-datum expanded))))
      (is (= 2 (length received)))
      (is (termis-name-p (syntax-datum (first received))))
      (is (string= "foo" (termis-name-value (syntax-datum (first received)))))
      (is (string= "%test-definition"
                   (termis-name-value (syntax-datum (first elements)))))
      (is (string= "foo" (termis-name-value (syntax-datum (second elements))))))))

(test discovers-primitive-definition-declarations
  (let* ((module (compile-string
                  (make-compiler)
                  (format nil "(%type Point (x f64) (y f64))~%\
(%constant pi f64 3.14)~%\
(%variable counter u64 0)~%\
(%function add ((a i32) (b i32)) i32 (+ a b))")
                  :name "definitions.termis"))
         (declarations (module-declarations module))
         (type (first declarations))
         (constant (second declarations))
         (variable (third declarations))
         (function (fourth declarations)))
    (is (= 4 (length declarations)))
    (is (typep type 'type-declaration))
    (is (= 2 (length (type-declaration-body type))))
    (is (typep constant 'constant-declaration))
    (is (typep variable 'variable-declaration))
    (is (typep function 'function-declaration))
    (is (eq module (declaration-module function)))
    (is (eq (first (module-forms module)) (declaration-source type)))
    (is (string= "add" (termis-name-value (declaration-name function))))
    (is (termis-list-p (syntax-datum (function-declaration-parameters function))))
    (is (termis-name-p (syntax-datum (function-declaration-return-type function))))
    (is (termis-list-p (syntax-datum (function-declaration-body function))))
    (is (termis-name-p (syntax-datum (constant-declaration-type constant))))
    (is (= 0 (syntax-datum (variable-declaration-initializer variable))))
    (is (eq type (module-lookup module (make-termis-name "Point"))))))

(test registers-macros-sequentially-in-the-compile-time-environment
  ;; X evaluates to the original, unevaluated syntax argument, making this a
  ;; minimal executable macro body without defining surface macro syntax yet.
  (let* ((module (compile-string
                  (make-compiler)
                  "(%macro identity (x) x) (identity (%type Later body))"
                  :name "macros.termis"))
         (declarations (module-declarations module))
         (macro (first declarations))
         (type (second declarations)))
    (is (= 2 (length declarations)))
    (is (typep macro 'macro-declaration))
    (is (termis-macro-p
         (environment-lookup (module-environment module)
                             (make-termis-name "identity"))))
    (is (string= "Later" (termis-name-value (declaration-name type))))))

(test rejects-duplicate-declarations-across-kinds
  (signals duplicate-declaration-error
    (compile-string (make-compiler)
                    "(%function value () i32 1) (%variable value i32 0)")))

(defun run-tests ()
  (run! :termis))
