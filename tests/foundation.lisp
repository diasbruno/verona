(defpackage #:termis/tests
  (:use #:cl #:fiveam)
  (:shadowing-import-from #:termis #:compile-file)
  (:import-from #:termis
		#:compile-string #:make-compiler #:make-source
		#:compilation-unit #:compilation-unit-source #:compilation-unit-forms
		#:compilation-unit-semantic-program
		#:unit-declarations #:find-declaration
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
		#:declaration-name #:declaration-source #:declaration-expanded-syntax #:declaration-module
		#:type-declaration #:type-declaration-body
		#:function-declaration #:function-declaration-parameters
		#:function-declaration-return-type #:function-declaration-body
		#:macro-declaration #:macro-declaration-parameters #:macro-declaration-body
		#:constant-declaration #:constant-declaration-type #:constant-declaration-value
		#:variable-declaration #:variable-declaration-type #:variable-declaration-initializer
		#:duplicate-declaration-error #:non-definition-top-level-error #:termis-macro-p
		#:semantic-program-declaration #:semantic-function-declaration
		#:semantic-program-type-context #:semantic-type-declaration
		#:semantic-type-declaration-type
		#:semantic-function-declaration-parameters
		#:semantic-function-declaration-return-type-reference
		#:semantic-function-declaration-return-type
		#:semantic-function-declaration-type
		#:semantic-function-declaration-body
		#:semantic-constant-declaration-initializer
		#:semantic-variable-declaration-initializer
		#:semantic-reference #:semantic-reference-binding
		#:semantic-call #:semantic-call-callee #:semantic-call-arguments
		#:semantic-expression-type #:expression-source
		#:primitive-call #:primitive-call-operation #:conversion-expression
		#:primitive-operation #:primitive-operation-kind #:primitive-operation-parameter-types
		#:primitive-operation-result-type #:primitive-operation-nan-semantics
		#:integer-literal #:boolean-literal #:string-literal
		#:sequence-expression #:sequence-expression-expressions
		#:match-expression #:match-expression-value #:match-expression-cases
		#:match-case #:match-case-pattern #:match-case-scope #:match-case-expression
		#:boolean-pattern #:integer-pattern #:wildcard-pattern #:binding-pattern
		#:binding-pattern-binding #:pattern-binding #:pattern-binding-type
		#:return-expression #:return-expression-value #:never-type
		#:assignment-expression #:assignment-expression-target
		#:address-expression #:dereference-expression
		#:load-expression #:load-expression-place #:store-expression
		#:place-expression-addressable-p #:place-expression-writable-p
		#:parameter-binding #:parameter-binding-type-reference #:parameter-binding-type
		#:semantic-variable-declaration #:semantic-variable-declaration-type
		#:semantic-constant-declaration #:semantic-constant-declaration-type
		#:unit-type #:boolean-type #:integer-type #:integer-type-signed #:integer-type-width
		#:unit-value #:unit-expression #:unit-expression-value
		#:float-type #:float-type-width #:pointer-type #:pointer-type-target
		#:function-type #:function-type-parameters #:function-type-result
		#:make-type-context #:type-context-unit-type #:type-context-unit-value
		#:type-context-unit-representation-type #:unit-machine-representation
		#:defined-type #:defined-type-declaration #:expected-type-error
		#:type-mismatch-error #:not-writable-error #:not-addressable-error
		#:non-exhaustive-match-error #:unreachable-pattern-error
		#:unreachable-expression-error
		#:make-semantic-scope #:semantic-scope-child #:semantic-scope-bind
		#:semantic-scope-lookup
		#:unresolved-name-error #:duplicate-local-binding-error
		#:validate-for-backend))

(in-package #:termis/tests)

(def-suite :termis)
(in-suite :termis)

(test reads-atoms
  (let ((forms (read-source (make-source "atoms.termis" "foo 42 -42 3.14 \"hello\" unit"))))
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
    (read-source (make-source "invalid.termis" ".5")))
  (signals termis-read-error
    (read-source (make-source "invalid.termis" "."))))

(test retains-multiple-source-forms-in-a-compilation-unit
  (let ((module (compile-string
		 (make-compiler)
		 (format nil "(type Point (x f32) (y f32))~%(function origin () i32 1)")
		 :name "repl.termis")))
    (is (typep module 'compilation-unit))
    (is (string= "repl.termis" (source-name (module-source module))))
    (is (= 2 (length (module-forms module))))
    (is (string= "type" (termis-symbol-name
			   (syntax-datum
			    (first (termis-list-elements
				    (syntax-datum (first (module-forms module)))))))))))

(test compiles-a-file-to-a-compilation-unit
  (let* ((pathname #P"/tmp/termis-compilation-unit-test.termis")
	 (contents "(constant answer i32 42)")
	 (module nil))
    (with-open-file (stream pathname :direction :output :if-exists :supersede)
      (write-string contents stream))
    (setf module (compile-file (make-compiler) pathname))
    (is (search "termis-compilation-unit-test.termis" (source-name (module-source module))))
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

(test bootstraps-the-public-definition-vocabulary-as-macros
  (let ((environment (make-bootstrap-environment)))
    (dolist (name '("type" "function" "macro" "constant" "variable"))
      (is (termis-macro-p
	   (environment-lookup environment (make-termis-name name)))))))

(test bootstrap-definition-macros-mechanically-rewrite-their-heads
  (let ((environment (make-bootstrap-environment)))
    (dolist (specification '(("type" . "%type")
			     ("function" . "%function")
			     ("macro" . "%macro")
			     ("constant" . "%constant")
			     ("variable" . "%variable")))
      (let* ((form (first (read-source
			   (make-source "expansion.termis"
					(format nil "(~A declaration payload)"
						(car specification))))))
	     (original-tail (rest (termis-list-elements (syntax-datum form))))
	     (expanded (expand form environment))
	     (expanded-elements (termis-list-elements (syntax-datum expanded))))
	(is (string= (cdr specification)
		     (termis-name-value (syntax-datum (first expanded-elements)))))
	(is (every #'eq original-tail (rest expanded-elements)))))))

(test compiles-surface-definition-macros-without-interpreting-their-content
  (let* ((contents
	   (format nil "(type Point (x f64) (y f64))~%\
 (constant pi f64 3.141592653589793)~%\
 (variable counter u64 0)~%\
 (function calculate ((x i32)) i32 (+ x 1))"))
	 (module (compile-string (make-compiler) contents
				 :name "surface-definitions.termis"))
	 (declarations (module-declarations module))
	 (function (fourth declarations)))
    (is (= 4 (length declarations)))
    (is (typep (first declarations) 'type-declaration))
    (is (typep (second declarations) 'constant-declaration))
    (is (typep (third declarations) 'variable-declaration))
    (is (typep function 'function-declaration))
    ;; The declaration remains raw syntax even though Step 7 also builds a
    ;; separate resolved semantic body.
    (let* ((body (function-declaration-body function))
	   (head (first (termis-list-elements (syntax-datum body)))))
      (is (string= "+"
		   (termis-name-value (syntax-datum head)))))))

(test makes-user-macros-available-after-the-surface-macro-declaration
  (let* ((module (compile-string
		  (make-compiler)
		  "(macro identity (x) x) (identity (type Later (value i32)))"
		  :name "surface-macros.termis"))
	 (declarations (module-declarations module))
	 (macro (first declarations))
	 (type (second declarations)))
    (is (= 2 (length declarations)))
    (is (typep macro 'macro-declaration))
    (is (termis-macro-p
	 (environment-lookup (module-environment module)
			     (make-termis-name "identity"))))
    (is (typep type 'type-declaration))
    (is (string= "Later" (termis-name-value (declaration-name type))))))

(test retains-original-and-expanded-declaration-syntax
  (let* ((module (compile-string
		  (make-compiler)
		  "(macro identity (x) x) (identity (type Later (value i32)))"
		  :name "expanded.termis"))
	 (source (second (module-forms module)))
	 (declaration (second (unit-declarations module)))
	 (expanded (declaration-expanded-syntax declaration)))
    (is (eq source (declaration-source declaration)))
    (is (not (eq source expanded)))
    (is (string= "identity"
		 (termis-name-value
		  (syntax-datum (first (termis-list-elements (syntax-datum source)))))))
    (is (string= "%type"
		 (termis-name-value
		  (syntax-datum (first (termis-list-elements (syntax-datum expanded)))))))))

(test permits-forward-references-and-uses-the-compilation-unit-namespace
  (let* ((module (compile-string
		  (make-compiler)
		  "(function first () i32 (second)) (function second () i32 42)"))
	 (first (first (unit-declarations module)))
	 (second (second (unit-declarations module))))
    (is (= 2 (length (unit-declarations module))))
    (is (eq first (find-declaration module (make-termis-name "first"))))
    (is (eq second (find-declaration module (make-termis-name "second"))))))

(test allows-a-macro-to-generate-multiple-definitions
  (let* ((module (compile-string
		  (make-compiler)
		  "(macro make-pair (left right) (definitions left right))\
		   (make-pair (constant first i32 1) (constant second i32 2))"))
	 (declarations (unit-declarations module)))
    (is (= 3 (length declarations)))
    (is (every (lambda (declaration)
		 (typep declaration 'constant-declaration))
	       (rest declarations)))
    (is (equal '("make-pair" "first" "second")
	       (mapcar (lambda (declaration)
			 (termis-name-value (declaration-name declaration)))
		       declarations)))))

(test macro-generated-macros-affect-following-source-forms
  (let* ((module (compile-string
		  (make-compiler)
		  "(macro define-identity (definition) definition)\
		   (define-identity (%macro identity (x) x))\
		   (identity (constant answer i32 42))"))
	 (declarations (unit-declarations module)))
    (is (= 3 (length declarations)))
    (is (typep (first declarations) 'macro-declaration))
    (is (typep (second declarations) 'macro-declaration))
    (is (typep (third declarations) 'constant-declaration))))

(test rejects-non-definition-top-level-expansion
  (signals non-definition-top-level-error
    (compile-string (make-compiler) "(+ 1 2)")))

(test surface-definition-forms-are-not-compiler-primitives
  (let* ((forms (read-source (make-source "boundary.termis"
					  "(function foo () i32 1) (%function foo () i32 1)")))
	 (empty-environment (make-environment)))
    (is (eq (first forms) (expand (first forms) empty-environment)))
    (is (eq (second forms) (expand (second forms) empty-environment)))
    (is (= 1 (length (module-declarations
		      (compile-string (make-compiler) "(%function foo () i32 1)")))))))

(test rejects-duplicate-declarations-across-kinds
  (signals duplicate-declaration-error
    (compile-string (make-compiler)
		    "(%function value () i32 1) (%variable value i32 0)")))

(test duplicate-definition-diagnostic-includes-both-locations
  (let ((condition
	  (handler-case
	      (compile-string (make-compiler)
			      (format nil "(constant answer i32 1)~%(variable answer i32 0)")
			      :name "duplicates.termis")
	    (duplicate-declaration-error (condition) condition))))
    (is (not (null condition)))
    (let ((message (format nil "~A" condition)))
      (is (search "duplicates.termis:2:1: duplicate definition `answer`" message))
      (is (search "previous definition:" message))
      (is (search "duplicates.termis:1:1" message)))))

(test resolves-global-bindings-signatures-and-forward-calls-by-identity
  (let* ((unit (compile-string
                (make-compiler)
                "(type Point body)\
                 (constant origin i32 0)\
                 (function second ((p Point)) i32 origin)\
                 (function first ((p Point)) i32 (second p))"))
         (declarations (unit-declarations unit))
         (point (first declarations))
         (origin (second declarations))
         (second (third declarations))
         (first (fourth declarations))
         (program (compilation-unit-semantic-program unit))
         (second-semantic (semantic-program-declaration program second))
         (first-semantic (semantic-program-declaration program first))
         (first-parameter (first (semantic-function-declaration-parameters
                                  first-semantic)))
         (first-body (semantic-function-declaration-body first-semantic)))
    ;; Signature type names point at the exact source declaration/builtin.
    (is (eq point
            (semantic-reference-binding
             (parameter-binding-type-reference first-parameter))))
    ;; A bare body name is a reference to the constant declaration itself.
    (is (eq origin
            (semantic-reference-binding
             (semantic-function-declaration-body second-semantic))))
    ;; A forward function call and its parameter use each retain identity.
    (is (typep first-body 'semantic-call))
    (is (eq second
            (semantic-reference-binding (semantic-call-callee first-body))))
    ;; A parameter used as a value carries an explicit semantic load; its
    ;; place identity remains available to address-of and store operations.
    (is (typep (first (semantic-call-arguments first-body)) 'load-expression))
    (is (eq first-parameter
            (semantic-reference-binding
             (load-expression-place
              (first (semantic-call-arguments first-body))))))))

(test resolves-builtins-and-shadows-global-bindings-with-parameters
  (let* ((unit (compile-string
                (make-compiler)
                "(variable x i32 10) (function foo ((x i32)) i32 (+ x 1))"))
         (global (first (unit-declarations unit)))
         (function (second (unit-declarations unit)))
         (semantic (semantic-program-declaration
                    (compilation-unit-semantic-program unit) function))
         (parameter (first (semantic-function-declaration-parameters semantic)))
         (body (semantic-function-declaration-body semantic))
         (argument (first (semantic-call-arguments body))))
    (is (typep argument 'load-expression))
    (is (not (eq global (semantic-reference-binding (load-expression-place argument)))))
    (is (eq parameter (semantic-reference-binding (load-expression-place argument))))
    (is (not (null (semantic-reference-binding
                    (semantic-function-declaration-return-type-reference
                     semantic)))))
    ;; + resolves through the bootstrap semantic scope, not special text.
    (is (typep (semantic-call-callee body) 'semantic-reference))
    (is (not (null (semantic-reference-binding
                    (semantic-call-callee body)))))))

(test semantic-scopes-support-nested-lookup-and-identity-shadowing
  (let* ((global (make-semantic-scope))
         (function (semantic-scope-child global))
         (lexical (semantic-scope-child function))
         (name (make-termis-name "x"))
         (outer (make-instance 'parameter-binding :name name))
         (inner (make-instance 'parameter-binding :name name)))
    (semantic-scope-bind global name outer)
    (is (eq outer (semantic-scope-lookup lexical name)))
    (semantic-scope-bind function name inner)
    (is (eq inner (semantic-scope-lookup lexical name)))
    (is (eq outer (semantic-scope-lookup global name)))))

(test reports-unresolved-runtime-names-and-duplicate-parameters
  (signals unresolved-name-error
    (compile-string (make-compiler) "(function foo () i32 unknown)"))
  (signals unresolved-name-error
    ;; The macro is available only to expansion, not to semantic lookup.
    (compile-string (make-compiler)
                    "(macro compile-only () 0) (function foo () i32 compile-only)"))
  (signals duplicate-local-binding-error
    (compile-string (make-compiler)
                    "(function foo ((x i32) (x i32)) i32 x)")))

(test resolves-canonical-primitive-pointer-and-function-types
  (let* ((unit (compile-string
                (make-compiler)
                "(type Node (value i32) (next (pointer Node)))\
                 (variable current (pointer Node) (deref (& current)))\
                 (function distance ((a (pointer Node))\
                                     (b (pointer (pointer i32)))) f64 3.14)\
                 (function another-distance ((a (pointer Node))\
                                             (b (pointer (pointer i32)))) f64 3.14)\
                 (function nothing () unit unit)"))
         (declarations (unit-declarations unit))
         (node (first declarations))
         (current (second declarations))
         (distance (third declarations))
         (another-distance (fourth declarations))
         (nothing (fifth declarations))
         (program (compilation-unit-semantic-program unit))
         (node-semantic (semantic-program-declaration program node))
         (current-semantic (semantic-program-declaration program current))
         (distance-semantic (semantic-program-declaration program distance))
         (another-semantic (semantic-program-declaration program another-distance))
         (nothing-semantic (semantic-program-declaration program nothing))
         (node-type (semantic-type-declaration-type node-semantic))
         (current-type (semantic-variable-declaration-type current-semantic))
         (parameters (semantic-function-declaration-parameters distance-semantic))
         (first-parameter-type (parameter-binding-type (first parameters)))
         (second-parameter-type (parameter-binding-type (second parameters))))
    (is (typep node-type 'defined-type))
    (is (eq node (defined-type-declaration node-type)))
    (is (typep current-type 'pointer-type))
    (is (eq node-type (pointer-type-target current-type)))
    ;; Repeated pointer syntax reuses the same interned object.
    (is (eq current-type first-parameter-type))
    (is (typep second-parameter-type 'pointer-type))
    (is (typep (pointer-type-target second-parameter-type) 'pointer-type))
    (let ((integer (pointer-type-target
                    (pointer-type-target second-parameter-type))))
      (is (typep integer 'integer-type))
      (is (integer-type-signed integer))
      (is (= 32 (integer-type-width integer))))
    (is (typep (semantic-function-declaration-return-type distance-semantic)
               'float-type))
    (is (= 64 (float-type-width
               (semantic-function-declaration-return-type distance-semantic))))
    (is (typep (semantic-function-declaration-type distance-semantic) 'function-type))
    (is (eq (semantic-function-declaration-type distance-semantic)
            (semantic-function-declaration-type another-semantic)))
    (is (typep (semantic-function-declaration-return-type nothing-semantic)
               'unit-type))))

(test rejects-resolved-names-that-do-not-denote-types
  (signals expected-type-error
    (compile-string (make-compiler)
                    "(function value () i32 unit) (variable counter value 0)")))

(test bootstraps-a-canonical-boolean-type
  (let* ((unit (compile-string (make-compiler)
                               "(function predicate ((value bool)) bool true)"))
         (semantic (semantic-program-declaration
                    (compilation-unit-semantic-program unit)
                    (first (unit-declarations unit))))
         (parameter (first (semantic-function-declaration-parameters semantic))))
    (is (typep (parameter-binding-type parameter) 'boolean-type))
    (is (eq (parameter-binding-type parameter)
            (semantic-function-declaration-return-type semantic)))))

(test uses-unit-for-the-unit-type-and-value
  (let* ((unit (compile-string (make-compiler)
                               "(function no-op () unit unit)"))
         (semantic (semantic-program-declaration
                    (compilation-unit-semantic-program unit)
                    (first (unit-declarations unit)))))
    (is (typep (semantic-function-declaration-return-type semantic) 'unit-type))))

(test analyzes-typed-expressions-and-contextual-initializers
  (let* ((unit (compile-string
                (make-compiler)
                "(constant initial i32 0)\
                 (variable counter i32 initial)\
                 (function add ((a i32) (b i32)) i32 (+ a b))\
                 (function increment ((value i32)) i32 (add value 1))\
                 (function reset () unit (do (assign counter 0) unit))\
                 (function main () i32 (increment counter))"))
         (declarations (unit-declarations unit))
         (program (compilation-unit-semantic-program unit))
         (initial (semantic-program-declaration program (first declarations)))
         (counter (semantic-program-declaration program (second declarations)))
         (add (semantic-program-declaration program (third declarations)))
         (increment (semantic-program-declaration program (fourth declarations)))
         (reset (semantic-program-declaration program (fifth declarations)))
         (main (semantic-program-declaration program (sixth declarations)))
         (initializer (semantic-constant-declaration-initializer initial))
         (add-body (semantic-function-declaration-body add))
         (increment-body (semantic-function-declaration-body increment))
         (reset-body (semantic-function-declaration-body reset))
         (assignment (first (sequence-expression-expressions reset-body))))
    (is (typep initializer 'integer-literal))
    (is (eq (semantic-constant-declaration-type initial)
            (semantic-expression-type initializer)))
    (is (typep (semantic-variable-declaration-initializer counter) 'semantic-reference))
    (is (typep add-body 'semantic-call))
    (is (eq (semantic-function-declaration-return-type add)
            (semantic-expression-type add-body)))
    (is (typep (second (semantic-call-arguments increment-body)) 'integer-literal))
    (is (typep reset-body 'sequence-expression))
    (is (typep assignment 'assignment-expression))
    (is (eq (semantic-function-declaration-return-type reset)
            (semantic-expression-type reset-body)))
    (is (typep (semantic-function-declaration-body main) 'semantic-call))
    ;; Nested expressions retain the exact syntax that produced them.
    (is (not (null (expression-source assignment))))))

(test diagnoses-type-mismatches-and-invalid-places
  (signals type-mismatch-error
    (compile-string (make-compiler) "(function wrong () i32 false)"))
  (signals not-writable-error
    (compile-string (make-compiler)
                    "(constant answer i32 42) (function change () unit (assign answer 1))"))
  (signals not-addressable-error
    (compile-string (make-compiler)
                    "(function address () (pointer i32) (& (+ 1 2)))")))

(test bootstraps-concrete-primitive-identities-and-explicit-conversions
  (let* ((unit (compile-string
		(make-compiler)
		"(function widen ((value i32)) i64 (%sext-primitive-i32-i64 value))\
                 (function compare ((left f64) (right f64)) bool (%<-primitive-f64 left right))"))
	 (program (compilation-unit-semantic-program unit))
	 (widen (semantic-program-declaration program (first (unit-declarations unit))) )
	 (compare (semantic-program-declaration program (second (unit-declarations unit))))
	 (conversion (semantic-function-declaration-body widen))
	 (comparison (semantic-function-declaration-body compare)))
    (is (typep conversion 'conversion-expression))
    (is (typep (primitive-call-operation conversion) 'primitive-operation))
    (is (eq :integer-sign-extend
	    (primitive-operation-kind (primitive-call-operation conversion))))
    (is (typep comparison 'primitive-call))
    (is (eq :float-ordered-less-than
	    (primitive-operation-kind (primitive-call-operation comparison))))
    (is (eq :ordered-false
	    (primitive-operation-nan-semantics (primitive-call-operation comparison))))
    (is (eq program (validate-for-backend program)))))

(test enforces-exact-primitive-types-and-explicit-memory-reads
  (signals type-mismatch-error
    (compile-string (make-compiler)
                    "(function wrong ((value i32)) i64 (%+-primitive-i64 value 1))"))
  (let* ((unit (compile-string
		(make-compiler)
		"(function read ((address (pointer i64))) i64 (load (dereference address)))\
                 (function write ((address (pointer i64)) (value i64)) unit\
                   (do (store (dereference address) value) unit))"))
	 (program (compilation-unit-semantic-program unit))
	 (read-function (semantic-program-declaration program (first (unit-declarations unit))))
	 (write-function (semantic-program-declaration program (second (unit-declarations unit))))
	 (write-body (semantic-function-declaration-body write-function)))
    (is (typep (semantic-function-declaration-body read-function) 'load-expression))
    (is (typep (load-expression-place (semantic-function-declaration-body read-function))
	       'dereference-expression))
    (is (typep (first (sequence-expression-expressions write-body)) 'store-expression))
    (is (eq program (validate-for-backend program)))))

(test models-unit-as-a-distinct-singleton-with-pointer-width-representation
  (let ((context32 (make-type-context :pointer-width 32))
	(context64 (make-type-context :pointer-width 64)))
    (is (typep (type-context-unit-type context32) 'unit-type))
    (is (typep (type-context-unit-value context32) 'unit-value))
    (is (= 32 (integer-type-width (type-context-unit-representation-type context32))))
    (is (= 64 (integer-type-width (type-context-unit-representation-type context64))))
    (is (not (eq (type-context-unit-type context64)
		 (type-context-unit-representation-type context64))))
    (is (= 0 (unit-machine-representation context64 (type-context-unit-value context64))))))

(test resolves-match-patterns-scopes-and-never
  (let* ((unit (compile-string
		(make-compiler)
		"(function choose ((enabled bool) (x i64)) i64 (match enabled (true x) (false 0)))\
                 (function identity ((value i64)) i64 (match value (bound bound)))\
                 (function early ((enabled bool)) i64 (match enabled (true (return 10)) (false 20)))"))
	 (program (compilation-unit-semantic-program unit))
	 (choose (semantic-program-declaration program (first (unit-declarations unit))))
	 (identity (semantic-program-declaration program (second (unit-declarations unit))))
	 (early (semantic-program-declaration program (third (unit-declarations unit))))
	 (choose-body (semantic-function-declaration-body choose))
	 (identity-body (semantic-function-declaration-body identity))
	 (early-body (semantic-function-declaration-body early)))
    (is (typep choose-body 'match-expression))
    (is (typep (match-case-pattern (first (match-expression-cases choose-body))) 'boolean-pattern))
    (is (typep (match-case-pattern (second (match-expression-cases choose-body))) 'boolean-pattern))
    (is (typep (match-case-pattern (first (match-expression-cases identity-body))) 'binding-pattern))
    (is (typep (binding-pattern-binding
		(match-case-pattern (first (match-expression-cases identity-body)))) 'pattern-binding))
    (is (typep (match-case-expression (first (match-expression-cases early-body)))
	       'return-expression))
    (is (typep (semantic-expression-type
		(match-case-expression (first (match-expression-cases early-body)))) 'never-type))
    (is (eq program (validate-for-backend program)))))

(test diagnoses-match-exhaustiveness-reachability-and-termination
  (signals non-exhaustive-match-error
    (compile-string (make-compiler) "(function bad ((x bool)) i64 (match x (true 1)))"))
  (signals non-exhaustive-match-error
    (compile-string (make-compiler) "(function bad ((x i64)) i64 (match x (0 1) (1 2)))"))
  (signals unreachable-pattern-error
    (compile-string (make-compiler) "(function bad ((x i64)) i64 (match x (_ 1) (0 2)))"))
  (signals unreachable-pattern-error
    (compile-string (make-compiler) "(function bad ((x bool)) i64 (match x (true 1) (true 2) (false 3)))"))
  (signals unreachable-expression-error
    (compile-string (make-compiler) "(function bad () i64 (do (return 1) 2))")))

(defun run-tests ()
  (run! :termis))
