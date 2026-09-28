(in-package #:termis)

;;; This file is the syntax-to-semantics boundary.  It first records binding
;;; identity, then turns executable syntax into typed runtime expressions.

(defclass builtin-binding (semantic-binding) ())
(defclass builtin-type-binding (builtin-binding)
  ((type :initarg :type :reader builtin-type-binding-type)))
;; Builtins are declarations/entities in the semantic namespace, but unlike
;; source TYPE-DECLARATIONs they have no source form to retain.
(defclass builtin-type-declaration (builtin-type-binding) ())
(defclass builtin-intrinsic-binding (builtin-binding)
  ((type :initarg :type :reader builtin-intrinsic-binding-type)))

;;; Types are compiler semantics, deliberately independent from any lowering
;;; target.  In particular, BOOLEAN-TYPE does not imply an LLVM integer type.
(defclass termis-type () ())

(defclass unit-type (termis-type) ())
(defclass boolean-type (termis-type) ())
(defclass string-type (termis-type) ())
(defclass integer-type (termis-type)
  ((signed :initarg :signed :reader integer-type-signed)
   (width :initarg :width :reader integer-type-width)))
(defclass float-type (termis-type)
  ((width :initarg :width :reader float-type-width)))
(defclass pointer-type (termis-type)
  ((target :initarg :target :reader pointer-type-target)))
(defclass function-type (termis-type)
  ((parameters :initarg :parameters :reader function-type-parameters)
   (result :initarg :result :reader function-type-result)))
(defclass defined-type (termis-type)
  ((declaration :initarg :declaration :reader defined-type-declaration)))

(defclass type-context ()
  ((unit-type :reader type-context-unit-type)
   (boolean-type :reader type-context-boolean-type)
   (string-type :reader type-context-string-type)
   (integer-types :initform '() :accessor type-context-integer-types)
   (float-types :initform '() :accessor type-context-float-types)
   (pointer-types :initform '() :accessor type-context-pointer-types)
   (function-types :initform '() :accessor type-context-function-types)
   ;; This association uses declaration object identity, never its spelling.
   (defined-types :initform '() :accessor type-context-defined-types)))

(defun make-type-context ()
  "Create the canonical Termis types for one semantic program."
  (let ((context (make-instance 'type-context)))
    (setf (slot-value context 'unit-type) (make-instance 'unit-type)
	  (slot-value context 'boolean-type) (make-instance 'boolean-type)
	  (slot-value context 'string-type) (make-instance 'string-type))
    (dolist (specification '((t 8) (t 16) (t 32) (t 64)
			     (nil 8) (nil 16) (nil 32) (nil 64)))
      (destructuring-bind (signed width) specification
	(push (cons (cons signed width)
		    (make-instance 'integer-type :signed signed :width width))
	      (type-context-integer-types context))))
    (dolist (width '(32 64))
      (push (cons width (make-instance 'float-type :width width))
	    (type-context-float-types context)))
    context))

(defun type-context-integer-type (context signed width)
  (or (cdr (assoc (cons signed width) (type-context-integer-types context)
		  :test #'equal))
      (error "No builtin integer type with signedness ~S and width ~S" signed width)))

(defun type-context-float-type (context width)
  (or (cdr (assoc width (type-context-float-types context)))
      (error "No builtin float type with width ~S" width)))

(defun type-context-pointer-type (context target)
  "Return the canonical pointer-to-TARGET type in CONTEXT."
  (check-type target termis-type)
  (or (cdr (assoc target (type-context-pointer-types context) :test #'eq))
      (let ((type (make-instance 'pointer-type :target target)))
	(push (cons target type) (type-context-pointer-types context))
	type)))

(defun type-context-function-type (context parameters result)
  "Return the canonical function type with PARAMETERS and RESULT in CONTEXT."
  (dolist (parameter parameters)
    (check-type parameter termis-type))
  (check-type result termis-type)
  (let ((key (cons parameters result)))
    (or (cdr (assoc key (type-context-function-types context) :test #'equal))
	(let ((type (make-instance 'function-type
				   :parameters parameters :result result)))
	  (push (cons key type) (type-context-function-types context))
	  type))))

(defun type-context-defined-type (context declaration)
  "Return DECLARATION's nominal type, creating its identity at most once."
  (or (cdr (assoc declaration (type-context-defined-types context) :test #'eq))
      (let ((type (make-instance 'defined-type :declaration declaration)))
	(push (cons declaration type) (type-context-defined-types context))
	type)))

;;; These nodes retain the resolved structure of compound type syntax until
;;; the type pass turns them into TERMIS-TYPE objects.  A bare type name stays
;;; a SEMANTIC-REFERENCE, preserving the Step 7 representation and API.
(defclass semantic-type-syntax ()
  ((syntax :initarg :syntax :reader semantic-type-syntax-syntax)))
(defclass semantic-unit-type-syntax (semantic-type-syntax) ())
(defclass semantic-pointer-type-syntax (semantic-type-syntax)
  ((target :initarg :target :reader semantic-pointer-type-syntax-target)))

(defclass parameter-binding (semantic-binding)
  ((syntax :initarg :syntax :reader parameter-binding-syntax)
   (type-syntax :initarg :type-syntax :reader parameter-binding-type-syntax)
   (type-reference :initform nil :accessor parameter-binding-type-reference)
   (type :initform nil :accessor parameter-binding-type)))

(defclass semantic-program ()
  ((bootstrap-scope :initarg :bootstrap-scope
                    :reader semantic-program-bootstrap-scope)
   (module-scope :initarg :module-scope :reader semantic-program-module-scope)
   (type-context :initarg :type-context :reader semantic-program-type-context)
   ;; Entries map source declarations to their resolved counterpart.  Macro
   ;; declarations deliberately have no entry: they belong to expansion.
   (declarations :initform '() :accessor semantic-program-declarations)))

(defclass semantic-declaration ()
  ((source-declaration :initarg :source-declaration
                       :reader semantic-declaration-source-declaration)))

(defclass semantic-type-declaration (semantic-declaration)
  ((type :initform nil :accessor semantic-type-declaration-type)))

(defclass semantic-constant-declaration (semantic-declaration)
  ((type-reference :initform nil
                   :accessor semantic-constant-declaration-type-reference)
   (type :initform nil :accessor semantic-constant-declaration-type)
   (initializer :initform nil
                :accessor semantic-constant-declaration-initializer)))

(defclass semantic-variable-declaration (semantic-declaration)
  ((type-reference :initform nil
                   :accessor semantic-variable-declaration-type-reference)
   (type :initform nil :accessor semantic-variable-declaration-type)
   (initializer :initform nil
                :accessor semantic-variable-declaration-initializer)))

(defclass semantic-function-declaration (semantic-declaration)
  ((scope :initform nil :accessor semantic-function-declaration-scope)
   (parameters :initform '() :accessor semantic-function-declaration-parameters)
   (return-type-reference :initform nil
                          :accessor semantic-function-declaration-return-type-reference)
   (return-type :initform nil
                :accessor semantic-function-declaration-return-type)
   (type :initform nil :accessor semantic-function-declaration-type)
   (body :initform nil :accessor semantic-function-declaration-body)))

;; EXPRESSION is the typed runtime semantic model.  The SEMANTIC-* classes
;; remain concrete compatibility names for clients of the earlier passes.
(defclass expression ()
  ((syntax :initarg :syntax :reader expression-syntax :reader expression-source
	   :reader semantic-expression-syntax)
   (type :initarg :type :initform nil
	 :reader expression-type :reader semantic-expression-type)))
(defclass semantic-expression (expression) ())

(defclass semantic-literal (semantic-expression) ())
(defclass unit-expression (semantic-literal) ())
(defclass boolean-literal (semantic-literal)
  ((value :initarg :value :reader boolean-literal-value)))
(defclass integer-literal (semantic-literal)
  ((value :initarg :value :reader integer-literal-value)))
(defclass float-literal (semantic-literal)
  ((value :initarg :value :reader float-literal-value)))
(defclass string-literal (semantic-literal)
  ((value :initarg :value :reader string-literal-value)))

(defclass place-expression ()
  ((addressable :initarg :addressable :initform nil :reader place-expression-addressable-p)
   (writable :initarg :writable :initform nil :reader place-expression-writable-p)))

(defclass reference-expression (semantic-expression place-expression)
  ((name :initarg :name :reader semantic-reference-name)
   (binding :initarg :binding :reader semantic-reference-binding)))
(defclass semantic-reference (reference-expression) ())

(defclass call-expression (semantic-expression)
  ((callee :initarg :callee :reader semantic-call-callee)
   (arguments :initarg :arguments :reader semantic-call-arguments)))
(defclass semantic-call (call-expression) ())
(defclass sequence-expression (semantic-expression)
  ((expressions :initarg :expressions :reader sequence-expression-expressions)))
(defclass address-expression (semantic-expression)
  ((operand :initarg :operand :reader address-expression-operand)))
(defclass dereference-expression (semantic-expression place-expression)
  ((operand :initarg :operand :reader dereference-expression-operand)))
(defclass assignment-expression (semantic-expression)
  ((target :initarg :target :reader assignment-expression-target)
   (value :initarg :value :reader assignment-expression-value)))

(defun semantic-program-declaration (program declaration)
  "Return DECLARATION's resolved semantic node, or NIL for macro declarations."
  (check-type program semantic-program)
  (check-type declaration declaration)
  (cdr (assoc declaration (semantic-program-declarations program) :test #'eq)))

(defun make-bootstrap-semantic-scope (&optional (type-context (make-type-context)))
  "Create compiler-provided semantic bindings for TYPE-CONTEXT.

Builtin names are bindings in the same semantic environment as user
declarations.  Their canonical type object is attached to that binding rather
than recovered later through ad-hoc string comparisons."
  (let ((scope (make-semantic-scope)))
    (setf (semantic-scope-type-context scope) type-context)
    (flet ((bind-type (name type)
	     (semantic-scope-bind scope (make-termis-name name)
				  (make-instance 'builtin-type-declaration
						 :name (make-termis-name name)
						 :type type))))
      (bind-type "bool" (type-context-boolean-type type-context))
      (bind-type "string" (type-context-string-type type-context))
      (dolist (specification '(("i8" t 8) ("i16" t 16)
			       ("i32" t 32) ("i64" t 64)
			       ("u8" nil 8) ("u16" nil 16)
			       ("u32" nil 32) ("u64" nil 64)))
	(destructuring-bind (name signed width) specification
	  (bind-type name (type-context-integer-type type-context signed width))))
      (dolist (specification '(("f32" 32) ("f64" 64)))
	(destructuring-bind (name width) specification
	  (bind-type name (type-context-float-type type-context width)))))
    ;; Intrinsics are ordinary callable bindings.  The initial language has a
    ;; deliberately small, monomorphic numeric surface; overload resolution
    ;; and numeric promotion remain future work.
    (dolist (name '("+" "-" "*" "/"))
      (semantic-scope-bind scope (make-termis-name name)
			   (make-instance 'builtin-intrinsic-binding
					  :name (make-termis-name name)
					  :type
					  (type-context-function-type
					   type-context
					   (list (type-context-integer-type type-context t 32)
						 (type-context-integer-type type-context t 32))
					   (type-context-integer-type type-context t 32)))))
    scope))

(defun resolve-name (scope syntax)
  "Resolve a name SYNTAX into a semantic reference in SCOPE."
  (let ((name (syntax-datum syntax)))
    (unless (termis-name-p name)
      (error 'semantic-error :syntax syntax :message "expected a Termis name"))
    (multiple-value-bind (binding foundp) (semantic-scope-find scope name)
      (unless foundp
	(error 'unresolved-name-error :name name :syntax syntax))
      (make-instance 'semantic-reference :syntax syntax :name name :binding binding))))

(defun build-semantic-expression (scope syntax)
  "Build a resolved expression from syntax in SCOPE.

This is intentionally small: atoms become literals, names become references,
and lists become calls.  Future expression forms can introduce child scopes
without changing the scope or binding model established here."
  (check-type scope semantic-scope)
  (check-type syntax syntax)
  (let ((datum (syntax-datum syntax)))
    (cond ((termis-name-p datum) (resolve-name scope syntax))
	  ((termis-list-p datum)
	   (let ((elements (termis-list-elements datum)))
	     (unless elements
	       (error 'semantic-error :syntax syntax
				      :message "an empty list is not an expression"))
	     (make-instance 'semantic-call
			    :syntax syntax
			    :callee (build-semantic-expression scope (first elements))
			    :arguments (mapcar (lambda (element)
						 (build-semantic-expression scope element))
					       (rest elements)))))
	  (t (make-instance 'semantic-literal :syntax syntax)))))

(defun resolve-type-syntax (scope syntax)
  "Resolve the names embedded in a restricted type-language syntax tree.

This is deliberately distinct from BUILD-SEMANTIC-EXPRESSION: POINTER is a
type constructor, never a runtime call.  The result is still syntax-shaped so
the following type pass can turn it into canonical TERMIS-TYPE objects."
  (check-type scope semantic-scope)
  (check-type syntax syntax)
  (let ((datum (syntax-datum syntax)))
    (cond ((termis-name-p datum) (resolve-name scope syntax))
	  ((unit-literal-p datum)
	   (make-instance 'semantic-unit-type-syntax :syntax syntax))
	  ((termis-list-p datum)
	   (let ((elements (termis-list-elements datum)))
	     (unless (= (length elements) 2)
	       (error 'semantic-error :syntax syntax
				      :message "a type constructor requires exactly one argument"))
	     (let ((head (syntax-datum (first elements))))
	       (unless (and (termis-name-p head)
			    (string= (termis-name-value head) "pointer"))
		 (error 'semantic-error :syntax (first elements)
					:message "unknown type constructor"))
	       (make-instance 'semantic-pointer-type-syntax
			      :syntax syntax
			      :target (resolve-type-syntax scope (second elements))))))
	  (t (error 'semantic-error :syntax syntax :message "expected a type")))))

(define-condition expected-type-error (semantic-error)
  ((binding :initarg :binding :reader expected-type-error-binding))
  (:default-initargs :message "expected a type")
  (:report (lambda (condition stream)
	     (let* ((syntax (semantic-error-syntax condition))
		    (name (and (typep syntax 'syntax)
			       (syntax-datum syntax))))
	       (if (termis-name-p name)
		   (format stream "~A is not a type"
			   (termis-name-value name))
		   (format stream "expected a type"))))))

(defun resolve-type (type-context resolved-type-syntax)
  "Turn resolved type syntax into a canonical, backend-independent type."
  (check-type type-context type-context)
  (cond ((typep resolved-type-syntax 'semantic-reference)
	 (let ((binding (semantic-reference-binding resolved-type-syntax)))
	   (cond ((typep binding 'builtin-type-binding)
		  (builtin-type-binding-type binding))
		 ((typep binding 'type-declaration)
		  (type-context-defined-type type-context binding))
		 (t (error 'expected-type-error
			   :syntax (semantic-expression-syntax resolved-type-syntax)
			   :binding binding)))))
	((typep resolved-type-syntax 'semantic-unit-type-syntax)
	 (type-context-unit-type type-context))
	((typep resolved-type-syntax 'semantic-pointer-type-syntax)
	 (type-context-pointer-type
	  type-context
	  (resolve-type type-context
			(semantic-pointer-type-syntax-target resolved-type-syntax))))
	(t (error "Unknown resolved type syntax ~S" resolved-type-syntax))))

(defun parse-parameter (function parameter-syntax)
  "Create a parameter entity from one (name type) syntax form."
  (unless (termis-list-p (syntax-datum parameter-syntax))
    (error 'semantic-error :syntax parameter-syntax
			   :message "function parameter must be a (name type) list"))
  (let ((elements (termis-list-elements (syntax-datum parameter-syntax))))
    (unless (= (length elements) 2)
      (error 'semantic-error :syntax parameter-syntax
			     :message "function parameter must contain a name and type"))
    (let ((name (syntax-datum (first elements))))
      (unless (termis-name-p name)
	(error 'semantic-error :syntax (first elements)
			       :message "function parameter name must be a Termis name"))
      (make-instance 'parameter-binding
		     :name name :syntax (first elements) :type-syntax (second elements)))))

(defun resolve-function-signature (program semantic-declaration)
  (let* ((declaration (semantic-declaration-source-declaration semantic-declaration))
	 (module-scope (semantic-program-module-scope program))
	 (parameters-syntax (function-declaration-parameters declaration)))
    (unless (termis-list-p (syntax-datum parameters-syntax))
      (error 'semantic-error :syntax parameters-syntax
			     :message "function parameters must be a list"))
    (let ((scope (semantic-scope-child module-scope))
	  (parameters
	    (mapcar (lambda (syntax) (parse-parameter declaration syntax))
		    (termis-list-elements (syntax-datum parameters-syntax)))))
      ;; Parameter types are interface names, and so resolve in the module
      ;; scope.  Binding parameters afterward avoids accidental access to a
      ;; preceding parameter while interpreting this still-untyped syntax.
      (dolist (parameter parameters)
	(setf (parameter-binding-type-reference parameter)
	      (resolve-type-syntax module-scope
				   (parameter-binding-type-syntax parameter))))
      (dolist (parameter parameters)
	(multiple-value-bind (existing foundp)
	    (semantic-scope-local-find scope (semantic-binding-name parameter))
	  (when foundp
	    (error 'duplicate-local-binding-error
		   :syntax (parameter-binding-syntax parameter)
		   :name (semantic-binding-name parameter) :existing existing))
	  (semantic-scope-bind scope (semantic-binding-name parameter) parameter)))
      (setf (semantic-function-declaration-scope semantic-declaration) scope
	    (semantic-function-declaration-parameters semantic-declaration) parameters
	    (semantic-function-declaration-return-type-reference semantic-declaration)
	    (resolve-type-syntax module-scope
				 (function-declaration-return-type declaration))))))

(defun make-semantic-declaration (declaration)
  (cond ((typep declaration 'type-declaration)
	 (make-instance 'semantic-type-declaration :source-declaration declaration))
	((typep declaration 'constant-declaration)
	 (make-instance 'semantic-constant-declaration :source-declaration declaration))
	((typep declaration 'variable-declaration)
	 (make-instance 'semantic-variable-declaration :source-declaration declaration))
	((typep declaration 'function-declaration)
	 (make-instance 'semantic-function-declaration :source-declaration declaration))
	;; Macro declarations have already been handled by the evaluator.
	((typep declaration 'macro-declaration) nil)
	(t (error "Unknown Termis declaration ~S" declaration))))

(defun resolve-declaration-signature (program semantic-declaration)
  (let ((declaration (semantic-declaration-source-declaration semantic-declaration))
	(scope (semantic-program-module-scope program)))
    (cond ((typep semantic-declaration 'semantic-function-declaration)
	   (resolve-function-signature program semantic-declaration))
	  ((typep semantic-declaration 'semantic-constant-declaration)
	   (setf (semantic-constant-declaration-type-reference semantic-declaration)
		 (resolve-type-syntax scope (constant-declaration-type declaration))))
	  ((typep semantic-declaration 'semantic-variable-declaration)
	   (setf (semantic-variable-declaration-type-reference semantic-declaration)
		 (resolve-type-syntax scope (variable-declaration-type declaration)))))))

(defun create-defined-type-identities (program)
  "Allocate every nominal type before resolving any use of one.

Doing this as a separate stage makes forward and recursive references refer
to a stable declaration identity rather than attempting to expand a type
definition eagerly."
  (let ((context (semantic-program-type-context program)))
    (dolist (entry (semantic-program-declarations program))
      (let ((semantic-declaration (cdr entry)))
	(when (typep semantic-declaration 'semantic-type-declaration)
	  (setf (semantic-type-declaration-type semantic-declaration)
		(type-context-defined-type
		 context
		 (semantic-declaration-source-declaration semantic-declaration))))))))

(defun resolve-declaration-types (program semantic-declaration)
  "Attach canonical types to the already name-resolved declaration interface."
  (let ((context (semantic-program-type-context program)))
    (cond
      ((typep semantic-declaration 'semantic-function-declaration)
       (let ((parameter-types
	       (mapcar (lambda (parameter)
			 (setf (parameter-binding-type parameter)
			       (resolve-type context
					     (parameter-binding-type-reference parameter))))
		       (semantic-function-declaration-parameters semantic-declaration))))
	 (setf (semantic-function-declaration-return-type semantic-declaration)
	       (resolve-type context
			     (semantic-function-declaration-return-type-reference
			      semantic-declaration))
	       (semantic-function-declaration-type semantic-declaration)
	       (type-context-function-type
		context parameter-types
		(semantic-function-declaration-return-type semantic-declaration)))))
      ((typep semantic-declaration 'semantic-constant-declaration)
       (setf (semantic-constant-declaration-type semantic-declaration)
	     (resolve-type context
			   (semantic-constant-declaration-type-reference
			    semantic-declaration))))
      ((typep semantic-declaration 'semantic-variable-declaration)
       (setf (semantic-variable-declaration-type semantic-declaration)
	     (resolve-type context
			   (semantic-variable-declaration-type-reference
			    semantic-declaration)))))))

(defun resolve-types (program)
  "Run the type representation/resolution stage for PROGRAM.

Expression bodies are intentionally untouched: this pass establishes only
declaration signatures and nominal type identities for the later expression
type checker."
  (check-type program semantic-program)
  (create-defined-type-identities program)
  (dolist (entry (semantic-program-declarations program))
    (resolve-declaration-types program (cdr entry)))
  program)

;;; Expression analysis ----------------------------------------------------

(define-condition type-mismatch-error (semantic-error)
  ((actual :initarg :actual :reader type-mismatch-error-actual)
   (expected :initarg :expected :reader type-mismatch-error-expected))
  (:default-initargs :message "type mismatch")
  (:report (lambda (condition stream)
	     (let ((syntax (semantic-error-syntax condition)))
	       (when syntax
		 (let ((location (syntax-start syntax)))
		   (format stream "~A:~D:~D: "
			   (source-name (syntax-source syntax))
			   (source-location-line location)
			   (source-location-column location))))
	       (format stream "type mismatch~%expected: ~A~%actual:   ~A"
		       (termis-type-name (type-mismatch-error-expected condition))
		       (termis-type-name (type-mismatch-error-actual condition)))))))

(define-condition semantic-not-callable-error (semantic-error)
  ((actual :initarg :actual :reader semantic-not-callable-error-actual))
  (:default-initargs :message "expression is not callable"))

(define-condition wrong-argument-count-error (semantic-error)
  ((expected :initarg :expected :reader wrong-argument-count-error-expected)
   (actual :initarg :actual :reader wrong-argument-count-error-actual))
  (:default-initargs :message "wrong argument count"))

(define-condition not-addressable-error (semantic-error) ())
(define-condition not-writable-error (semantic-error) ())
(define-condition invalid-expression-error (semantic-error) ())

(defun termis-type-name (type)
  "A compact stable spelling used in semantic diagnostics."
  (cond ((typep type 'unit-type) "unit")
	((typep type 'boolean-type) "bool")
	((typep type 'string-type) "string")
	((typep type 'integer-type)
	 (format nil "~:[u~;i~]~D" (integer-type-signed type)
		 (integer-type-width type)))
	((typep type 'float-type) (format nil "f~D" (float-type-width type)))
	((typep type 'pointer-type)
	 (format nil "(pointer ~A)" (termis-type-name (pointer-type-target type))))
	((typep type 'function-type) "function")
	((typep type 'defined-type)
	 (termis-name-value (declaration-name (defined-type-declaration type))))
	(t "<unknown type>")))

(defun same-type-p (left right)
  "Whether LEFT and RIGHT are the same canonical Termis type."
  (eq left right))

(defun compatible-p (actual expected)
  "Current non-literal compatibility rule: types must be identical."
  (same-type-p actual expected))

(defun expression-special-form-name (syntax)
  (let ((datum (syntax-datum syntax)))
    (when (termis-list-p datum)
      (let ((head (first (termis-list-elements datum))))
	(when (and head (termis-name-p (syntax-datum head)))
	  (termis-name-value (syntax-datum head)))))))

(defun binding-expression-type (scope binding syntax)
  "Return BINDING's runtime type without changing its identity."
  (cond ((typep binding 'parameter-binding) (parameter-binding-type binding))
	((typep binding 'builtin-intrinsic-binding)
	 (builtin-intrinsic-binding-type binding))
	((or (typep binding 'builtin-type-binding)
	     (typep binding 'type-declaration))
	 (error 'invalid-expression-error :syntax syntax
					  :message "a type is not a runtime value"))
	((typep binding 'declaration)
	 (let* ((program (semantic-scope-owning-program scope))
		(semantic (and program (semantic-program-declaration program binding))))
	   (cond ((typep semantic 'semantic-constant-declaration)
		  (semantic-constant-declaration-type semantic))
		 ((typep semantic 'semantic-variable-declaration)
		  (semantic-variable-declaration-type semantic))
		 ((typep semantic 'semantic-function-declaration)
		  (semantic-function-declaration-type semantic))
		 (t (error 'invalid-expression-error :syntax syntax
						     :message "declaration has no runtime type")))))
	(t (error 'invalid-expression-error :syntax syntax
					    :message "unknown runtime binding"))))

(defun binding-place-properties (binding)
  "Return addressable and writable flags for a reference binding.

Parameters are deliberately addressable and writable in this initial model;
they represent parameter storage rather than C's accidental value category."
  (cond ((typep binding 'parameter-binding) (values t t))
	((typep binding 'variable-declaration) (values t t))
	(t (values nil nil))))

(defun infer-reference-expression (syntax scope)
  (let* ((untyped (resolve-name scope syntax))
	 (binding (semantic-reference-binding untyped)))
    (multiple-value-bind (addressable writable) (binding-place-properties binding)
      (make-instance 'semantic-reference :syntax syntax
					 :name (semantic-reference-name untyped) :binding binding
					 :type (binding-expression-type scope binding syntax)
					 :addressable addressable :writable writable))))

(defun infer-call-expression (syntax scope)
  (let* ((elements (termis-list-elements (syntax-datum syntax)))
	 (callee (infer-expression (first elements) scope))
	 (callee-type (expression-type callee)))
    (unless (typep callee-type 'function-type)
      (error 'semantic-not-callable-error :syntax (first elements)
					  :actual callee-type))
    (let ((parameter-types (function-type-parameters callee-type))
	  (argument-syntax (rest elements)))
      (unless (= (length argument-syntax) (length parameter-types))
	(error 'wrong-argument-count-error :syntax syntax
					   :expected (length parameter-types) :actual (length argument-syntax)))
      (make-instance 'semantic-call :syntax syntax :callee callee
				    :arguments (loop for argument in argument-syntax
						     for parameter-type in parameter-types
						     collect (check-expression argument scope parameter-type))
				    :type (function-type-result callee-type)))))

(defun infer-sequence-expression (syntax scope)
  (let ((expressions (mapcar (lambda (form) (infer-expression form scope))
			     (rest (termis-list-elements (syntax-datum syntax))))))
    (make-instance 'sequence-expression :syntax syntax :expressions expressions
					:type (if expressions
						  (expression-type (car (last expressions)))
						  (type-context-unit-type
						   (semantic-scope-owning-type-context scope))))))

(defun infer-address-expression (syntax scope)
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (unless (= (length arguments) 1)
      (error 'invalid-expression-error :syntax syntax
				       :message "& requires exactly one operand"))
    (let ((operand (infer-expression (first arguments) scope)))
      (unless (and (typep operand 'place-expression)
		   (place-expression-addressable-p operand))
	(error 'not-addressable-error :syntax (first arguments)
				      :message "expression is not addressable"))
      (make-instance 'address-expression :syntax syntax :operand operand
					 :type (type-context-pointer-type
						(semantic-scope-owning-type-context scope)
						(expression-type operand))))))

(defun infer-dereference-expression (syntax scope)
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (unless (= (length arguments) 1)
      (error 'invalid-expression-error :syntax syntax
				       :message "deref requires exactly one operand"))
    (let* ((operand (infer-expression (first arguments) scope))
	   (operand-type (expression-type operand)))
      (unless (typep operand-type 'pointer-type)
	(error 'invalid-expression-error :syntax (first arguments)
					 :message "dereference requires a pointer"))
      (make-instance 'dereference-expression :syntax syntax :operand operand
					     :type (pointer-type-target operand-type)
					     :addressable t :writable t))))

(defun infer-assignment-expression (syntax scope)
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (unless (= (length arguments) 2)
      (error 'invalid-expression-error :syntax syntax
				       :message "assign requires a target and a value"))
    (let ((target (infer-expression (first arguments) scope)))
      (unless (and (typep target 'place-expression)
		   (place-expression-writable-p target))
	(error 'not-writable-error :syntax (first arguments)
				   :message "expression is not writable"))
      (make-instance 'assignment-expression :syntax syntax :target target
					    :value (check-expression (second arguments) scope
								     (expression-type target))
					    :type (type-context-unit-type
						   (semantic-scope-owning-type-context scope))))))

(defun infer-expression (syntax scope)
  "Analyze SYNTAX in SCOPE and return a fully typed semantic expression."
  (check-type syntax syntax)
  (check-type scope semantic-scope)
  (let ((datum (syntax-datum syntax))
	(context (semantic-scope-owning-type-context scope)))
    (cond ((unit-literal-p datum)
	   (make-instance 'unit-expression :syntax syntax
					   :type (type-context-unit-type context)))
	  ((termis-boolean-literal-p datum)
	   (make-instance 'boolean-literal :syntax syntax :value (termis-boolean-literal-value datum)
					   :type (type-context-boolean-type context)))
	  ((integerp datum)
	   (make-instance 'integer-literal :syntax syntax :value datum
					   :type (type-context-integer-type context t 32)))
	  ((floatp datum)
	   (make-instance 'float-literal :syntax syntax :value datum
					 :type (type-context-float-type context 64)))
	  ((stringp datum)
	   (make-instance 'string-literal :syntax syntax :value datum
					  :type (type-context-string-type context)))
	  ((termis-name-p datum) (infer-reference-expression syntax scope))
	  ((termis-list-p datum)
	   (let ((elements (termis-list-elements datum)))
	     (unless elements
	       (error 'invalid-expression-error :syntax syntax
						:message "an empty list is not an expression"))
	     (let ((special (expression-special-form-name syntax)))
	       (cond ((and special (string= special "do"))
		      (infer-sequence-expression syntax scope))
		     ((and special (string= special "assign"))
		      (infer-assignment-expression syntax scope))
		     ((and special (string= special "&"))
		      (infer-address-expression syntax scope))
		     ((and special (string= special "deref"))
		      (infer-dereference-expression syntax scope))
		     (t (infer-call-expression syntax scope))))))
	  (t (error 'invalid-expression-error :syntax syntax
					      :message "unsupported expression")))))

(defun check-expression (syntax scope expected-type)
  "Analyze SYNTAX with EXPECTED-TYPE, contextually typing numeric literals."
  (check-type expected-type termis-type)
  (let ((datum (syntax-datum syntax)))
    (cond ((and (integerp datum) (typep expected-type 'integer-type))
	   (make-instance 'integer-literal :syntax syntax :value datum :type expected-type))
	  ((and (floatp datum) (typep expected-type 'float-type))
	   (make-instance 'float-literal :syntax syntax :value datum :type expected-type))
	  (t (let ((expression (infer-expression syntax scope)))
	       (unless (compatible-p (expression-type expression) expected-type)
		 (error 'type-mismatch-error :syntax syntax
					     :actual (expression-type expression) :expected expected-type))
	       expression)))))

(defun resolve-declaration-body (program semantic-declaration)
  (let ((declaration (semantic-declaration-source-declaration semantic-declaration)))
    (cond ((typep semantic-declaration 'semantic-function-declaration)
	   (setf (semantic-function-declaration-body semantic-declaration)
		 (check-expression
		  (function-declaration-body declaration)
		  (semantic-function-declaration-scope semantic-declaration)
		  (semantic-function-declaration-return-type semantic-declaration))))
	  ((typep semantic-declaration 'semantic-constant-declaration)
	   (setf (semantic-constant-declaration-initializer semantic-declaration)
		 (check-expression
		  (constant-declaration-value declaration)
		  (semantic-program-module-scope program)
		  (semantic-constant-declaration-type semantic-declaration))))
	  ((typep semantic-declaration 'semantic-variable-declaration)
	   (setf (semantic-variable-declaration-initializer semantic-declaration)
		 (check-expression
		  (variable-declaration-initializer declaration)
		  (semantic-program-module-scope program)
		  (semantic-variable-declaration-type semantic-declaration)))))))

(defun resolve-compilation-unit (unit)
  "Resolve UNIT after all declarations have been collected.

The two passes are intentional: the first registers every runtime/type
declaration and resolves interfaces; the second resolves executable bodies.
Thus ordinary declaration order has no effect on name visibility."
  (check-type unit compilation-unit)
  (let* ((type-context (make-type-context))
	 (bootstrap (make-bootstrap-semantic-scope type-context))
	 (module-scope (semantic-scope-child bootstrap))
	 (program (make-instance 'semantic-program :bootstrap-scope bootstrap
						   :module-scope module-scope
						   :type-context type-context)))
    (setf (semantic-scope-program bootstrap) program)
    (dolist (declaration (unit-declarations unit))
      (unless (typep declaration 'macro-declaration)
	(semantic-scope-bind module-scope (declaration-name declaration) declaration)
	(let ((semantic-declaration (make-semantic-declaration declaration)))
	  (push (cons declaration semantic-declaration)
		(semantic-program-declarations program)))))
    (setf (semantic-program-declarations program)
	  (nreverse (semantic-program-declarations program)))
    (dolist (entry (semantic-program-declarations program))
      (resolve-declaration-signature program (cdr entry)))
    (resolve-types program)
    (dolist (entry (semantic-program-declarations program))
      (resolve-declaration-body program (cdr entry)))
    (setf (compilation-unit-semantic-program unit) program)
    program))
