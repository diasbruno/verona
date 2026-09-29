(in-package #:termis)

;;; This file is the syntax-to-semantics boundary.  It first records binding
;;; identity, then turns executable syntax into typed runtime expressions.

(defclass builtin-binding (semantic-binding) ())
(defclass builtin-type-binding (builtin-binding)
  ((type :initarg :type :reader builtin-type-binding-type)))
;; Builtins are declarations/entities in the semantic namespace, but unlike
;; source TYPE-DECLARATIONs they have no source form to retain.
(defclass builtin-type-declaration (builtin-type-binding) ())

;;; Types are compiler semantics, deliberately independent from any lowering
;;; target.  In particular, BOOLEAN-TYPE does not imply an LLVM integer type.
(defclass termis-type () ())

(defclass unit-type (termis-type) ())
(defclass never-type (termis-type) ())
;; UNIT-VALUE is deliberately an object rather than the host value NIL.  A
;; type context allocates exactly one of these objects, making the singleton
;; nature of Termis unit visible to later compiler stages without conflating it
;; with void or an integer zero.
(defclass unit-value () ())
(defclass boolean-type (termis-type) ())
(defclass string-type (termis-type) ())
(defclass integer-type (termis-type)
  ((signed :initarg :signed :reader integer-type-signed)
   (width :initarg :width :reader integer-type-width)))
(defclass float-type (termis-type)
  ((width :initarg :width :reader float-type-width)))
(defclass pointer-type (termis-type)
  ((target :initarg :target :reader pointer-type-target :reader pointer-type-pointee)))
(defclass function-type (termis-type)
  ((parameters :initarg :parameters :reader function-type-parameters)
   (result :initarg :result :reader function-type-result)))
(defclass defined-type (termis-type)
  ((declaration :initarg :declaration :reader defined-type-declaration)))

;; A product retains its declaration identity through DEFINED-TYPE while also
;; carrying the complete, ordered value layout needed by later stages.
(defclass product-type (defined-type)
  ((fields :initarg :fields :reader product-type-fields)))

;; Sums, like products, are nominal through their TypeDeclaration.  Their
;; alternatives are semantic entities; a backend never has to recover them
;; from source spellings.
(defclass sum-type (defined-type)
  ((alternatives :initarg :alternatives :reader sum-type-alternatives)))

(defclass sum-alternative ()
  ((sum-type :initarg :sum-type :reader sum-alternative-sum-type)
   (name :initarg :name :reader sum-alternative-name)
   (index :initarg :index :reader sum-alternative-index)
   (payload-types :initarg :payload-types :reader sum-alternative-payload-types)
   (source :initarg :source :reader sum-alternative-source)))

(defclass product-field ()
  ((name :initarg :name :reader product-field-name)
   (type :initarg :type :reader product-field-type)
   (index :initarg :index :reader product-field-index)
   (source :initarg :source :reader product-field-source)))

(defclass type-context ()
  ((unit-type :reader type-context-unit-type)
   (never-type :reader type-context-never-type)
   (unit-value :reader type-context-unit-value)
   (pointer-width :initarg :pointer-width :reader type-context-pointer-width)
   (boolean-type :reader type-context-boolean-type)
   (string-type :reader type-context-string-type)
   (integer-types :initform '() :accessor type-context-integer-types)
   (float-types :initform '() :accessor type-context-float-types)
   (pointer-types :initform '() :accessor type-context-pointer-types)
   (function-types :initform '() :accessor type-context-function-types)
   ;; This association uses declaration object identity, never its spelling.
   (defined-types :initform '() :accessor type-context-defined-types)))

(defun make-type-context (&key (pointer-width 64))
  "Create the canonical Termis types for one semantic program."
  (unless (member pointer-width '(32 64))
    (error "Termis currently supports 32-bit and 64-bit pointer targets, not ~S"
           pointer-width))
  (let ((context (make-instance 'type-context)))
    (setf (slot-value context 'unit-type) (make-instance 'unit-type)
	  (slot-value context 'never-type) (make-instance 'never-type)
	  (slot-value context 'unit-value) (make-instance 'unit-value)
	  (slot-value context 'pointer-width) pointer-width
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

(defun type-context-unit-representation-type (context)
  "The target-dependent machine representation of semantic UnitType.

This is intentionally an IntegerType only at the representation boundary;
the semantic type of a unit expression remains UnitType."
  (check-type context type-context)
  (type-context-integer-type context nil (type-context-pointer-width context)))

(defun unit-machine-representation (context value)
  "Return the canonical machine representation for VALUE, the sole UnitValue."
  (check-type context type-context)
  (unless (eq value (type-context-unit-value context))
    (error "not this type context's UnitValue"))
  0)

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

(defun type-context-product-type (context declaration fields)
  "Install DECLARATION's complete nominal product type exactly once."
  (or (cdr (assoc declaration (type-context-defined-types context) :test #'eq))
      (let ((type (make-instance 'product-type :declaration declaration :fields fields)))
	(push (cons declaration type) (type-context-defined-types context))
	type)))

(defun type-context-sum-type (context declaration alternatives)
  "Install DECLARATION's complete nominal sum type exactly once."
  (or (cdr (assoc declaration (type-context-defined-types context) :test #'eq))
      (let ((type (make-instance 'sum-type :declaration declaration
                                 :alternatives alternatives)))
	(push (cons declaration type) (type-context-defined-types context))
	type)))

;;; Concrete primitive operations -----------------------------------------

;; A primitive operation is a Termis semantic entity.  Its KIND is the
;; complete operation selection the LLVM backend will later translate; it is
;; never reconstructed from NAME or from operand types.
(defclass primitive-operation ()
  ((identity :initarg :identity :initform (gensym "PRIMITIVE-")
	     :reader primitive-operation-identity)
   (name :initarg :name :reader primitive-operation-name)
   (parameter-types :initarg :parameter-types
		    :reader primitive-operation-parameter-types)
   (result-type :initarg :result-type :reader primitive-operation-result-type)
   (kind :initarg :kind :reader primitive-operation-kind)
   ;; Floating comparisons are explicitly ordered: any NaN operand yields
   ;; false, including for equality.  This is semantic policy, not an LLVM
   ;; default the backend is allowed to choose.
   (nan-semantics :initarg :nan-semantics :initform nil
		  :reader primitive-operation-nan-semantics)))

(defclass primitive-binding (builtin-binding)
  ((operation :initarg :operation :reader primitive-binding-operation)
   (context :initarg :context :reader primitive-binding-context)))

;; Keep the previous public class operational for clients from the preceding
;; milestone.  It is now a primitive binding, so even legacy '+' resolves to
;; an operation identity rather than a backend-facing textual convention.
(defclass builtin-intrinsic-binding (primitive-binding) ())

(defun builtin-intrinsic-binding-type (binding)
  (type-context-function-type
   (primitive-operation-context binding)
   (primitive-operation-parameter-types (primitive-binding-operation binding))
   (primitive-operation-result-type (primitive-binding-operation binding))))

;; The operation already owns canonical type objects.  Recover their context
;; through a compact association on the binding so compatibility callers can
;; still ask for its FunctionType.
(defgeneric primitive-operation-context (binding))
(defmethod primitive-operation-context ((binding primitive-binding))
  (primitive-binding-context binding))

;;; Compile-time generic dispatch -----------------------------------------

(defclass generic ()
  ((declaration :initarg :declaration :initform nil :reader generic-declaration)
   (name :initarg :name :reader generic-name)
   (arity :initarg :arity :reader generic-arity)
   ;; Keys are lists of canonical TERMIS-TYPE objects.  EQUAL is intentional:
   ;; standard objects compare by identity, never by their printed spelling.
   (implementations :initform '() :accessor generic-implementations)))

(defclass generic-binding (semantic-binding)
  ((generic :initarg :generic :reader generic-binding-generic)))

(defclass generic-implementation ()
  ((declaration :initarg :declaration :initform nil
                :reader generic-implementation-declaration)
   (generic :initarg :generic :reader generic-implementation-generic)
   (parameters :initarg :parameters :initform '()
               :accessor generic-implementation-parameters)
   (parameter-types :initarg :parameter-types :initform '()
                    :accessor generic-implementation-parameter-types)
   (result-type :initarg :result-type :initform nil
                :accessor generic-implementation-result-type)
   (body :initarg :body :initform nil :accessor generic-implementation-body)
   (primitive-operation :initarg :primitive-operation :initform nil
                        :reader generic-implementation-primitive-operation)
   (source :initarg :source :initform nil :reader generic-implementation-source)))

(defun generic-find-implementation (generic parameter-types)
  (cdr (assoc parameter-types (generic-implementations generic) :test #'equal)))

(defun generic-add-implementation (generic implementation &optional syntax)
  (let ((key (generic-implementation-parameter-types implementation)))
    (when (generic-find-implementation generic key)
      (error 'duplicate-generic-implementation-error :syntax syntax
             :generic generic :parameter-types key
             :original (generic-find-implementation generic key)
             :duplicate implementation
             :original-source (generic-implementation-source
                               (generic-find-implementation generic key))
             :duplicate-source syntax))
    (push (cons key implementation) (generic-implementations generic))
    implementation))

(defun make-primitive-binding (context name parameter-types result-type kind
				     &key nan-semantics class)
  (let* ((operation (make-instance 'primitive-operation
					  :name (make-termis-name name)
					  :parameter-types parameter-types
					  :result-type result-type :kind kind
					  :nan-semantics nan-semantics))
	 (binding (make-instance (or class 'primitive-binding)
				 :name (primitive-operation-name operation)
				 :operation operation :context context)))
    ;; CONTEXT is not semantic operation state; it only preserves the legacy
    ;; builtin-intrinsic-binding-type accessor.
    binding))

;;; These nodes retain the resolved structure of compound type syntax until
;;; the type pass turns them into TERMIS-TYPE objects.  A bare type name stays
;;; a SEMANTIC-REFERENCE, preserving the established representation and API.
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

(defclass pattern-binding (semantic-binding)
  ((syntax :initarg :syntax :reader pattern-binding-syntax)
   (type :initarg :type :reader pattern-binding-type)))

;; A LET-BINDING is a semantic identity, not a request for storage.  Its
;; initializer is resolved before the binding enters its lexical scope.
(defclass let-binding (semantic-binding)
  ((syntax :initarg :syntax :reader let-binding-syntax :reader let-binding-source)
   (type-syntax :initarg :type-syntax :reader let-binding-type-syntax)
   (type-reference :initarg :type-reference :reader let-binding-type-reference)
   (type :initarg :type :reader let-binding-type)
   (initializer :initarg :initializer :reader let-binding-initializer)))

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
  ((type :initform nil :accessor semantic-type-declaration-type)
   (fields :initform '() :accessor semantic-type-declaration-fields)
   (alternatives :initform '() :accessor semantic-type-declaration-alternatives)))

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

(defclass semantic-generic-declaration (semantic-declaration)
  ((generic :initarg :generic :reader semantic-generic-declaration-generic)))

;; A source implementation is both a declaration retained by the program and
;; the selected concrete callable.  Primitive-backed implementations use the
;; base GENERIC-IMPLEMENTATION class instead.
(defclass semantic-generic-implementation (generic-implementation semantic-declaration semantic-binding)
  ((scope :initform nil :accessor semantic-generic-implementation-scope)
   (return-type-reference :initform nil
                          :accessor semantic-generic-implementation-return-type-reference)
   (type :initform nil :accessor semantic-generic-implementation-type)))

;; EXPRESSION is the typed runtime semantic model.  The SEMANTIC-* classes
;; remain concrete compatibility names for clients of the earlier passes.
(defclass expression ()
  ((syntax :initarg :syntax :reader expression-syntax :reader expression-source
	   :reader semantic-expression-syntax)
   (type :initarg :type :initform nil
	 :reader expression-type :reader semantic-expression-type)))
(defclass semantic-expression (expression) ())

(defclass semantic-literal (semantic-expression) ())
(defclass unit-expression (semantic-literal)
  ((value :initarg :value :reader unit-expression-value)))
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
(defclass primitive-call (semantic-call)
  ((operation :initarg :operation :reader primitive-call-operation)))
;; Conversion calls are a distinct semantic class so a backend can lower
;; them mechanically without inspecting primitive names or argument types.
(defclass conversion-expression (primitive-call) ())
(defclass construct-expression (semantic-expression)
  ((product-type :initarg :product-type :reader construct-expression-product-type)
   (fields :initarg :fields :reader construct-expression-fields)))
(defclass sum-construct-expression (semantic-expression)
  ((alternative :initarg :alternative :reader sum-construct-expression-alternative)
   (arguments :initarg :arguments :reader sum-construct-expression-arguments)))
(defclass field-expression (semantic-expression)
  ((value :initarg :value :reader field-expression-value)
   (field :initarg :field :reader field-expression-field)))
(defclass sequence-expression (semantic-expression)
  ((expressions :initarg :expressions :reader sequence-expression-expressions)))
(defclass let-expression (semantic-expression)
  ((bindings :initarg :bindings :reader let-expression-bindings)
   (scope :initarg :scope :reader let-expression-scope)
   (body :initarg :body :reader let-expression-body)))
(defclass address-expression (semantic-expression)
  ((operand :initarg :operand :reader address-expression-operand)))
(defclass dereference-expression (semantic-expression place-expression)
  ((operand :initarg :operand :reader dereference-expression-operand)))
(defclass load-expression (semantic-expression)
  ((place :initarg :place :reader load-expression-place)))
(defclass assignment-expression (semantic-expression)
  ((target :initarg :target :reader assignment-expression-target)
   (value :initarg :value :reader assignment-expression-value)))
(defclass store-expression (assignment-expression) ())
(defclass return-expression (semantic-expression)
  ((value :initarg :value :reader return-expression-value)))

;;; The source representation is resolved before reaching the backend.
(defclass pattern ()
  ((syntax :initarg :syntax :reader pattern-syntax)
   (type :initarg :type :reader pattern-type)))
(defclass literal-pattern (pattern)
  ((value :initarg :value :reader literal-pattern-value)))
(defclass boolean-pattern (literal-pattern) ())
(defclass integer-pattern (literal-pattern) ())
(defclass wildcard-pattern (pattern) ())
(defclass binding-pattern (pattern)
  ((binding :initarg :binding :reader binding-pattern-binding)))
(defclass constructor-pattern (pattern)
  ((alternative :initarg :alternative :reader constructor-pattern-alternative)
   (payload-patterns :initarg :payload-patterns
                     :reader constructor-pattern-payload-patterns)))
(defclass match-case ()
  ((syntax :initarg :syntax :reader match-case-syntax)
   (pattern :initarg :pattern :reader match-case-pattern)
   (scope :initarg :scope :reader match-case-scope)
   (expression :initarg :expression :reader match-case-expression)))
(defclass match-expression (semantic-expression)
  ((value :initarg :value :reader match-expression-value)
   (cases :initarg :cases :reader match-expression-cases)))

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
    (labels ((bind (name parameters result kind &key nan-semantics class)
	       (let ((binding (make-primitive-binding type-context name parameters result kind
							 :nan-semantics nan-semantics :class class)))
		 (semantic-scope-bind scope (make-termis-name name) binding)))
	     (integer-name (type)
	       (format nil "~:[u~;i~]~D" (integer-type-signed type)
		       (integer-type-width type)))
	     (float-name (type) (format nil "f~D" (float-type-width type))))
      ;; Arithmetic retains signedness in the operation's concrete identity,
      ;; including the cases where LLVM eventually uses the same instruction.
      (dolist (signed '(t nil))
	(dolist (width '(8 16 32 64))
	  (let* ((type (type-context-integer-type type-context signed width))
		 (suffix (integer-name type)))
	    (dolist (spec '(("%+" :integer-add) ("%-" :integer-subtract)
			    ("%*" :integer-multiply) ("%/" :integer-divide)))
	      (bind (format nil "~A-primitive-~A" (first spec) suffix)
		    (list type type) type
		    (if (eq (second spec) :integer-divide)
			(if signed :signed-integer-divide :unsigned-integer-divide)
			(second spec))))
	    (dolist (spec '(("%=" :integer-equal) ("%/=" :integer-not-equal)
			    ("%<" :integer-less-than) ("%<=" :integer-less-than-or-equal)
			    ("%>" :integer-greater-than) ("%>=" :integer-greater-than-or-equal)))
	      (bind (format nil "~A-primitive-~A" (first spec) suffix)
		    (list type type) (type-context-boolean-type type-context)
		    (intern (format nil "~A-~A" (if signed "SIGNED" "UNSIGNED")
				    (symbol-name (second spec))) :keyword))))))
      (dolist (width '(32 64))
	(let* ((type (type-context-float-type type-context width))
	       (suffix (float-name type)))
	  (dolist (spec '(("%+" :float-add) ("%-" :float-subtract)
			  ("%*" :float-multiply) ("%/" :float-divide)))
	    (bind (format nil "~A-primitive-~A" (first spec) suffix)
		  (list type type) type (second spec)))
	  (dolist (spec '(("%=" :float-ordered-equal) ("%/=" :float-ordered-not-equal)
			  ("%<" :float-ordered-less-than)
			  ("%<=" :float-ordered-less-than-or-equal)
			  ("%>" :float-ordered-greater-than)
			  ("%>=" :float-ordered-greater-than-or-equal)))
	    (bind (format nil "~A-primitive-~A" (first spec) suffix)
		  (list type type) (type-context-boolean-type type-context) (second spec)
		  :nan-semantics :ordered-false))))
      (let ((bool (type-context-boolean-type type-context)))
	(bind "%not-primitive-bool" (list bool) bool :boolean-not)
	(bind "%and-primitive-bool" (list bool bool) bool :boolean-and)
	(bind "%or-primitive-bool" (list bool bool) bool :boolean-or)
	(bind "%=-primitive-bool" (list bool bool) bool :boolean-equal)
	(bind "%/=-primitive-bool" (list bool bool) bool :boolean-not-equal))
      ;; Every conversion is a concrete operation.  The source spelling is
      ;; deliberately descriptive, so there is no generic conversion rule for
      ;; a backend to recover or invent.
      (dolist (source-signed '(t nil))
	(dolist (source-width '(8 16 32 64))
	  (let ((source (type-context-integer-type type-context source-signed source-width)))
	    (dolist (destination-signed '(t nil))
	      (dolist (destination-width '(8 16 32 64))
		(let ((destination (type-context-integer-type type-context destination-signed destination-width)))
		  (cond ((< source-width destination-width)
			 (bind (format nil "%~A-primitive-~A-~A"
				       (if source-signed "sext" "zext")
				       (integer-name source) (integer-name destination))
			       (list source) destination
			       (if source-signed :integer-sign-extend :integer-zero-extend)))
			((> source-width destination-width)
			 (bind (format nil "%trunc-primitive-~A-~A"
				       (integer-name source) (integer-name destination))
			       (list source) destination :integer-truncate))))))
	    (dolist (float-width '(32 64))
	      (let ((float (type-context-float-type type-context float-width)))
		(bind (format nil "%~A-primitive-~A-~A"
			      (if source-signed "sitofp" "uitofp")
			      (integer-name source) (float-name float))
		      (list source) float
		      (if source-signed :signed-integer-to-float :unsigned-integer-to-float)))))))
      (dolist (source-width '(32 64))
	(let ((source (type-context-float-type type-context source-width)))
	  (dolist (destination-signed '(t nil))
	    (dolist (destination-width '(8 16 32 64))
	      (let ((destination (type-context-integer-type type-context destination-signed destination-width)))
		(bind (format nil "%~A-primitive-~A-~A"
			      (if destination-signed "fptosi" "fptoui")
			      (float-name source) (integer-name destination))
		      (list source) destination
		      (if destination-signed :float-to-signed-integer :float-to-unsigned-integer)))))
	  (dolist (destination-width '(32 64))
	    (let ((destination (type-context-float-type type-context destination-width)))
	      (cond ((< source-width destination-width)
		     (bind (format nil "%fext-primitive-~A-~A" (float-name source) (float-name destination))
			   (list source) destination :float-extend))
		    ((> source-width destination-width)
		     (bind (format nil "%ftrunc-primitive-~A-~A" (float-name source) (float-name destination))
			   (list source) destination :float-truncate)))))))
      ;; Transitional aliases preserve the established surface spelling while
      ;; resolving to concrete i32 operations, never generic dispatch.
      (let ((i32 (type-context-integer-type type-context t 32)))
	(dolist (spec '(("+" :integer-add) ("-" :integer-subtract)
			("*" :integer-multiply) ("/" :integer-divide)))
	  (bind (first spec) (list i32 i32) i32 (second spec)
		:class 'builtin-intrinsic-binding)))
      ;; Surface arithmetic and comparison are compile-time generics.  The
      ;; older concrete i32 aliases above remain available as primitives for
      ;; bootstrap code and backwards compatibility.
      (labels ((install (name arity)
                 (let ((generic (make-instance 'generic :name (make-termis-name name)
                                                 :arity arity)))
                   (semantic-scope-bind scope (generic-name generic)
                                        (make-instance 'generic-binding
                                                       :name (generic-name generic)
                                                       :generic generic))
                   generic))
               (primitive (name)
                 (primitive-binding-operation
                  (semantic-scope-lookup scope (make-termis-name name))))
               (add (generic name)
                 (let ((operation (primitive name)))
                   (generic-add-implementation
                    generic
                    (make-instance 'generic-implementation :generic generic
                                   :parameter-types (primitive-operation-parameter-types operation)
                                   :result-type (primitive-operation-result-type operation)
                                   :primitive-operation operation)))))
        (dolist (operator '("+" "-" "*" "/" "==" "!=" "<" "<=" ">" ">="))
          (let ((generic (install operator 2)))
            (dolist (signed '(t nil))
              (dolist (width '(8 16 32 64))
                (let ((suffix (format nil "~:[u~;i~]~D" signed width)))
                  (add generic
                       (format nil "~A-primitive-~A"
                               (cond ((string= operator "+") "%+")
                                     ((string= operator "-") "%-")
                                     ((string= operator "*") "%*")
                                     ((string= operator "/") "%/")
                                     ((string= operator "==") "%=")
                                     ((string= operator "!=") "%/=")
                                     (t (format nil "%~A" operator)))
                               suffix)))))
            (when (member operator '("+" "-" "*" "/" "==" "!=" "<" "<=" ">" ">=")
                          :test #'string=)
              (dolist (width '(32 64))
                (let ((suffix (format nil "f~D" width)))
                  (add generic
                       (format nil "~A-primitive-~A"
                               (cond ((string= operator "+") "%+")
                                     ((string= operator "-") "%-")
                                     ((string= operator "*") "%*")
                                     ((string= operator "/") "%/")
                                     ((string= operator "==") "%=")
                                     ((string= operator "!=") "%/=")
                                     (t (format nil "%~A" operator)))
                               suffix))))))))
      scope)))

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

(define-condition duplicate-field-error (semantic-error)
  ((name :initarg :name :reader duplicate-field-error-name)
   (existing :initarg :existing :reader duplicate-field-error-existing)))

(define-condition duplicate-alternative-error (semantic-error)
  ((name :initarg :name :reader duplicate-alternative-error-name)
   (existing :initarg :existing :reader duplicate-alternative-error-existing)))

(define-condition recursive-type-not-supported-error (semantic-error) ())

(define-condition unknown-field-error (semantic-error)
  ((product-type :initarg :product-type :reader unknown-field-error-product-type)
   (name :initarg :name :reader unknown-field-error-name)))

(define-condition field-access-requires-product-error (semantic-error)
  ((actual :initarg :actual :reader field-access-requires-product-error-actual)))

(defun product-type-find-field (product-type name)
  "Return PRODUCT-TYPE's field named NAME, plus a presence flag."
  (check-type product-type product-type)
  (check-type name termis-name)
  (let ((field (find name (product-type-fields product-type)
                     :key #'product-field-name :test #'termis-name=)))
    (values field (not (null field)))))

(defun sum-type-find-alternative (sum-type name)
  "Return SUM-TYPE's alternative named NAME, plus a presence flag."
  (check-type sum-type sum-type)
  (check-type name termis-name)
  (let ((alternative (find name (sum-type-alternatives sum-type)
                           :key #'sum-alternative-name :test #'termis-name=)))
    (values alternative (not (null alternative)))))

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
  (declare (ignore function))
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
	    (semantic-scope-function scope) semantic-declaration
	    (semantic-function-declaration-parameters semantic-declaration) parameters
	    (semantic-function-declaration-return-type-reference semantic-declaration)
	    (resolve-type-syntax module-scope
			 (function-declaration-return-type declaration))))))

(defun resolve-generic-implementation-signature (program semantic-implementation)
  (let* ((declaration (semantic-declaration-source-declaration semantic-implementation))
         (module-scope (semantic-program-module-scope program))
         (target (semantic-scope-lookup module-scope
                                        (implementation-declaration-generic-name declaration))))
    (unless (typep target 'generic-binding)
      (error 'semantic-error :syntax (declaration-source declaration)
             :message "implementation target is not a generic"))
    (let* ((generic (generic-binding-generic target))
           (parameters-syntax (implementation-declaration-parameters declaration)))
      (unless (termis-list-p (syntax-datum parameters-syntax))
        (error 'semantic-error :syntax parameters-syntax
               :message "implementation parameters must be a list"))
      (let ((parameter-syntaxes (termis-list-elements (syntax-datum parameters-syntax))))
        (unless (= (length parameter-syntaxes) (generic-arity generic))
          (error 'generic-arity-mismatch-error :syntax parameters-syntax
                 :generic generic :actual (length parameter-syntaxes)))
        (let ((scope (semantic-scope-child module-scope))
              (parameters (mapcar (lambda (syntax) (parse-parameter declaration syntax))
                                  parameter-syntaxes)))
          (dolist (parameter parameters)
            (setf (parameter-binding-type-reference parameter)
                  (resolve-type-syntax module-scope
                                       (parameter-binding-type-syntax parameter))))
          (dolist (parameter parameters)
            (multiple-value-bind (existing foundp)
                (semantic-scope-local-find scope (semantic-binding-name parameter))
              (when foundp
                (error 'duplicate-local-binding-error :syntax (parameter-binding-syntax parameter)
                       :name (semantic-binding-name parameter) :existing existing))
              (semantic-scope-bind scope (semantic-binding-name parameter) parameter)))
          (setf (slot-value semantic-implementation 'generic) generic
                (semantic-generic-implementation-scope semantic-implementation) scope
                (semantic-scope-function scope) semantic-implementation
                (generic-implementation-parameters semantic-implementation) parameters
                (semantic-generic-implementation-return-type-reference semantic-implementation)
                (resolve-type-syntax module-scope
                                     (implementation-declaration-return-type declaration))))))))

(defun make-semantic-declaration (declaration)
  (cond ((typep declaration 'type-declaration)
	 (make-instance 'semantic-type-declaration :source-declaration declaration))
	((typep declaration 'constant-declaration)
	 (make-instance 'semantic-constant-declaration :source-declaration declaration))
	((typep declaration 'variable-declaration)
	 (make-instance 'semantic-variable-declaration :source-declaration declaration))
	((typep declaration 'function-declaration)
	 (make-instance 'semantic-function-declaration :source-declaration declaration))
	((typep declaration 'generic-declaration)
         (let ((generic (make-instance 'generic :declaration declaration
                                        :name (declaration-name declaration)
                                        :arity (generic-declaration-arity declaration))))
           (make-instance 'semantic-generic-declaration :source-declaration declaration
                          :generic generic)))
	((typep declaration 'implementation-declaration)
         (make-instance 'semantic-generic-implementation :source-declaration declaration
                        :declaration declaration :source (declaration-source declaration)
                        :name (declaration-name declaration)))
	;; Macro declarations have already been handled by the evaluator.
	((typep declaration 'macro-declaration) nil)
	(t (error "Unknown Termis declaration ~S" declaration))))

(defun resolve-declaration-signature (program semantic-declaration)
  (let ((declaration (semantic-declaration-source-declaration semantic-declaration))
	(scope (semantic-program-module-scope program)))
    (cond ((typep semantic-declaration 'semantic-function-declaration)
	   (resolve-function-signature program semantic-declaration))
	  ((typep semantic-declaration 'semantic-generic-implementation)
           (resolve-generic-implementation-signature program semantic-declaration))
	  ((typep semantic-declaration 'semantic-constant-declaration)
	   (setf (semantic-constant-declaration-type-reference semantic-declaration)
		 (resolve-type-syntax scope (constant-declaration-type declaration))))
	  ((typep semantic-declaration 'semantic-variable-declaration)
	   (setf (semantic-variable-declaration-type-reference semantic-declaration)
		 (resolve-type-syntax scope (variable-declaration-type declaration)))))))

(defun product-field-syntaxes (declaration)
  "Normalize the product body while accepting the earlier flat form.

The documented spelling has one list containing every field.  Accepting the
previous flat spelling keeps source compatibility without changing the
semantic representation."
  (let ((body (type-declaration-body declaration)))
    ;; The explicit algebraic spelling is preferred, while the Step 15
    ;; spellings below remain accepted for source compatibility.
    (when (and (= (length body) 1) (termis-list-p (syntax-datum (first body))))
      (let ((elements (termis-list-elements (syntax-datum (first body)))))
        (when (and elements (termis-name-p (syntax-datum (first elements)))
                   (string= (termis-name-value (syntax-datum (first elements))) "product"))
          (return-from product-field-syntaxes (rest elements)))))
    (cond ((and (= (length body) 1) (termis-list-p (syntax-datum (first body)))
		(let ((elements (termis-list-elements (syntax-datum (first body)))))
		  (or (null elements)
		      (termis-list-p (syntax-datum (first elements))))))
	   (termis-list-elements (syntax-datum (first body))))
	  ((every (lambda (syntax) (termis-list-p (syntax-datum syntax))) body) body)
	  ;; Earlier front-end milestones allowed TYPE to be an opaque declaration
	  ;; payload.  Preserve those expansion tests as a zero-field nominal
	  ;; product; actual product syntax is always list-shaped.
	  ((every (lambda (syntax) (not (termis-list-p (syntax-datum syntax)))) body) '())
	  (t body))))

(defun parse-product-field-syntax (field-syntax)
  (unless (termis-list-p (syntax-datum field-syntax))
    (error 'semantic-error :syntax field-syntax
           :message "product field must be a (name type) list"))
  (let ((elements (termis-list-elements (syntax-datum field-syntax))))
    (unless (= (length elements) 2)
      (error 'semantic-error :syntax field-syntax
             :message "product field must contain a name and type"))
    (let ((name (syntax-datum (first elements))))
      (unless (termis-name-p name)
        (error 'semantic-error :syntax (first elements)
               :message "product field name must be a Termis name"))
      (values name (second elements)))))

(defun ensure-type-is-complete (program resolved-type-syntax syntax)
  "Reject self and forward references before a finite type gets a layout."
  (cond ((typep resolved-type-syntax 'semantic-reference)
         (let ((binding (semantic-reference-binding resolved-type-syntax)))
           (when (typep binding 'type-declaration)
             (let ((semantic (semantic-program-declaration program binding)))
               (unless (and (typep semantic 'semantic-type-declaration)
                            (typep (semantic-type-declaration-type semantic)
                                   '(or product-type sum-type)))
                 (error 'recursive-type-not-supported-error :syntax syntax
                        :message "RecursiveTypeNotSupported: type members may reference only earlier complete types"))))))
        ((typep resolved-type-syntax 'semantic-pointer-type-syntax)
         ;; Pointer recursion is deferred with all other recursive product
         ;; machinery, even though LLVM could represent some instances.
         (ensure-type-is-complete
          program (semantic-pointer-type-syntax-target resolved-type-syntax) syntax))))

(defun resolve-product-type-declaration (program semantic-declaration)
  (let* ((declaration (semantic-declaration-source-declaration semantic-declaration))
         (scope (semantic-program-module-scope program))
         (context (semantic-program-type-context program))
         (fields '()))
    (dolist (field-syntax (product-field-syntaxes declaration))
      (multiple-value-bind (name type-syntax) (parse-product-field-syntax field-syntax)
        (let ((existing (find name fields :key #'product-field-name :test #'termis-name=)))
          (when existing
            (error 'duplicate-field-error :syntax (first (termis-list-elements
                                                           (syntax-datum field-syntax)))
                   :message "DuplicateField" :name name :existing existing)))
        (let ((reference (resolve-type-syntax scope type-syntax)))
          (ensure-type-is-complete program reference type-syntax)
          (push (make-instance 'product-field :name name
                               :type (resolve-type context reference)
                               :index (length fields) :source field-syntax)
                fields))))
    (setf fields (nreverse fields))
    ;; Indexes were assigned while accumulating in declaration order.  Repair
    ;; them after reversal so backend indexes always equal source order.
    (loop for field in fields for index from 0
          do (setf (slot-value field 'index) index))
    (setf (semantic-type-declaration-fields semantic-declaration) fields
          (semantic-type-declaration-type semantic-declaration)
          (type-context-product-type context declaration fields))))

(defun sum-alternative-syntaxes (declaration)
  (let ((body (type-declaration-body declaration)))
    (unless (and (= (length body) 1) (termis-list-p (syntax-datum (first body))))
      (error 'semantic-error :syntax (declaration-source declaration)
             :message "sum type body must be a (sum ...) form"))
    (let ((elements (termis-list-elements (syntax-datum (first body)))))
      (unless (and elements (termis-name-p (syntax-datum (first elements)))
                   (string= (termis-name-value (syntax-datum (first elements))) "sum"))
        (error 'semantic-error :syntax (first body)
               :message "type body must begin with product or sum"))
      (rest elements))))

(defun parse-sum-alternative-syntax (alternative-syntax)
  (unless (termis-list-p (syntax-datum alternative-syntax))
    (error 'semantic-error :syntax alternative-syntax
           :message "sum alternative must be a (name type...) list"))
  (let ((elements (termis-list-elements (syntax-datum alternative-syntax))))
    (unless elements
      (error 'semantic-error :syntax alternative-syntax
             :message "sum alternative requires a name"))
    (let ((name (syntax-datum (first elements))))
      (unless (termis-name-p name)
        (error 'semantic-error :syntax (first elements)
               :message "sum alternative name must be a Termis name"))
      (values name (rest elements)))))

(defun resolve-sum-type-declaration (program semantic-declaration)
  (let* ((declaration (semantic-declaration-source-declaration semantic-declaration))
         (scope (semantic-program-module-scope program))
         (context (semantic-program-type-context program))
         ;; Install identity before alternatives, but only after all previous
         ;; declarations are complete.  A reference to this declaration is
         ;; still rejected by ENSURE-TYPE-IS-COMPLETE.
         (sum-type (type-context-sum-type context declaration '()))
         (alternatives '()))
    (dolist (alternative-syntax (sum-alternative-syntaxes declaration))
      (multiple-value-bind (name payload-syntaxes)
          (parse-sum-alternative-syntax alternative-syntax)
        (let ((existing (find name alternatives :key #'sum-alternative-name
                              :test #'termis-name=)))
          (when existing
            (error 'duplicate-alternative-error :syntax alternative-syntax
                   :message "DuplicateAlternative" :name name :existing existing)))
        (let ((payload-types
                (mapcar (lambda (payload-syntax)
                          (let ((reference (resolve-type-syntax scope payload-syntax)))
                            (ensure-type-is-complete program reference payload-syntax)
                            (resolve-type context reference)))
                        payload-syntaxes)))
          (push (make-instance 'sum-alternative :sum-type sum-type :name name
                               :index (length alternatives)
                               :payload-types payload-types :source alternative-syntax)
                alternatives))))
    (setf alternatives (nreverse alternatives))
    (loop for alternative in alternatives for index from 0
          do (setf (slot-value alternative 'index) index))
    (setf (slot-value sum-type 'alternatives) alternatives
          (semantic-type-declaration-alternatives semantic-declaration) alternatives
          (semantic-type-declaration-type semantic-declaration) sum-type)))

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
      ((typep semantic-declaration 'semantic-generic-implementation)
       (let ((parameter-types
               (mapcar (lambda (parameter)
                         (setf (parameter-binding-type parameter)
                               (resolve-type context
                                             (parameter-binding-type-reference parameter))))
                       (generic-implementation-parameters semantic-declaration))))
         (setf (generic-implementation-parameter-types semantic-declaration) parameter-types
               (generic-implementation-result-type semantic-declaration)
               (resolve-type context
                             (semantic-generic-implementation-return-type-reference
                              semantic-declaration))
               (semantic-generic-implementation-type semantic-declaration)
               (type-context-function-type context parameter-types
                                           (generic-implementation-result-type semantic-declaration)))
         (generic-add-implementation (generic-implementation-generic semantic-declaration)
                                     semantic-declaration
                                     (declaration-source
                                      (semantic-declaration-source-declaration semantic-declaration)))))
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
  ;; Finite product and sum definitions are resolved in source order.  Members
  ;; can use only a previously completed type; recursion is deferred.
  (dolist (entry (semantic-program-declarations program))
    (let ((declaration (cdr entry)))
      (when (typep declaration 'semantic-type-declaration)
        (let* ((source (semantic-declaration-source-declaration declaration))
               (body (type-declaration-body source))
               (first-body (first body))
               (head (and first-body (termis-list-p (syntax-datum first-body))
                          (first (termis-list-elements (syntax-datum first-body))))))
          (if (and head (termis-name-p (syntax-datum head))
                   (string= (termis-name-value (syntax-datum head)) "sum"))
              (resolve-sum-type-declaration program declaration)
              (resolve-product-type-declaration program declaration))))))
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

(define-condition generic-arity-mismatch-error (semantic-error)
  ((generic :initarg :generic :reader generic-arity-mismatch-error-generic)
   (actual :initarg :actual :reader generic-arity-mismatch-error-actual))
  (:default-initargs :message "GenericArityMismatch"))

(define-condition duplicate-generic-implementation-error (semantic-error)
  ((generic :initarg :generic :reader duplicate-generic-implementation-error-generic)
   (parameter-types :initarg :parameter-types
                    :reader duplicate-generic-implementation-error-parameter-types)
   (original :initarg :original :reader duplicate-generic-implementation-error-original)
   (duplicate :initarg :duplicate :reader duplicate-generic-implementation-error-duplicate)
   (original-source :initarg :original-source
                    :reader duplicate-generic-implementation-error-original-source)
   (duplicate-source :initarg :duplicate-source
                     :reader duplicate-generic-implementation-error-duplicate-source))
  (:default-initargs :message "DuplicateGenericImplementation"))

(define-condition no-generic-implementation-error (semantic-error)
  ((generic :initarg :generic :reader no-generic-implementation-error-generic)
   (argument-types :initarg :argument-types
                   :reader no-generic-implementation-error-argument-types))
  (:default-initargs :message "NoGenericImplementation"))

(define-condition not-addressable-error (semantic-error) ())
(define-condition not-writable-error (semantic-error) ())
(define-condition invalid-expression-error (semantic-error) ())

(defun termis-type-name (type)
  "A compact stable spelling used in semantic diagnostics."
  (cond ((typep type 'never-type) "never")
	((typep type 'unit-type) "unit")
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
	((typep binding 'pattern-binding) (pattern-binding-type binding))
	((typep binding 'let-binding) (let-binding-type binding))
	((typep binding 'primitive-binding)
	 (builtin-intrinsic-binding-type binding))
	((typep binding 'generic-binding)
         (error 'invalid-expression-error :syntax syntax
                :message "a generic is only callable in call position"))
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

(defun product-constructor-type (scope syntax)
  "Return the resolved ProductType selected by constructor head SYNTAX, if any."
  (when (termis-name-p (syntax-datum syntax))
    (multiple-value-bind (binding foundp)
        (semantic-scope-find scope (syntax-datum syntax))
      (when (and foundp (typep binding 'type-declaration))
        (let* ((program (semantic-scope-owning-program scope))
               (semantic (semantic-program-declaration program binding)))
          (and (typep semantic 'semantic-type-declaration)
               (semantic-type-declaration-type semantic)))))))

(defun infer-construct-expression (syntax scope product-type argument-syntax)
  (let ((fields (product-type-fields product-type)))
    (unless (= (length argument-syntax) (length fields))
      (error 'wrong-argument-count-error :syntax syntax
             :expected (length fields) :actual (length argument-syntax)))
    (make-instance 'construct-expression :syntax syntax :product-type product-type
                   :fields (loop for argument in argument-syntax
                                 for field in fields
                                 collect (check-expression argument scope
                                                           (product-field-type field)))
                   :type product-type)))

(defun infer-sum-construct-expression (syntax scope sum-type alternative argument-syntax)
  (let ((payload-types (sum-alternative-payload-types alternative)))
    (unless (= (length argument-syntax) (length payload-types))
      (error 'wrong-argument-count-error :syntax syntax
             :expected (length payload-types) :actual (length argument-syntax)))
    (make-instance 'sum-construct-expression :syntax syntax
                   :alternative alternative
                   :arguments (loop for argument in argument-syntax
                                    for payload-type in payload-types
                                    collect (check-expression argument scope payload-type))
                   :type sum-type)))

(defun expected-sum-constructor (syntax expected-type)
  "Resolve a constructor name only in the supplied expected sum type."
  (when (and (typep expected-type 'sum-type) (termis-list-p (syntax-datum syntax)))
    (let ((head (first (termis-list-elements (syntax-datum syntax)))) )
      (when (and head (termis-name-p (syntax-datum head)))
        (sum-type-find-alternative expected-type (syntax-datum head))))))

(defun infer-call-expression (syntax scope)
  (let* ((elements (termis-list-elements (syntax-datum syntax)))
	 (product-type (product-constructor-type scope (first elements))))
    (when (typep product-type 'product-type)
      (return-from infer-call-expression
        (infer-construct-expression syntax scope product-type (rest elements))))
    ;; Generics are resolved here, after arguments have concrete semantic
    ;; types, and are immediately replaced by a primitive or ordinary call.
    (when (termis-name-p (syntax-datum (first elements)))
      (multiple-value-bind (head-binding foundp)
          (semantic-scope-find scope (syntax-datum (first elements)))
        (when (and foundp (typep head-binding 'generic-binding))
          (return-from infer-call-expression
            (let* ((generic (generic-binding-generic head-binding))
                 (argument-syntax (rest elements)))
            (unless (= (length argument-syntax) (generic-arity generic))
              (error 'wrong-argument-count-error :syntax syntax
                     :expected (generic-arity generic) :actual (length argument-syntax)))
            (let* ((arguments (mapcar (lambda (argument)
                                        (infer-value-expression argument scope))
                                      argument-syntax))
                   (argument-types (mapcar #'expression-type arguments))
                   (implementation (generic-find-implementation generic argument-types)))
              (unless implementation
                (error 'no-generic-implementation-error :syntax syntax
                       :generic generic :argument-types argument-types))
              (let ((operation (generic-implementation-primitive-operation implementation)))
                (if operation
                    (make-instance 'primitive-call :syntax syntax
                                   :callee (make-instance 'semantic-reference
                                                          :syntax (first elements)
                                                          :name (generic-name generic)
                                                          :binding head-binding)
                                   :arguments arguments :operation operation
                                   :type (generic-implementation-result-type implementation))
                    (let ((callee (make-instance 'semantic-reference
                                                 :syntax (first elements)
                                                 :name (generic-name generic)
                                                 :binding implementation
                                                 :type (semantic-generic-implementation-type implementation))))
                      (make-instance 'semantic-call :syntax syntax :callee callee
                                     :arguments arguments
                                     :type (generic-implementation-result-type implementation)))))))))))
    (let* ((callee (infer-expression (first elements) scope))
	 (callee-type (expression-type callee)))
    (unless (typep callee-type 'function-type)
      (error 'semantic-not-callable-error :syntax (first elements)
					  :actual callee-type))
    (let ((parameter-types (function-type-parameters callee-type))
	  (argument-syntax (rest elements)))
      (unless (= (length argument-syntax) (length parameter-types))
	(error 'wrong-argument-count-error :syntax syntax
					   :expected (length parameter-types) :actual (length argument-syntax)))
      (let* ((arguments (loop for argument in argument-syntax
				     for parameter-type in parameter-types
				     collect (check-expression argument scope parameter-type)))
	     (binding (and (typep callee 'semantic-reference)
			   (semantic-reference-binding callee))))
	(if (typep binding 'primitive-binding)
	    (let* ((operation (primitive-binding-operation binding))
		   (kind (primitive-operation-kind operation))
		   (conversion-p (member kind '(:integer-sign-extend :integer-zero-extend
						 :integer-truncate :signed-integer-to-float
						 :unsigned-integer-to-float :float-to-signed-integer
						 :float-to-unsigned-integer :float-extend
						 :float-truncate))))
	      (make-instance (if conversion-p 'conversion-expression 'primitive-call)
			     :syntax syntax :callee callee :arguments arguments
			     :operation operation :type (function-type-result callee-type)))
	    (make-instance 'semantic-call :syntax syntax :callee callee
			   :arguments arguments :type (function-type-result callee-type))))))))

(defun infer-field-expression (syntax scope)
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (unless (= (length arguments) 2)
      (error 'invalid-expression-error :syntax syntax
             :message "field requires a product value and field name"))
    (let ((value (infer-value-expression (first arguments) scope))
          (name (syntax-datum (second arguments))))
      (unless (termis-name-p name)
        (error 'invalid-expression-error :syntax (second arguments)
               :message "field name must be a Termis name"))
      (let ((product-type (expression-type value)))
        (unless (typep product-type 'product-type)
          (error 'field-access-requires-product-error :syntax (first arguments)
                 :message "field access requires a product value" :actual product-type))
        (multiple-value-bind (field foundp) (product-type-find-field product-type name)
          (unless foundp
            (error 'unknown-field-error :syntax (second arguments) :message "UnknownField"
                   :product-type product-type :name name))
          (make-instance 'field-expression :syntax syntax :value value :field field
                         :type (product-field-type field)))))))

(defun infer-sequence-expression (syntax scope)
  (let ((expressions '()))
    (dolist (form (rest (termis-list-elements (syntax-datum syntax))))
      (when (and expressions (typep (expression-type (car (last expressions))) 'never-type))
        (error 'unreachable-expression-error :syntax form
               :message "expression follows terminating control flow"))
      (push (infer-value-expression form scope) expressions))
    (setf expressions (nreverse expressions))
    (make-instance 'sequence-expression :syntax syntax :expressions expressions
					:type (if expressions
						  (expression-type (car (last expressions)))
						  (type-context-unit-type
						   (semantic-scope-owning-type-context scope))))))

(defun let-definition-name-p (name)
  "Whether NAME is a top-level definition spelling used in executable code."
  (and name
       (member name '("constant" "variable" "%constant" "%variable")
               :test #'string=)))

(defun parse-let-binding (binding-syntax scope)
  "Resolve one LET binding, installing it only after its initializer.

SCOPE is the child scope owned by the enclosing LET.  Earlier bindings are
therefore visible, while the binding being built cannot see itself."
  (unless (termis-list-p (syntax-datum binding-syntax))
    (error 'invalid-expression-error :syntax binding-syntax
           :message "let binding must be a (name type initializer) list"))
  (let ((elements (termis-list-elements (syntax-datum binding-syntax))))
    (unless (= (length elements) 3)
      (error 'invalid-expression-error :syntax binding-syntax
             :message "let binding must contain a name, type, and initializer"))
    (let ((name (syntax-datum (first elements))))
      (unless (termis-name-p name)
        (error 'invalid-expression-error :syntax (first elements)
               :message "let binding name must be a Termis name"))
      (multiple-value-bind (existing foundp) (semantic-scope-local-find scope name)
        (when foundp
          (error 'duplicate-local-binding-error :syntax (first elements)
                 :name name :existing existing)))
      (let* ((type-reference (resolve-type-syntax scope (second elements)))
             (type (resolve-type (semantic-scope-owning-type-context scope)
                                 type-reference))
             (initializer (check-expression (third elements) scope type))
             (binding (make-instance 'let-binding :name name :syntax (first elements)
                                     :type-syntax (second elements)
                                     :type-reference type-reference :type type
                                     :initializer initializer)))
        (semantic-scope-bind scope name binding)
        binding))))

(defun infer-let-body (syntax body-syntaxes scope expected-type)
  "Resolve a LET body as a non-empty expression sequence."
  (unless body-syntaxes
    (error 'invalid-expression-error :syntax syntax :message "let requires a body"))
  (let ((expressions '())
        (last-syntax (car (last body-syntaxes))))
    (dolist (form body-syntaxes)
      (when (and expressions (typep (expression-type (car (last expressions))) 'never-type))
        (error 'unreachable-expression-error :syntax form
               :message "expression follows terminating control flow"))
      (push (if (and expected-type (eq form last-syntax))
                (check-expression form scope expected-type)
                (infer-value-expression form scope))
            expressions))
    (setf expressions (nreverse expressions))
    ;; Preserve the direct body node for the common one-expression form.  A
    ;; multi-form body uses the established sequence representation.
    (if (null (cdr expressions))
        (first expressions)
        (make-instance 'sequence-expression :syntax syntax :expressions expressions
                       :type (expression-type (car (last expressions)))))))

(defun infer-let-expression (syntax scope &optional expected-type)
  "Analyze a sequential, immutable lexical LET expression."
  (let ((elements (termis-list-elements (syntax-datum syntax))))
    (unless (>= (length elements) 3)
      (error 'invalid-expression-error :syntax syntax
             :message "let requires bindings and a body"))
    (let ((bindings-syntax (second elements)))
      (unless (termis-list-p (syntax-datum bindings-syntax))
        (error 'invalid-expression-error :syntax bindings-syntax
               :message "let bindings must be a list"))
      (let ((let-scope (semantic-scope-child scope))
            (bindings '()))
        (dolist (binding-syntax (termis-list-elements (syntax-datum bindings-syntax)))
          (push (parse-let-binding binding-syntax let-scope) bindings))
        (let ((body (infer-let-body syntax (cddr elements) let-scope expected-type)))
          (make-instance 'let-expression :syntax syntax :scope let-scope
                         :bindings (nreverse bindings) :body body
                         :type (expression-type body)))))))

(defun analyze-pattern (syntax scope scrutinee-type)
  "Resolve one source pattern and install a binding in the case scope." 
  (let ((datum (syntax-datum syntax)))
    (cond ((termis-list-p datum)
           (unless (typep scrutinee-type 'sum-type)
             (error 'invalid-expression-error :syntax syntax
                    :message "constructor patterns require a sum scrutinee"))
           (let ((elements (termis-list-elements datum)))
             (unless elements
               (error 'invalid-expression-error :syntax syntax
                      :message "constructor pattern requires an alternative name"))
             (let ((name (syntax-datum (first elements))))
               (unless (termis-name-p name)
                 (error 'invalid-expression-error :syntax (first elements)
                        :message "constructor pattern name must be a Termis name"))
               (multiple-value-bind (alternative foundp)
                   (sum-type-find-alternative scrutinee-type name)
                 (unless foundp
                   (error 'invalid-expression-error :syntax (first elements)
                          :message "unknown sum alternative"))
                 (let ((payload-syntaxes (rest elements))
                       (payload-types (sum-alternative-payload-types alternative)))
                   (unless (= (length payload-syntaxes) (length payload-types))
                     (error 'wrong-argument-count-error :syntax syntax
                            :expected (length payload-types) :actual (length payload-syntaxes)))
                   (make-instance 'constructor-pattern :syntax syntax :type scrutinee-type
                                  :alternative alternative
                                  :payload-patterns
                                  (loop for payload-syntax in payload-syntaxes
                                        for payload-type in payload-types
                                        do (when (termis-list-p (syntax-datum payload-syntax))
                                             (error 'invalid-expression-error :syntax payload-syntax
                                                    :message "nested constructor patterns are not supported yet"))
                                        collect (analyze-pattern payload-syntax scope payload-type))))))))
          ((termis-boolean-literal-p datum)
	   (unless (typep scrutinee-type 'boolean-type)
	     (error 'type-mismatch-error :syntax syntax
		    :actual (type-context-boolean-type (semantic-scope-owning-type-context scope))
		    :expected scrutinee-type))
	   (make-instance 'boolean-pattern :syntax syntax :type scrutinee-type
			  :value (termis-boolean-literal-value datum)))
	  ((integerp datum)
	   (unless (typep scrutinee-type 'integer-type)
	     (error 'type-mismatch-error :syntax syntax
		    :actual (type-context-integer-type (semantic-scope-owning-type-context scope) t 32)
		    :expected scrutinee-type))
	   (make-instance 'integer-pattern :syntax syntax :type scrutinee-type :value datum))
	  ((termis-name-p datum)
	   (if (string= (termis-name-value datum) "_")
	       (make-instance 'wildcard-pattern :syntax syntax :type scrutinee-type)
	       (let ((binding (make-instance 'pattern-binding :name datum :syntax syntax
					    :type scrutinee-type)))
		 (semantic-scope-bind scope datum binding)
		 (make-instance 'binding-pattern :syntax syntax :type scrutinee-type
				:binding binding))))
	  (t (error 'invalid-expression-error :syntax syntax
		    :message "match patterns must be a literal, binding, or _")))))

(defun pattern-catches-all-p (pattern)
  (or (typep pattern 'wildcard-pattern) (typep pattern 'binding-pattern)))

(defun constructor-pattern-complete-p (pattern)
  (and (typep pattern 'constructor-pattern)
       (every #'pattern-catches-all-p (constructor-pattern-payload-patterns pattern))))

(defun pattern-already-covered-p (pattern covered)
  (or (and (pattern-catches-all-p covered) t)
      (and (typep pattern 'constructor-pattern)
           (typep covered 'constructor-pattern)
           (eq (constructor-pattern-alternative pattern)
               (constructor-pattern-alternative covered))
           (constructor-pattern-complete-p covered))
      (and (typep pattern 'boolean-pattern) (typep covered 'boolean-pattern)
	   (eql (literal-pattern-value pattern) (literal-pattern-value covered)))
      (and (typep pattern 'integer-pattern) (typep covered 'integer-pattern)
	   (= (literal-pattern-value pattern) (literal-pattern-value covered)))))

(defun validate-match-coverage (syntax scrutinee-type cases)
  (let ((covered '()))
    (dolist (case cases)
      (let ((pattern (match-case-pattern case)))
	(when (find-if (lambda (prior) (pattern-already-covered-p pattern prior)) covered)
	  (error 'unreachable-pattern-error :syntax (pattern-syntax pattern)
		 :message "pattern is unreachable"
		 :covering-pattern (find-if (lambda (prior) (pattern-already-covered-p pattern prior)) covered)))
	(push pattern covered)))
    (labels ((complete-alternative-p (alternative)
               (find-if (lambda (pattern)
                          (and (typep pattern 'constructor-pattern)
                               (eq (constructor-pattern-alternative pattern) alternative)
                               (constructor-pattern-complete-p pattern)))
                        covered)))
      (unless (or (find-if #'pattern-catches-all-p covered)
                  (and (typep scrutinee-type 'sum-type)
                       (every #'complete-alternative-p
                              (sum-type-alternatives scrutinee-type)))
                  (and (typep scrutinee-type 'boolean-type)
                       (find-if (lambda (p)
                                  (and (typep p 'boolean-pattern)
                                       (literal-pattern-value p)))
                                covered)
                       (find-if (lambda (p)
                                  (and (typep p 'boolean-pattern)
                                       (not (literal-pattern-value p))))
                                covered)))
        (error 'non-exhaustive-match-error :syntax syntax
               :message "match is not exhaustive"
               :uncovered
               (cond ((typep scrutinee-type 'boolean-type) "true or false")
                     ((typep scrutinee-type 'sum-type)
                      (format nil "~{~A~^, ~}"
                              (mapcar (lambda (alternative)
                                        (termis-name-value
                                         (sum-alternative-name alternative)))
                                      (remove-if #'complete-alternative-p
                                                 (sum-type-alternatives scrutinee-type)))))
                     (t "a catch-all pattern")))))))

(defun parse-match-cases (syntax scope scrutinee-type)
  (let ((case-syntaxes (cddr (termis-list-elements (syntax-datum syntax)))))
    (unless case-syntaxes
      (error 'invalid-expression-error :syntax syntax :message "match requires at least one case"))
    (let ((cases
	    (mapcar (lambda (case-syntax)
		      (let ((elements (and (termis-list-p (syntax-datum case-syntax))
				   (termis-list-elements (syntax-datum case-syntax)))))
			(unless (= (length elements) 2)
			  (error 'invalid-expression-error :syntax case-syntax
				 :message "match case must be a (pattern expression) list"))
			(let ((case-scope (semantic-scope-child scope)))
			  (make-instance 'match-case :syntax case-syntax :scope case-scope
				 :pattern (analyze-pattern (first elements) case-scope scrutinee-type)
				 :expression (second elements)))))
		    case-syntaxes)))
      (validate-match-coverage syntax scrutinee-type cases)
      cases)))

(defun infer-match-expression (syntax scope &optional expected-type)
  (let* ((elements (termis-list-elements (syntax-datum syntax)))
	 (value (infer-value-expression (second elements) scope))
	 (cases (parse-match-cases syntax scope (expression-type value))))
    ;; Analyse all branch scopes before choosing contextual literal types.
    (let ((result-type expected-type))
      (unless result-type
	(dolist (case cases)
	  (let ((expression (infer-value-expression (match-case-expression case)
							 (match-case-scope case))))
	    (setf (slot-value case 'expression) expression)
	    (unless (typep (expression-type expression) 'never-type)
	      (setf result-type (expression-type expression))
	      (return))))
	(unless result-type
	  (setf result-type (type-context-never-type (semantic-scope-owning-type-context scope)))))
      (dolist (case cases)
	(let ((expression (match-case-expression case)))
	  (setf (slot-value case 'expression)
		(if (typep expression 'expression)
		    (if (or (typep (expression-type expression) 'never-type)
			    (same-type-p (expression-type expression) result-type)) expression
			(check-expression (match-case-expression case) (match-case-scope case) result-type))
		    (check-expression expression (match-case-scope case) result-type)))))
      (make-instance 'match-expression :syntax syntax :value value :cases cases
			     :type result-type))))

(defun infer-return-expression (syntax scope)
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax))))
	(function (semantic-scope-owning-function scope)))
    (unless function
      (error 'return-outside-function-error :syntax syntax :message "return is only valid inside a function"))
    (unless (= (length arguments) 1)
      (error 'invalid-expression-error :syntax syntax :message "return requires exactly one value"))
    (make-instance 'return-expression :syntax syntax
		   :value (check-expression (first arguments) scope
				    (if (typep function 'semantic-generic-implementation)
                                        (generic-implementation-result-type function)
                                        (semantic-function-declaration-return-type function)))
		   :type (type-context-never-type (semantic-scope-owning-type-context scope)))))

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
    (let* ((operand (infer-value-expression (first arguments) scope))
	   (operand-type (expression-type operand)))
      (unless (typep operand-type 'pointer-type)
	(error 'invalid-expression-error :syntax (first arguments)
					 :message "dereference requires a pointer"))
      (make-instance 'dereference-expression :syntax syntax :operand operand
					     :type (pointer-type-target operand-type)
			     :addressable t :writable t))))

(defun load-place-expression (syntax place)
  "Make a read from PLACE explicit in the resolved semantic program."
  (unless (and (typep place 'place-expression)
	       (place-expression-addressable-p place))
    (error 'not-addressable-error :syntax syntax :message "load requires an addressable place"))
  (make-instance 'load-expression :syntax syntax :place place :type (expression-type place)))

(defun infer-load-expression (syntax scope)
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (unless (= (length arguments) 1)
      (error 'invalid-expression-error :syntax syntax :message "load requires exactly one operand"))
    (load-place-expression syntax (infer-expression (first arguments) scope))))

(defun infer-value-expression (syntax scope)
  "Infer SYNTAX in a value context, preserving reads as explicit LOAD nodes."
  (let ((expression (infer-expression syntax scope)))
    (if (and (typep expression 'place-expression)
	     (place-expression-addressable-p expression))
	(load-place-expression syntax expression)
	expression)))

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
	      (make-instance 'store-expression :syntax syntax :target target
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
					   :value (type-context-unit-value context)
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
		     ((and special (string= special "field"))
		      (infer-field-expression syntax scope))
		     ((and special (string= special "let"))
		      (infer-let-expression syntax scope))
		     ((let-definition-name-p special)
		      (error 'invalid-definition-context-error :syntax syntax
			     :message "constant and variable definitions are only valid at top level"))
		     ((and special (string= special "match"))
		      (let ((elements (termis-list-elements datum)))
			(unless (>= (length elements) 3)
			  (error 'invalid-expression-error :syntax syntax
				 :message "match requires a value and at least one case"))
			(infer-match-expression syntax scope)))
		     ((and special (string= special "return"))
		      (infer-return-expression syntax scope))
		     ((and special (string= special "assign"))
		      (infer-assignment-expression syntax scope))
		     ((and special (string= special "store"))
		      (infer-assignment-expression syntax scope))
		     ((and special (string= special "&"))
		      (infer-address-expression syntax scope))
		     ((and special (string= special "address-of"))
		      (infer-address-expression syntax scope))
		     ((and special (string= special "deref"))
		      (infer-dereference-expression syntax scope))
		     ((and special (string= special "dereference"))
		      (infer-dereference-expression syntax scope))
		     ((and special (string= special "load"))
		      (infer-load-expression syntax scope))
		     (t (infer-call-expression syntax scope))))))
	  (t (error 'invalid-expression-error :syntax syntax
					      :message "unsupported expression")))))

(defun check-expression (syntax scope expected-type)
  "Analyze SYNTAX with EXPECTED-TYPE, contextually typing numeric literals."
  (check-type expected-type termis-type)
  (let ((datum (syntax-datum syntax)))
    (cond ((and (termis-list-p datum)
		(expected-sum-constructor syntax expected-type))
	   (multiple-value-bind (alternative foundp)
	       (expected-sum-constructor syntax expected-type)
	     (declare (ignore foundp))
	     (infer-sum-construct-expression
	      syntax scope expected-type alternative
	      (rest (termis-list-elements datum)))))
	  ((and (termis-list-p datum)
		(expression-special-form-name syntax)
		(string= (expression-special-form-name syntax) "match"))
	   (let ((expression (infer-match-expression syntax scope expected-type)))
	     expression))
	  ((and (termis-list-p datum)
		(expression-special-form-name syntax)
		(string= (expression-special-form-name syntax) "let"))
	   (infer-let-expression syntax scope expected-type))
	  ((and (integerp datum) (typep expected-type 'integer-type))
	   (make-instance 'integer-literal :syntax syntax :value datum :type expected-type))
	  ((and (floatp datum) (typep expected-type 'float-type))
	   (make-instance 'float-literal :syntax syntax :value datum :type expected-type))
	  (t (let ((expression (infer-value-expression syntax scope)))
	       (unless (or (typep (expression-type expression) 'never-type)
		   (compatible-p (expression-type expression) expected-type))
		 (error 'type-mismatch-error :syntax syntax
				     :actual (expression-type expression) :expected expected-type))
	       expression)))))

;;; Backend-readiness validation ------------------------------------------

(define-condition backend-validation-error (semantic-error) ())

(defun backend-validation-fail (object control &rest arguments)
  (error 'backend-validation-error
	 :syntax (and (typep object 'expression) (expression-syntax object))
	 :message (apply #'format nil control arguments)))

(defun backend-representable-type-p (type)
  "Whether TYPE has a complete backend representation contract.

Defined types retain their declaration identity and may be used behind a
pointer.  Their layout is a later type-definition concern, so this predicate
only accepts their semantic identity here; STRING deliberately remains outside
the primitive model."
  (cond ((or (typep type 'unit-type) (typep type 'boolean-type)) t)
	((typep type 'integer-type) (member (integer-type-width type) '(8 16 32 64)))
	((typep type 'float-type) (member (float-type-width type) '(32 64)))
	((typep type 'pointer-type) (backend-representable-type-p (pointer-type-pointee type)))
	((typep type 'function-type)
	 (and (every #'backend-representable-type-p (function-type-parameters type))
	      (backend-representable-type-p (function-type-result type))))
	((typep type 'product-type)
	 (every (lambda (field)
		  (and (typep field 'product-field)
		       (typep (product-field-type field) 'termis-type)
		       (backend-representable-type-p (product-field-type field))))
		(product-type-fields type)))
	((typep type 'sum-type)
	 (every (lambda (alternative)
		  (and (typep alternative 'sum-alternative)
		       (every #'backend-representable-type-p
			      (sum-alternative-payload-types alternative))))
		(sum-type-alternatives type)))
	((typep type 'defined-type) t)
	(t nil)))

(defun validate-primitive-operation (operation expression)
  (unless (typep operation 'primitive-operation)
    (backend-validation-fail expression "primitive call has no PrimitiveOperation identity"))
  (unless (and (every (lambda (type) (typep type 'termis-type))
		      (primitive-operation-parameter-types operation))
	       (typep (primitive-operation-result-type operation) 'termis-type))
    (backend-validation-fail expression "primitive operation has an unresolved type"))
  (unless (backend-representable-type-p (primitive-operation-result-type operation))
    (backend-validation-fail expression "primitive operation result is not backend representable")))

(defun validate-expression-for-backend (expression)
  (unless (and (typep expression 'expression) (typep (expression-type expression) 'termis-type))
    (backend-validation-fail expression "expression is missing a resolved semantic type"))
  (unless (or (typep (expression-type expression) 'never-type)
	      (backend-representable-type-p (expression-type expression)))
    (backend-validation-fail expression "expression type ~A is not backend representable"
			     (termis-type-name (expression-type expression))))
  (cond
    ((typep expression 'construct-expression)
     (let ((product-type (construct-expression-product-type expression))
	   (values (construct-expression-fields expression)))
       (unless (and (typep product-type 'product-type)
		    (same-type-p (expression-type expression) product-type)
		    (= (length values) (length (product-type-fields product-type))))
	 (backend-validation-fail expression "product construction is incomplete"))
       (loop for value in values
	     for field in (product-type-fields product-type)
	     do (validate-expression-for-backend value)
		(unless (same-type-p (expression-type value) (product-field-type field))
		  (backend-validation-fail expression "product constructor has a non-exact field type")))))
    ((typep expression 'sum-construct-expression)
     (let* ((alternative (sum-construct-expression-alternative expression))
	    (sum-type (and (typep alternative 'sum-alternative)
			   (sum-alternative-sum-type alternative)))
	    (arguments (sum-construct-expression-arguments expression)))
       (unless (and (typep sum-type 'sum-type)
		    (member alternative (sum-type-alternatives sum-type) :test #'eq)
		    (same-type-p (expression-type expression) sum-type)
		    (= (length arguments) (length (sum-alternative-payload-types alternative))))
	 (backend-validation-fail expression "sum construction is incomplete"))
       (loop for argument in arguments
	     for payload-type in (sum-alternative-payload-types alternative)
	     do (validate-expression-for-backend argument)
		(unless (same-type-p (expression-type argument) payload-type)
		  (backend-validation-fail expression "sum constructor has a non-exact payload type")))))
    ((typep expression 'field-expression)
     (let* ((value (field-expression-value expression))
	    (field (field-expression-field expression))
	    (product-type (expression-type value)))
       (validate-expression-for-backend value)
       (unless (and (typep product-type 'product-type)
		    (typep field 'product-field)
		    (member field (product-type-fields product-type) :test #'eq)
		    (same-type-p (expression-type expression) (product-field-type field)))
	 (backend-validation-fail expression "field access is unresolved or has the wrong type"))))
    ((typep expression 'return-expression)
     (validate-expression-for-backend (return-expression-value expression))
     (unless (and (typep (expression-type expression) 'never-type)
		  (typep (return-expression-value expression) 'expression))
       (backend-validation-fail expression "return is not fully resolved")))
    ((typep expression 'match-expression)
     (validate-expression-for-backend (match-expression-value expression))
	     (unless (or (typep (expression-type (match-expression-value expression)) 'boolean-type)
			 (typep (expression-type (match-expression-value expression)) 'integer-type)
			 (typep (expression-type (match-expression-value expression)) 'sum-type))
	       (backend-validation-fail expression "match scrutinee has no LLVM comparison lowering"))
     (dolist (case (match-expression-cases expression))
       (let ((pattern (match-case-pattern case))
	     (branch (match-case-expression case)))
	   (unless (and (typep pattern 'pattern)
			(typep (pattern-type pattern) 'termis-type)
			(same-type-p (pattern-type pattern)
				     (expression-type (match-expression-value expression))))
	     (backend-validation-fail expression "match pattern is unresolved or incompatible"))
	   (when (typep pattern 'binding-pattern)
	     (unless (and (typep (binding-pattern-binding pattern) 'pattern-binding)
			  (same-type-p (pattern-binding-type (binding-pattern-binding pattern))
				       (pattern-type pattern)))
	       (backend-validation-fail expression "match binding is unresolved")))
	   (when (typep pattern 'constructor-pattern)
	     (let ((alternative (constructor-pattern-alternative pattern)))
	       (unless (and (typep alternative 'sum-alternative)
			    (eq (sum-alternative-sum-type alternative)
				(expression-type (match-expression-value expression)))
			    (= (length (constructor-pattern-payload-patterns pattern))
			       (length (sum-alternative-payload-types alternative))))
		 (backend-validation-fail expression "constructor pattern is unresolved"))
	       (loop for payload-pattern in (constructor-pattern-payload-patterns pattern)
		     for payload-type in (sum-alternative-payload-types alternative)
		     do (unless (same-type-p (pattern-type payload-pattern) payload-type)
			  (backend-validation-fail expression "constructor payload pattern has the wrong type")))))
	   (validate-expression-for-backend branch)
	   (unless (or (typep (expression-type branch) 'never-type)
		       (same-type-p (expression-type branch) (expression-type expression)))
	     (backend-validation-fail expression "match branch has a different result type")))))
	    ((typep expression 'let-expression)
	     (dolist (binding (let-expression-bindings expression))
	       (unless (and (typep binding 'let-binding)
			    (typep (let-binding-type binding) 'termis-type)
			    (typep (let-binding-initializer binding) 'expression))
		 (backend-validation-fail expression "let binding is incomplete"))
	       (validate-expression-for-backend (let-binding-initializer binding))
	       (unless (same-type-p (expression-type (let-binding-initializer binding))
			    (let-binding-type binding))
		 (backend-validation-fail expression "let initializer is not exactly typed")))
	     (validate-expression-for-backend (let-expression-body expression))
	     (unless (same-type-p (expression-type expression)
			  (expression-type (let-expression-body expression)))
	       (backend-validation-fail expression "let result type is not its body type")))
    ((typep expression 'unit-expression)
     (unless (typep (expression-type expression) 'unit-type)
       (backend-validation-fail expression "UnitValue does not have UnitType")))
    ((typep expression 'semantic-reference)
     (unless (semantic-reference-binding expression)
       (backend-validation-fail expression "reference is unresolved")))
    ((typep expression 'load-expression)
     (let ((place (load-expression-place expression)))
       (validate-expression-for-backend place)
       (unless (and (typep place 'place-expression)
		    (place-expression-addressable-p place)
		    (same-type-p (expression-type place) (expression-type expression)))
	 (backend-validation-fail expression "load does not read its exactly typed place"))))
    ((typep expression 'address-expression)
     (let ((place (address-expression-operand expression)))
       (validate-expression-for-backend place)
       (unless (and (typep place 'place-expression)
		    (place-expression-addressable-p place)
		    (typep (expression-type expression) 'pointer-type)
		    (same-type-p (pointer-type-pointee (expression-type expression))
			 (expression-type place)))
	 (backend-validation-fail expression "address-of is not fully typed"))))
    ((typep expression 'dereference-expression)
     (validate-expression-for-backend (dereference-expression-operand expression))
     (unless (and (typep (expression-type (dereference-expression-operand expression)) 'pointer-type)
		  (same-type-p (expression-type expression)
		       (pointer-type-pointee (expression-type (dereference-expression-operand expression)))))
	(backend-validation-fail expression "dereference is not fully typed")))
    ((typep expression 'store-expression)
     (validate-expression-for-backend (assignment-expression-target expression))
     (validate-expression-for-backend (assignment-expression-value expression))
     (unless (and (typep (assignment-expression-target expression) 'place-expression)
		  (place-expression-writable-p (assignment-expression-target expression))
		  (same-type-p (expression-type (assignment-expression-target expression))
		       (expression-type (assignment-expression-value expression)))
		  (typep (expression-type expression) 'unit-type))
	(backend-validation-fail expression "store is not exactly typed")))
    ((typep expression 'sequence-expression)
     (let ((children (sequence-expression-expressions expression)))
       (dolist (child children)
	 (validate-expression-for-backend child))
       (if children
	   (unless (same-type-p (expression-type expression)
			      (expression-type (car (last children))))
	     (backend-validation-fail expression "sequence result type is not its final value"))
	   (unless (typep (expression-type expression) 'unit-type)
	     (backend-validation-fail expression "empty sequence does not produce unit")))))
    ((typep expression 'primitive-call)
     (let ((operation (primitive-call-operation expression))
	   (arguments (semantic-call-arguments expression)))
       (validate-primitive-operation operation expression)
       (unless (= (length arguments) (length (primitive-operation-parameter-types operation)))
	 (backend-validation-fail expression "primitive call has an invalid argument count"))
       (loop for argument in arguments
	     for parameter in (primitive-operation-parameter-types operation)
	     do (validate-expression-for-backend argument)
		(unless (same-type-p (expression-type argument) parameter)
		  (backend-validation-fail expression "primitive call has a non-exact argument type")))
       (unless (same-type-p (expression-type expression) (primitive-operation-result-type operation))
	 (backend-validation-fail expression "primitive call result type disagrees with its operation"))))
    ((typep expression 'semantic-call)
     (validate-expression-for-backend (semantic-call-callee expression))
     (let ((callee-type (expression-type (semantic-call-callee expression))))
       (unless (typep callee-type 'function-type)
	 (backend-validation-fail expression "call target is not callable"))
       (unless (= (length (semantic-call-arguments expression))
		  (length (function-type-parameters callee-type)))
	 (backend-validation-fail expression "call has an invalid argument count"))
       (loop for argument in (semantic-call-arguments expression)
	     for parameter in (function-type-parameters callee-type)
	     do (validate-expression-for-backend argument)
		(unless (same-type-p (expression-type argument) parameter)
		  (backend-validation-fail expression "call has a non-exact argument type")))
       (unless (same-type-p (expression-type expression) (function-type-result callee-type))
	 (backend-validation-fail expression "call result type disagrees with callee type"))))))

(defun validate-for-backend (program)
  "Final frontend gate: return PROGRAM only when LLVM lowering is mechanical."
  (check-type program semantic-program)
  (let ((context (semantic-program-type-context program)))
    (unless (member (type-context-pointer-width context) '(32 64))
      (backend-validation-fail nil "target pointer width is not representable"))
    (unless (and (typep (type-context-unit-type context) 'unit-type)
		 (typep (type-context-unit-value context) 'unit-value)
		 (= (unit-machine-representation context (type-context-unit-value context)) 0))
      (backend-validation-fail nil "unit does not have its canonical zero representation"))
    ;; Validate the entire bootstrapped primitive environment, not merely the
    ;; subset reached by this program's source.  A backend therefore has one
    ;; closed, representable primitive vocabulary to implement.
    (dolist (entry (semantic-scope-bindings (semantic-program-bootstrap-scope program)))
      (let ((binding (cdr entry)))
	(when (typep binding 'primitive-binding)
	  (validate-primitive-operation (primitive-binding-operation binding) nil))))
    (dolist (entry (semantic-program-declarations program))
      (let ((declaration (cdr entry)))
	(cond ((typep declaration 'semantic-function-declaration)
	       (let ((type (semantic-function-declaration-type declaration)))
		 (unless (and (typep type 'function-type)
			      (equal (function-type-parameters type)
				     (mapcar #'parameter-binding-type
					     (semantic-function-declaration-parameters declaration)))
			      (same-type-p (function-type-result type)
				   (semantic-function-declaration-return-type declaration)))
		   (backend-validation-fail nil "function signature is incomplete"))
		 (validate-expression-for-backend (semantic-function-declaration-body declaration))
		 (unless (or (typep (expression-type (semantic-function-declaration-body declaration)) 'never-type)
			     (same-type-p (expression-type (semantic-function-declaration-body declaration))
				  (semantic-function-declaration-return-type declaration)))
		   (backend-validation-fail (semantic-function-declaration-body declaration)
					    "function result is not exactly typed"))))
	      ((typep declaration 'semantic-generic-declaration)
               (let ((generic (semantic-generic-declaration-generic declaration)))
                 (unless (and (typep generic 'generic)
                              (= (generic-arity generic)
                                 (generic-declaration-arity
                                  (semantic-declaration-source-declaration declaration))))
                   (backend-validation-fail nil "generic declaration is incomplete"))))
	      ((typep declaration 'semantic-generic-implementation)
               (let ((type (semantic-generic-implementation-type declaration))
                     (body (generic-implementation-body declaration)))
                 (unless (and (typep type 'function-type)
                              (= (length (generic-implementation-parameters declaration))
                                 (generic-arity (generic-implementation-generic declaration)))
                              (equal (function-type-parameters type)
                                     (generic-implementation-parameter-types declaration))
                              (same-type-p (function-type-result type)
                                           (generic-implementation-result-type declaration)))
                   (backend-validation-fail nil "generic implementation signature is incomplete"))
                 (validate-expression-for-backend body)
                 (unless (or (typep (expression-type body) 'never-type)
                             (same-type-p (expression-type body)
                                          (generic-implementation-result-type declaration)))
                   (backend-validation-fail body "generic implementation result is not exactly typed"))))
	      ((typep declaration 'semantic-type-declaration)
	       (let ((type (semantic-type-declaration-type declaration)))
		 (unless (and (typep type '(or product-type sum-type))
			      (eq (defined-type-declaration type)
				  (semantic-declaration-source-declaration declaration))
			      (if (typep type 'product-type)
				  (loop for field in (product-type-fields type)
					for index from 0
					always (and (typep field 'product-field)
						    (typep (product-field-name field) 'termis-name)
						    (= (product-field-index field) index)
						    (backend-representable-type-p (product-field-type field))))
				  (loop for alternative in (sum-type-alternatives type)
					for index from 0
					always (and (typep alternative 'sum-alternative)
						    (eq (sum-alternative-sum-type alternative) type)
						    (typep (sum-alternative-name alternative) 'termis-name)
						    (= (sum-alternative-index alternative) index)
						    (every #'backend-representable-type-p
							   (sum-alternative-payload-types alternative))))))
		   (backend-validation-fail nil "type declaration is incomplete"))))
	      ((typep declaration 'semantic-constant-declaration)
	       (let ((initializer (semantic-constant-declaration-initializer declaration)))
		 (when (typep (semantic-constant-declaration-type declaration) '(or product-type sum-type))
		   (backend-validation-fail initializer
			    "top-level aggregate constants are not supported yet"))
		 (validate-expression-for-backend initializer)
		 (unless (same-type-p (expression-type initializer)
				      (semantic-constant-declaration-type declaration))
		   (backend-validation-fail initializer "constant initializer is not exactly typed"))))
	      ((typep declaration 'semantic-variable-declaration)
	       (let ((initializer (semantic-variable-declaration-initializer declaration)))
		 (when (typep (semantic-variable-declaration-type declaration) '(or product-type sum-type))
		   (backend-validation-fail initializer
			    "top-level aggregate variables are not supported yet"))
		 (validate-expression-for-backend initializer)
		 (unless (same-type-p (expression-type initializer)
				      (semantic-variable-declaration-type declaration))
		   (backend-validation-fail initializer "variable initializer is not exactly typed")))))))
  program))

(defun resolve-declaration-body (program semantic-declaration)
  (let ((declaration (semantic-declaration-source-declaration semantic-declaration)))
    (cond ((typep semantic-declaration 'semantic-function-declaration)
	   (setf (semantic-function-declaration-body semantic-declaration)
		 (check-expression
		  (function-declaration-body declaration)
		  (semantic-function-declaration-scope semantic-declaration)
		  (semantic-function-declaration-return-type semantic-declaration))))
	  ((typep semantic-declaration 'semantic-generic-implementation)
           (setf (generic-implementation-body semantic-declaration)
                 (check-expression
                  (implementation-declaration-body declaration)
                  (semantic-generic-implementation-scope semantic-declaration)
                  (generic-implementation-result-type semantic-declaration))))
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
    ;; Establish every semantic identity before publishing runtime names.
    ;; Implementations deliberately do not occupy the module namespace.
    (dolist (declaration (unit-declarations unit))
      (unless (typep declaration 'macro-declaration)
        (let ((semantic-declaration (make-semantic-declaration declaration)))
          (push (cons declaration semantic-declaration)
                (semantic-program-declarations program)))))
    (setf (semantic-program-declarations program)
	  (nreverse (semantic-program-declarations program)))
    (dolist (entry (semantic-program-declarations program))
      (let* ((declaration (car entry))
             (semantic-declaration (cdr entry)))
        (cond ((typep semantic-declaration 'semantic-generic-declaration)
               (let ((generic (semantic-generic-declaration-generic semantic-declaration)))
                 (semantic-scope-bind module-scope (declaration-name declaration)
                                      (make-instance 'generic-binding
                                                     :name (declaration-name declaration)
                                                     :generic generic))))
              ((not (typep declaration 'implementation-declaration))
               (semantic-scope-bind module-scope (declaration-name declaration) declaration)))))
    (dolist (entry (semantic-program-declarations program))
      (resolve-declaration-signature program (cdr entry)))
    (resolve-types program)
    (dolist (entry (semantic-program-declarations program))
      (resolve-declaration-body program (cdr entry)))
    (validate-for-backend program)
    (setf (compilation-unit-semantic-program unit) program)
    program))
