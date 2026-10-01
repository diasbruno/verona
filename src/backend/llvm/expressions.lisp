(in-package #:verona.backend.llvm)

(defun llvm-trap-function (backend)
  "Return the module-local declaration of LLVM's non-returning trap intrinsic."
  (or (llvm-backend-trap-function backend)
      (setf (llvm-backend-trap-function backend)
            (llvm:add-function
             (llvm-backend-module backend) "llvm.trap"
             (llvm:function-type (llvm:void-type :context (llvm-backend-context backend))
                                 '())))))

(defun emit-checked-array-element-address (backend array-type base-address index-expression)
  "Branch to llvm.trap when INDEX is outside ARRAY-TYPE, then form its GEP."
  (let* ((builder (llvm-backend-builder backend))
         (function (llvm:basic-block-parent (llvm:insertion-block builder)))
         (access (llvm:append-basic-block function "array.index.ok"
                                           :context (llvm-backend-context backend)))
         (failure (llvm:append-basic-block function "array.index.oob"
                                            :context (llvm-backend-context backend)))
         (index (emit-value backend index-expression))
         (index-type (lower-type backend (verona:expression-type index-expression)))
         (bound (llvm:const-int index-type (verona:array-type-length array-type))))
    (llvm:build-cond-br builder (llvm:build-i-cmp builder :unsigned-< index bound "array.in.bounds")
                        access failure)
    (llvm:position-builder-at-end builder failure)
    (llvm:build-call builder (llvm-trap-function backend) '())
    (llvm:build-unreachable builder)
    (llvm:position-builder-at-end builder access)
    (llvm:build-gep builder base-address
                    (list (llvm:const-int (llvm:int-type 32
                                                          :context (llvm-backend-context backend)) 0)
                          index)
                    "array.element" (lower-type backend array-type))))

(defun emit-array-value-address (backend expression)
  "Materialize an array value only for a dynamic index; LET values stay SSA."
  (let* ((array-type (verona:expression-type expression))
         (address (llvm:build-alloca (llvm-backend-builder backend)
                                     (lower-type backend array-type) "array.value")))
    (llvm:build-store (llvm-backend-builder backend) (emit-value backend expression) address)
    address))

(defun emit-place (backend expression)
  "Emit an LLVM address for a semantic Place expression."
  (cond
    ((typep expression 'verona:reference-expression)
     (let ((binding (verona:semantic-reference-binding expression)))
       (unless (or (typep binding 'verona:parameter-binding)
                   (typep binding 'verona:variable-declaration))
         (backend-fail "semantic binding ~S is not an LLVM place" binding))
       (backend-binding backend binding)))
    ((typep expression 'verona:dereference-expression)
     (emit-value backend (verona:dereference-expression-operand expression)))
    ((typep expression 'verona:index-place)
     (emit-checked-array-element-address
      backend (verona:expression-type (verona:index-expression-base expression))
      (emit-place backend (verona:index-expression-base expression))
      (verona:index-expression-index expression)))
    (t (backend-fail "expression ~S is not an LLVM place" expression))))

(defun emit-reference-value (backend expression)
  (let ((binding (verona:semantic-reference-binding expression)))
    (cond ((typep binding 'verona:function-declaration)
           (backend-binding backend binding))
	  ((typep binding 'verona:pattern-binding)
	   ;; Binding patterns introduce an SSA value, not a mutable place.
	   (backend-binding backend binding))
	  ((typep binding 'verona:let-binding)
	   ;; Immutable LET bindings are SSA values, never implicit stack storage.
	   (backend-binding backend binding))
          ((typep binding 'verona:constant-declaration)
           (llvm:build-load (llvm-backend-builder backend)
                            (backend-binding backend binding) "constant"
                            (lower-type backend (verona:expression-type expression))))
          ;; A reference remaining in a value position is a frontend
          ;; invariant violation: the resolver inserts LOAD explicitly.
          (t (backend-fail "unlowered value reference to ~S" binding)))))

(defun match-pattern-constant (backend pattern)
  (llvm:const-int (lower-type backend (verona:pattern-type pattern))
		  (let ((value (verona:literal-pattern-value pattern)))
		    (if (typep pattern 'verona:boolean-pattern)
			(if value 1 0)
			value))))

(defun sum-payload-aggregate (backend sum-value alternative)
  (llvm:build-extract-value (llvm-backend-builder backend) sum-value
                            (1+ (verona:sum-alternative-index alternative))
                            "sum.payload"))

(defun emit-pattern-bindings (backend scrutinee pattern)
  "Populate semantic PatternBinding identities after a case is selected."
  (cond
    ((typep pattern 'verona:binding-pattern)
     (setf (backend-binding backend (verona:binding-pattern-binding pattern)) scrutinee))
    ((typep pattern 'verona:constructor-pattern)
     (let ((payload (sum-payload-aggregate
                     backend scrutinee (verona:constructor-pattern-alternative pattern))))
       (loop for payload-pattern in (verona:constructor-pattern-payload-patterns pattern)
             for index from 0
             do (when (typep payload-pattern 'verona:binding-pattern)
	                  (setf (backend-binding backend
	                                         (verona:binding-pattern-binding payload-pattern))
	                        (llvm:build-extract-value (llvm-backend-builder backend)
	                                                  payload index "sum.binding"))))))))

(defun emit-match-dispatch (backend scrutinee pattern target fallback function)
  "Emit one ordered pattern test and leave the builder at FALLBACK."
  (let ((builder (llvm-backend-builder backend)))
    (cond
      ((or (typep pattern 'verona:wildcard-pattern)
           (typep pattern 'verona:binding-pattern))
       (llvm:build-br builder target))
      ((typep pattern 'verona:constructor-pattern)
       (let* ((alternative (verona:constructor-pattern-alternative pattern))
              (tag (llvm:build-extract-value builder scrutinee 0 "sum.tag"))
              (tag-match (llvm:build-i-cmp
                          builder := tag
                          (llvm:const-int (llvm:int-type 32
                                                         :context (llvm-backend-context backend))
                                          (verona:sum-alternative-index alternative))
                          "sum.tag.match"))
              (literal-patterns
                (loop for payload-pattern in (verona:constructor-pattern-payload-patterns pattern)
                      for index from 0
                      unless (or (typep payload-pattern 'verona:wildcard-pattern)
                                 (typep payload-pattern 'verona:binding-pattern))
                        collect (cons index payload-pattern))))
         (if (null literal-patterns)
             (llvm:build-cond-br builder tag-match target fallback)
             (let ((first-test (llvm:append-basic-block function "sum.payload.test"
                                                         :context (llvm-backend-context backend))))
               (llvm:build-cond-br builder tag-match first-test fallback)
               (llvm:position-builder-at-end builder first-test)
               (let ((payload (sum-payload-aggregate backend scrutinee alternative)))
                 (loop for remaining on literal-patterns
                       for entry = (car remaining)
                       for next = (if (cdr remaining)
                                      (llvm:append-basic-block function "sum.payload.test"
                                                               :context (llvm-backend-context backend))
                                      target)
                       do (let ((matches (llvm:build-i-cmp
                                          builder :=
                                          (llvm:build-extract-value builder payload (car entry)
                                                                    "sum.payload.value")
                                          (match-pattern-constant backend (cdr entry))
                                          "sum.payload.match")))
	                            (llvm:build-cond-br builder matches next fallback)
	                            (when (cdr remaining)
	                              (llvm:position-builder-at-end builder next)))))))))
      (t
       (llvm:build-cond-br builder
                           (llvm:build-i-cmp builder := scrutinee
                                             (match-pattern-constant backend pattern) "match.test")
                           target fallback)))
    (llvm:position-builder-at-end builder fallback)))

(defun emit-match-value (backend expression)
  "Lower a resolved match without re-evaluating its scrutinee.

The semantic checker guarantees exhaustiveness and compatible patterns.  The
only job here is to form the CFG and merge non-terminating case values."
  (let* ((builder (llvm-backend-builder backend))
	 (function (llvm:basic-block-parent (llvm:insertion-block builder)))
	 (scrutinee (emit-value backend (verona:match-expression-value expression)))
	 (cases (verona:match-expression-cases expression))
	 (case-blocks (mapcar (lambda (case)
				 (declare (ignore case))
				 (llvm:append-basic-block function "match.case"
							  :context (llvm-backend-context backend)))
			       cases))
	 (terminatingp (typep (verona:expression-type expression) 'verona:never-type))
	 (end-block (unless terminatingp
		      (llvm:append-basic-block function "match.end"
					       :context (llvm-backend-context backend))))
	 (default-block (llvm:append-basic-block function "match.unreachable"
					  :context (llvm-backend-context backend))))
    ;; Dispatch is ordered.  Constructor tag and literal payload tests are
    ;; resolved from semantic identities; no source-name lookup reaches LLVM.
    (loop for case in cases
	  for block in case-blocks
	  for remaining on cases
	  for pattern = (verona:match-case-pattern case)
          do (emit-match-dispatch
              backend scrutinee pattern block
              (if (cdr remaining)
                  (llvm:append-basic-block function "match.test"
                                           :context (llvm-backend-context backend))
                  default-block)
              function))
    (llvm:position-builder-at-end builder default-block)
    (llvm:build-unreachable builder)
    (let ((incoming '()))
      (loop for case in cases
	    for block in case-blocks
	    do (llvm:position-builder-at-end builder block)
	       (let ((pattern (verona:match-case-pattern case)))
		 (emit-pattern-bindings backend scrutinee pattern))
	       (let ((branch (verona:match-case-expression case)))
		 (let ((value (emit-value backend branch))
		       (source (llvm:insertion-block builder)))
		   (unless (typep (verona:expression-type branch) 'verona:never-type)
		     (llvm:build-br builder end-block)
		     (push (cons value source) incoming)))))
      (unless terminatingp
	(llvm:position-builder-at-end builder end-block)
	(cond ((typep (verona:expression-type expression) 'verona:unit-type)
	       (llvm:const-int (lower-type backend (verona:expression-type expression)) 0))
	      ((null (cdr incoming)) (caar incoming))
	      (t (let ((phi (llvm:build-phi builder
					 (lower-type backend (verona:expression-type expression))
					 "match.result")))
		   (llvm:add-incoming phi
			      (coerce (mapcar #'car incoming) 'vector)
			      (coerce (mapcar #'cdr incoming) 'vector))
		   phi)))))))

(defun emit-value (backend expression)
  "Emit EXPRESSION's already-resolved LLVM value."
  (cond
    ((typep expression 'verona:unit-expression)
     (llvm:const-int (lower-type backend (verona:expression-type expression)) 0))
    ((typep expression 'verona:boolean-literal)
     (llvm:const-int (lower-type backend (verona:expression-type expression))
                     (if (verona:boolean-literal-value expression) 1 0)))
    ((typep expression 'verona:integer-literal)
     (llvm:const-int (lower-type backend (verona:expression-type expression))
                     (verona:integer-literal-value expression)))
    ((typep expression 'verona:float-literal)
     (llvm:const-real (lower-type backend (verona:expression-type expression))
                      (verona:float-literal-value expression)))
    ((typep expression 'verona:construct-expression)
     (let ((aggregate (llvm:undef (lower-type backend
                                             (verona:construct-expression-product-type expression)))))
       (loop for value-expression in (verona:construct-expression-fields expression)
             for index from 0
             do (setf aggregate
                      (llvm:build-insert-value (llvm-backend-builder backend)
                                               aggregate
                                               (emit-value backend value-expression)
                                               index "product.insert")))
       aggregate))
    ((typep expression 'verona:sum-construct-expression)
     (let* ((alternative (verona:sum-construct-expression-alternative expression))
            (sum-type (verona:sum-alternative-sum-type alternative))
            (aggregate (llvm:undef (lower-type backend sum-type)))
            (aggregate (llvm:build-insert-value
                        (llvm-backend-builder backend) aggregate
                        (llvm:const-int (llvm:int-type 32 :context (llvm-backend-context backend))
                                        (verona:sum-alternative-index alternative))
                        0 "sum.tag"))
            (payload (llvm:undef
                      (llvm:struct-type
                       (mapcar (lambda (payload-type) (lower-type backend payload-type))
                               (verona:sum-alternative-payload-types alternative))
                       nil :context (llvm-backend-context backend)))))
       (loop for value-expression in (verona:sum-construct-expression-arguments expression)
             for index from 0
             do (setf payload (llvm:build-insert-value (llvm-backend-builder backend)
                                                       payload (emit-value backend value-expression)
                                                       index "sum.payload.insert")))
       (llvm:build-insert-value (llvm-backend-builder backend) aggregate payload
                                (1+ (verona:sum-alternative-index alternative))
                                "sum.payload")))
    ((typep expression 'verona:array-construct-expression)
     (let ((aggregate (llvm:undef (lower-type backend (verona:expression-type expression)))))
       (loop for value-expression in (verona:array-construct-expression-elements expression)
             for index from 0
             do (setf aggregate
                      (llvm:build-insert-value (llvm-backend-builder backend) aggregate
                                               (emit-value backend value-expression)
                                               index "array.insert")))
       aggregate))
    ((typep expression 'verona:index-expression)
     (let ((address (emit-checked-array-element-address
                     backend (verona:expression-type (verona:index-expression-base expression))
                     (emit-array-value-address backend (verona:index-expression-base expression))
                     (verona:index-expression-index expression))))
       (llvm:build-load (llvm-backend-builder backend) address "array.element.value"
                        (lower-type backend (verona:expression-type expression)))))
    ((typep expression 'verona:field-expression)
     (let ((field (verona:field-expression-field expression)))
       ;; The resolver records ProductField identity and index.  LLVM never
       ;; sees or looks up a source-level field name.
       (llvm:build-extract-value (llvm-backend-builder backend)
                                 (emit-value backend
                                             (verona:field-expression-value expression))
                                 (verona:product-field-index field)
                                 "product.field")))
    ((typep expression 'verona:reference-expression)
     (emit-reference-value backend expression))
    ((typep expression 'verona:load-expression)
     (llvm:build-load (llvm-backend-builder backend)
                      (emit-place backend (verona:load-expression-place expression))
                      "load"
                      (lower-type backend (verona:expression-type expression))))
    ((typep expression 'verona:address-expression)
     (emit-place backend (verona:address-expression-operand expression)))
    ((typep expression 'verona:dereference-expression)
     (emit-place backend expression))
    ((typep expression 'verona:pointer-cast-expression)
     ;; Pointer casts involving void* change only the Verona semantic type;
     ;; LLVM opaque pointers require no generated conversion instruction.
     (emit-value backend (verona:pointer-cast-expression-operand expression)))
    ((typep expression 'verona:store-expression)
     (llvm:build-store (llvm-backend-builder backend)
                       (emit-value backend (verona:assignment-expression-value expression))
                       (emit-place backend (verona:assignment-expression-target expression)))
     (llvm:const-int (lower-type backend (verona:expression-type expression)) 0))
    ((typep expression 'verona:sequence-expression)
     (let ((expressions (verona:sequence-expression-expressions expression)))
       (if expressions
           (loop for child in expressions
                 for value = (emit-value backend child)
                 finally (return value))
           (llvm:const-int (lower-type backend (verona:expression-type expression)) 0))))

    ((typep expression 'verona:let-expression)
     ;; Binding identity is the environment key, so nested shadowing needs no
     ;; LLVM-level name lookup or environment restoration.
     (dolist (binding (verona:let-expression-bindings expression))
       (setf (backend-binding backend binding)
             (emit-value backend (verona:let-binding-initializer binding))))
     (emit-value backend (verona:let-expression-body expression)))

    ((typep expression 'verona:return-expression)
     (llvm:build-ret (llvm-backend-builder backend)
		     (emit-value backend (verona:return-expression-value expression)))
     nil)
    ((typep expression 'verona:match-expression)
     (emit-match-value backend expression))
    ((typep expression 'verona:primitive-call)
     (emit-primitive backend expression))
    ((typep expression 'verona:external-call-expression)
     (let* ((external (verona:external-call-expression-external-function expression))
	    (function (backend-binding backend external))
	    (arguments (mapcar (lambda (argument) (emit-value backend argument))
			       (verona:semantic-call-arguments expression))))
       (if (typep (verona:semantic-external-function-declaration-result-type external)
		  'verona:void-type)
	   (progn
	     (llvm:build-call (llvm-backend-builder backend) function arguments)
	     (unit-value backend expression))
	   (llvm:build-call (llvm-backend-builder backend) function arguments "call"))))
    ((typep expression 'verona:semantic-call)
     (let ((callee (verona:semantic-call-callee expression)))
       (unless (and (typep callee 'verona:reference-expression)
                    (typep (verona:semantic-reference-binding callee)
                           '(or verona:function-declaration
                                verona:semantic-generic-implementation)))
         (backend-fail "ordinary call has no resolved concrete callable"))
       (llvm:build-call
        (llvm-backend-builder backend)
        (backend-binding backend (verona:semantic-reference-binding callee))
        (mapcar (lambda (argument) (emit-value backend argument))
                (verona:semantic-call-arguments expression))
        "call")))
    (t (backend-fail "expression ~S has no LLVM value lowering" expression))))
