(in-package #:termis.backend.llvm)

(defun emit-place (backend expression)
  "Emit an LLVM address for a semantic Place expression."
  (cond
    ((typep expression 'termis:reference-expression)
     (let ((binding (termis:semantic-reference-binding expression)))
       (unless (or (typep binding 'termis:parameter-binding)
                   (typep binding 'termis:variable-declaration))
         (backend-fail "semantic binding ~S is not an LLVM place" binding))
       (backend-binding backend binding)))
    ((typep expression 'termis:dereference-expression)
     (emit-value backend (termis:dereference-expression-operand expression)))
    (t (backend-fail "expression ~S is not an LLVM place" expression))))

(defun emit-reference-value (backend expression)
  (let ((binding (termis:semantic-reference-binding expression)))
    (cond ((typep binding 'termis:function-declaration)
           (backend-binding backend binding))
	  ((typep binding 'termis:pattern-binding)
	   ;; Binding patterns introduce an SSA value, not a mutable place.
	   (backend-binding backend binding))
	  ((typep binding 'termis:let-binding)
	   ;; Immutable LET bindings are SSA values, never implicit stack storage.
	   (backend-binding backend binding))
          ((typep binding 'termis:constant-declaration)
           (llvm:build-load (llvm-backend-builder backend)
                            (backend-binding backend binding) "constant"
                            (lower-type backend (termis:expression-type expression))))
          ;; A reference remaining in a value position is a frontend
          ;; invariant violation: the resolver inserts LOAD explicitly.
          (t (backend-fail "unlowered value reference to ~S" binding)))))

(defun match-pattern-constant (backend pattern)
  (llvm:const-int (lower-type backend (termis:pattern-type pattern))
		  (let ((value (termis:literal-pattern-value pattern)))
		    (if (typep pattern 'termis:boolean-pattern)
			(if value 1 0)
			value))))

(defun emit-match-value (backend expression)
  "Lower a resolved match without re-evaluating its scrutinee.

The semantic checker guarantees exhaustiveness and compatible patterns.  The
only job here is to form the CFG and merge non-terminating case values."
  (let* ((builder (llvm-backend-builder backend))
	 (function (llvm:basic-block-parent (llvm:insertion-block builder)))
	 (scrutinee (emit-value backend (termis:match-expression-value expression)))
	 (cases (termis:match-expression-cases expression))
	 (case-blocks (mapcar (lambda (case)
				 (declare (ignore case))
				 (llvm:append-basic-block function "match.case"
							  :context (llvm-backend-context backend)))
			       cases))
	 (terminatingp (typep (termis:expression-type expression) 'termis:never-type))
	 (end-block (unless terminatingp
		      (llvm:append-basic-block function "match.end"
					       :context (llvm-backend-context backend))))
	 (default-block (llvm:append-basic-block function "match.unreachable"
					  :context (llvm-backend-context backend))))
    ;; Dispatch.  A wildcard/binding reaches its case directly; literal
    ;; patterns form a comparison chain.  The default is semantically dead.
    (loop for case in cases
	  for block in case-blocks
	  for remaining on cases
	  for pattern = (termis:match-case-pattern case)
	  do (if (or (typep pattern 'termis:wildcard-pattern)
		     (typep pattern 'termis:binding-pattern))
		 (llvm:build-br builder block)
		 (let ((next (if (cdr remaining)
				 (llvm:append-basic-block function "match.test"
							  :context (llvm-backend-context backend))
				 default-block)))
		   (llvm:build-cond-br
		    builder
		    (llvm:build-i-cmp builder := scrutinee
				      (match-pattern-constant backend pattern) "match.test")
		    block next)
		   (when (cdr remaining)
		     (llvm:position-builder-at-end builder next)))))
    (llvm:position-builder-at-end builder default-block)
    (llvm:build-unreachable builder)
    (let ((incoming '()))
      (loop for case in cases
	    for block in case-blocks
	    do (llvm:position-builder-at-end builder block)
	       (let ((pattern (termis:match-case-pattern case)))
		 (when (typep pattern 'termis:binding-pattern)
		   (setf (backend-binding backend (termis:binding-pattern-binding pattern)) scrutinee)))
	       (let ((branch (termis:match-case-expression case)))
		 (let ((value (emit-value backend branch))
		       (source (llvm:insertion-block builder)))
		   (unless (typep (termis:expression-type branch) 'termis:never-type)
		     (llvm:build-br builder end-block)
		     (push (cons value source) incoming)))))
      (unless terminatingp
	(llvm:position-builder-at-end builder end-block)
	(cond ((typep (termis:expression-type expression) 'termis:unit-type)
	       (llvm:const-int (lower-type backend (termis:expression-type expression)) 0))
	      ((null (cdr incoming)) (caar incoming))
	      (t (let ((phi (llvm:build-phi builder
					 (lower-type backend (termis:expression-type expression))
					 "match.result")))
		   (llvm:add-incoming phi
			      (coerce (mapcar #'car incoming) 'vector)
			      (coerce (mapcar #'cdr incoming) 'vector))
		   phi)))))))

(defun emit-value (backend expression)
  "Emit EXPRESSION's already-resolved LLVM value."
  (cond
    ((typep expression 'termis:unit-expression)
     (llvm:const-int (lower-type backend (termis:expression-type expression)) 0))
    ((typep expression 'termis:boolean-literal)
     (llvm:const-int (lower-type backend (termis:expression-type expression))
                     (if (termis:boolean-literal-value expression) 1 0)))
    ((typep expression 'termis:integer-literal)
     (llvm:const-int (lower-type backend (termis:expression-type expression))
                     (termis:integer-literal-value expression)))
    ((typep expression 'termis:float-literal)
     (llvm:const-real (lower-type backend (termis:expression-type expression))
                      (termis:float-literal-value expression)))
    ((typep expression 'termis:construct-expression)
     (let ((aggregate (llvm:undef (lower-type backend
                                             (termis:construct-expression-product-type expression)))))
       (loop for value-expression in (termis:construct-expression-fields expression)
             for index from 0
             do (setf aggregate
                      (llvm:build-insert-value (llvm-backend-builder backend)
                                               aggregate
                                               (emit-value backend value-expression)
                                               index "product.insert")))
       aggregate))
    ((typep expression 'termis:field-expression)
     (let ((field (termis:field-expression-field expression)))
       ;; The resolver records ProductField identity and index.  LLVM never
       ;; sees or looks up a source-level field name.
       (llvm:build-extract-value (llvm-backend-builder backend)
                                 (emit-value backend
                                             (termis:field-expression-value expression))
                                 (termis:product-field-index field)
                                 "product.field")))
    ((typep expression 'termis:reference-expression)
     (emit-reference-value backend expression))
    ((typep expression 'termis:load-expression)
     (llvm:build-load (llvm-backend-builder backend)
                      (emit-place backend (termis:load-expression-place expression))
                      "load"
                      (lower-type backend (termis:expression-type expression))))
    ((typep expression 'termis:address-expression)
     (emit-place backend (termis:address-expression-operand expression)))
    ((typep expression 'termis:dereference-expression)
     (emit-place backend expression))
    ((typep expression 'termis:store-expression)
     (llvm:build-store (llvm-backend-builder backend)
                       (emit-value backend (termis:assignment-expression-value expression))
                       (emit-place backend (termis:assignment-expression-target expression)))
     (llvm:const-int (lower-type backend (termis:expression-type expression)) 0))
    ((typep expression 'termis:sequence-expression)
     (let ((expressions (termis:sequence-expression-expressions expression)))
       (if expressions
           (loop for child in expressions
                 for value = (emit-value backend child)
                 finally (return value))
           (llvm:const-int (lower-type backend (termis:expression-type expression)) 0))))

    ((typep expression 'termis:let-expression)
     ;; Binding identity is the environment key, so nested shadowing needs no
     ;; LLVM-level name lookup or environment restoration.
     (dolist (binding (termis:let-expression-bindings expression))
       (setf (backend-binding backend binding)
             (emit-value backend (termis:let-binding-initializer binding))))
     (emit-value backend (termis:let-expression-body expression)))

    ((typep expression 'termis:return-expression)
     (llvm:build-ret (llvm-backend-builder backend)
		     (emit-value backend (termis:return-expression-value expression)))
     nil)
    ((typep expression 'termis:match-expression)
     (emit-match-value backend expression))
    ((typep expression 'termis:primitive-call)
     (emit-primitive backend expression))
    ((typep expression 'termis:semantic-call)
     (let ((callee (termis:semantic-call-callee expression)))
       (unless (and (typep callee 'termis:reference-expression)
                    (typep (termis:semantic-reference-binding callee)
                           'termis:function-declaration))
         (backend-fail "ordinary call has no resolved function declaration"))
       (llvm:build-call
        (llvm-backend-builder backend)
        (backend-binding backend (termis:semantic-reference-binding callee))
        (mapcar (lambda (argument) (emit-value backend argument))
                (termis:semantic-call-arguments expression))
        "call")))
    (t (backend-fail "expression ~S has no LLVM value lowering" expression))))
