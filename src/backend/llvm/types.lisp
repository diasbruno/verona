(in-package #:termis.backend.llvm)

(defun lower-type (backend type)
  "Return TYPE's LLVM type, memoized strictly in BACKEND."
  (or (gethash type (llvm-backend-types backend))
      (setf (gethash type (llvm-backend-types backend))
            (cond
              ((typep type 'termis:unit-type)
               (llvm:int-type (llvm-backend-pointer-width backend)
                              :context (llvm-backend-context backend)))
              ((typep type 'termis:void-type)
               (llvm:void-type :context (llvm-backend-context backend)))
              ((typep type 'termis:boolean-type)
               (llvm:int1-type :context (llvm-backend-context backend)))
              ((typep type 'termis:integer-type)
               (llvm:int-type (termis:integer-type-width type)
                              :context (llvm-backend-context backend)))
              ((typep type 'termis:float-type)
               (ecase (termis:float-type-width type)
                 (32 (llvm:float-type :context (llvm-backend-context backend)))
                 (64 (llvm:double-type :context (llvm-backend-context backend)))))
              ((typep type 'termis:pointer-type)
               ;; LLVM opaque pointers carry no pointee representation.  The
               ;; legacy C API still accepts an element type, so use i8 for
               ;; void* rather than attempting to form a pointer-to-void.
               (llvm:pointer-type
                (if (typep (termis:pointer-type-pointee type) 'termis:void-type)
                    (llvm:int-type 8 :context (llvm-backend-context backend))
                    (lower-type backend (termis:pointer-type-pointee type)))))
              ((typep type 'termis:function-type)
               (llvm:function-type
                (lower-type backend (termis:function-type-result type))
                (mapcar (lambda (parameter) (lower-type backend parameter))
                        (termis:function-type-parameters type))))
              ((typep type 'termis:product-type)
               ;; Product fields are already complete and acyclic by semantic
               ;; validation.  The named LLVM struct preserves nominal Termis
               ;; identity; setting its body is a one-shot layout operation.
               (let ((struct (llvm:struct-create-named
                              (llvm-backend-context backend)
                              (llvm-name (termis:defined-type-declaration type)))))
                 (llvm:struct-set-body
                  struct
                  (mapcar (lambda (field)
                            (lower-type backend (termis:product-field-type field)))
                          (termis:product-type-fields type)))
                 struct))
              ((typep type 'termis:sum-type)
               ;; The frontend knows only alternatives and payload types.  This
               ;; backend chooses a deterministic i32 tag followed by one
               ;; target-lowered payload aggregate per alternative.  Empty
               ;; payload aggregates represent zero-payload alternatives;
               ;; they are representation detail, never semantic unit values.
               (let ((struct (llvm:struct-create-named
                              (llvm-backend-context backend)
                              (llvm-name (termis:defined-type-declaration type)))))
                 (llvm:struct-set-body
                  struct
                  (cons (llvm:int-type 32 :context (llvm-backend-context backend))
                        (mapcar (lambda (alternative)
                                  (llvm:struct-type
                                   (mapcar (lambda (payload-type)
                                             (lower-type backend payload-type))
                                           (termis:sum-alternative-payload-types alternative))
                                   nil :context (llvm-backend-context backend)))
                                (termis:sum-type-alternatives type)))
                  nil)
                 struct))
              ;; Defined types have identity, but their field layout is not
              ;; part of the current semantic model.  An opaque named LLVM
              ;; struct preserves that identity for pointer uses.
              ((typep type 'termis:defined-type)
               (llvm:struct-create-named
                (llvm-backend-context backend)
                (llvm-name (termis:defined-type-declaration type))))
              (t (backend-fail "Termis type ~S has no LLVM representation" type))))))

(defun unit-value (backend expression)
  (declare (ignore expression))
  (llvm:const-int (lower-type backend (termis:type-context-unit-representation-type
                                       (backend-type-context backend)))
                  0))
