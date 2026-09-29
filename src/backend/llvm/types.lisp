(in-package #:termis.backend.llvm)

(defun lower-type (backend type)
  "Return TYPE's LLVM type, memoized strictly in BACKEND."
  (or (gethash type (llvm-backend-types backend))
      (setf (gethash type (llvm-backend-types backend))
            (cond
              ((typep type 'termis:unit-type)
               (llvm:int-type (llvm-backend-pointer-width backend)
                              :context (llvm-backend-context backend)))
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
               (llvm:pointer-type (lower-type backend (termis:pointer-type-pointee type))))
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
