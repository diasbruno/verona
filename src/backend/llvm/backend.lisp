(in-package #:termis.backend.llvm)

(defun backend-fail (control &rest arguments)
  (error 'llvm-backend-error :message (apply #'format nil control arguments)))

(defclass llvm-backend ()
  ((context :initarg :context :reader llvm-backend-context)
   (module :initarg :module :reader llvm-backend-module)
   (builder :initarg :builder :reader llvm-backend-builder)
   (target-machine :initarg :target-machine :reader llvm-backend-target-machine)
   (target-configuration :initarg :target-configuration
                         :reader llvm-backend-target-configuration)
   (target-triple :initarg :target-triple :reader llvm-backend-target-triple)
   (data-layout :initarg :data-layout :reader llvm-backend-data-layout)
   (target-data :initarg :target-data :reader llvm-backend-target-data)
   (pointer-width :initarg :pointer-width :reader llvm-backend-pointer-width)
   (type-context :initarg :type-context :accessor backend-type-context)
   ;; Semantic identities are keys.  LLVM values/types never escape into the
   ;; Termis semantic objects themselves.
   (bindings :initform (make-hash-table :test #'eq)
             :reader llvm-backend-bindings)
   (types :initform (make-hash-table :test #'eq)
          :reader llvm-backend-types)))

(defun make-llvm-backend (&key (module-name "termis")
                               (target-configuration (make-target-configuration))
                               (optimization-level :none))
  "Create a backend for an explicit LLVM target description.

DATA-LAYOUT is the authority for target representation.  In particular, the
pointer width is queried from LLVM target data; it is never inferred from the
Common Lisp implementation or the compiler host."
  (check-type target-configuration target-configuration)
  (let* ((context (llvm:global-context))
         (module (llvm:make-module module-name context))
         (builder (llvm:make-builder context))
         (target-machine (create-target-machine target-configuration optimization-level)))
    (setf (llvm:target module) (target-configuration-triple target-configuration))
    (multiple-value-bind (target-data data-layout)
        (attach-target-machine-layout module target-machine)
    (let ((pointer-width (* 8 (llvm:pointer-size target-data))))
      (unless (member pointer-width '(32 64))
        (backend-fail "LLVM target pointer width ~D is unsupported" pointer-width))
      (make-instance 'llvm-backend :context context :module module :builder builder
                      :target-machine target-machine
                      :target-configuration target-configuration
                      :target-triple (target-configuration-triple target-configuration)
                      :data-layout data-layout :target-data target-data
                      :pointer-width pointer-width)))))

(defun backend-binding (backend binding)
  (multiple-value-bind (value presentp)
      (gethash binding (llvm-backend-bindings backend))
    (if presentp value
        (backend-fail "no LLVM value registered for semantic binding ~S" binding))))

(defun (setf backend-binding) (value backend binding)
  (setf (gethash binding (llvm-backend-bindings backend)) value))

(defun llvm-name (binding)
  "Mangle a semantic name so no source spelling shares generated LLVM symbols."
  (with-output-to-string (stream)
    (write-string "__termis_" stream)
    (loop for character across (termis:termis-name-value (termis:semantic-binding-name binding))
          do (format stream "~6,'0X" (char-code character)))))

(defun semantic-source-binding (semantic-declaration)
  (termis:semantic-declaration-source-declaration semantic-declaration))
