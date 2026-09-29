(in-package #:termis.backend.llvm)

(defclass linker-configuration ()
  ((executable :initarg :executable :initform (or (uiop:getenv "TERMIS_LINKER") "clang")
               :reader linker-configuration-executable)
   (arguments :initarg :arguments :initform '() :reader linker-configuration-arguments)
   (libraries :initarg :libraries :initform '() :reader linker-configuration-libraries)
   (library-paths :initarg :library-paths :initform '() :reader linker-configuration-library-paths)
   (framework-paths :initarg :framework-paths :initform '()
                    :reader linker-configuration-framework-paths)))

(defun make-linker-configuration (&key executable (arguments '()) (libraries '())
                                       (library-paths '()) (framework-paths '()))
  "Configure the system compiler driver used for platform startup and linking."
  (make-instance 'linker-configuration :executable (or executable
                                                        (uiop:getenv "TERMIS_LINKER")
                                                        "clang")
                 :arguments arguments :libraries libraries
                 :library-paths library-paths :framework-paths framework-paths))

(defclass codegen-configuration ()
  ((target :initarg :target :reader codegen-configuration-target)
   (optimization-level :initarg :optimization-level :initform :none
                       :reader codegen-configuration-optimization-level)
   (relocation-model :initarg :relocation-model :reader codegen-configuration-relocation-model)
   (code-model :initarg :code-model :reader codegen-configuration-code-model)
   (output-kind :initarg :output-kind :initform :object :reader codegen-configuration-output-kind)
   (linker :initarg :linker :reader codegen-configuration-linker)))

(defun make-codegen-configuration (&key (target (make-target-configuration))
                                        (optimization-level :none)
                                        relocation-model code-model (output-kind :object)
                                        (linker (make-linker-configuration)))
  "Keep target and output choices outside the semantic Termis program."
  (check-type target target-configuration)
  (unless (member output-kind '(:llvm-ir :object :executable))
    (target-fail "unsupported output kind ~S" output-kind))
  (let* ((effective-relocation (or relocation-model
                                   (target-configuration-relocation-model target)))
         (effective-code-model (or code-model (target-configuration-code-model target)))
         ;; CODEGEN options are authoritative when supplied, so create the
         ;; target machine from their complete, explicit target description.
         (effective-target (if (or relocation-model code-model)
                               (make-target-configuration
                                :triple (target-configuration-triple target)
                                :cpu (target-configuration-cpu target)
                                :features (target-configuration-features target)
                                :relocation-model effective-relocation
                                :code-model effective-code-model)
                               target)))
    (make-instance 'codegen-configuration
                   :target effective-target :optimization-level optimization-level
                   :relocation-model effective-relocation :code-model effective-code-model
                   :output-kind output-kind :linker linker)))

(define-condition entry-point-error (llvm-backend-error) ())
(define-condition linker-error (error)
  ((command :initarg :command :reader linker-error-command)
   (exit-status :initarg :exit-status :reader linker-error-exit-status)
   (stdout :initarg :stdout :reader linker-error-stdout)
   (stderr :initarg :stderr :reader linker-error-stderr))
  (:report (lambda (condition stream)
             (format stream "linker failed (~D): ~{~A~^ ~}~@[~%~A~]"
                     (linker-error-exit-status condition)
                     (linker-error-command condition)
                     (linker-error-stderr condition)))))

(defun entry-fail (control &rest arguments)
  (error 'entry-point-error :message (apply #'format nil control arguments)))

(defun source-name= (binding name)
  (string= (termis:termis-name-value (termis:semantic-binding-name binding)) name))

(defun find-entry-function (program)
  (find-if (lambda (declaration)
             (and (typep declaration 'termis:semantic-function-declaration)
                  (source-name= (semantic-source-binding declaration) "main")))
           (semantic-declarations program)))

(defun i64-type-p (type)
  (and (typep type 'termis:integer-type)
       (termis:integer-type-signed type)
       (= 64 (termis:integer-type-width type))))

(defun validate-executable-entry-point (program)
  "Enforce the Termis executable contract: main : () -> i64."
  (let ((entry (find-entry-function program)))
    (unless entry
      (entry-fail "executable requires a Termis function named main"))
    (unless (null (termis:semantic-function-declaration-parameters entry))
      (entry-fail "Termis main must not have parameters"))
    (unless (i64-type-p (termis:semantic-function-declaration-return-type entry))
      (entry-fail "Termis main must return i64"))
    entry))

(defun add-platform-entry-wrapper (backend program)
  "Add C-compatible main without making the C ABI a Termis language rule."
  (let* ((entry (validate-executable-entry-point program))
         (termis-main (backend-binding backend entry))
         (context (llvm-backend-context backend))
         (platform-main (llvm:add-function
                         (llvm-backend-module backend) "main"
                         (llvm:function-type (llvm:int32-type :context context) '())))
         (block (llvm:append-basic-block platform-main "entry" :context context)))
    (llvm:position-builder-at-end (llvm-backend-builder backend) block)
    (let ((result (llvm:build-call (llvm-backend-builder backend) termis-main '() "termis.exit")))
      ;; Process exit semantics use the platform C main result.  LLVM applies
      ;; the defined i64-to-i32 ABI-boundary conversion only in this wrapper.
      (llvm:build-ret (llvm-backend-builder backend)
                      (llvm:build-trunc (llvm-backend-builder backend) result
                                        (llvm:int32-type :context context) "exit.status")))
    platform-main))

(defun emit-object (backend output)
  "Emit BACKEND's already-verified module as a native object file."
  (check-type backend llvm-backend)
  (verify-llvm-module backend)
  (emit-module-object (llvm-backend-target-machine backend)
                      (llvm-backend-module backend) output))

(defun linker-command (object output configuration)
  (let ((linker (codegen-configuration-linker configuration)))
    (append (list (linker-configuration-executable linker) (namestring (pathname object)))
            (loop for directory in (linker-configuration-library-paths linker)
                  append (list "-L" (namestring (pathname directory))))
            (loop for directory in (linker-configuration-framework-paths linker)
                  append (list "-F" (namestring (pathname directory))))
            (loop for library in (linker-configuration-libraries linker)
                  collect (format nil "-l~A" library))
            (linker-configuration-arguments linker)
            (list "-o" (namestring (pathname output))))))

(defun link-executable (object output configuration)
  (let ((command (linker-command object output configuration)))
    (multiple-value-bind (stdout stderr status)
        (uiop:run-program command :output :string :error-output :string
                           :ignore-error-status t)
      (unless (zerop status)
        (error 'linker-error :command command :exit-status status
               :stdout stdout :stderr stderr))
      output)))

(defun temporary-object-path ()
  (merge-pathnames (format nil "termis-~A.o" (gensym "OBJECT-"))
                   (uiop:temporary-directory)))

(defun build-executable (program output &key (configuration (make-codegen-configuration :output-kind :executable)))
  "Lower PROGRAM, generate a private object, link it, then remove that object."
  (check-type program termis:semantic-program)
  (let* ((object (temporary-object-path))
         (backend (generate-llvm program
                                 :target-configuration (codegen-configuration-target configuration)
                                 :optimization-level (codegen-configuration-optimization-level configuration))))
    (unwind-protect
         (progn
           (add-platform-entry-wrapper backend program)
           (verify-llvm-module backend)
           (emit-object backend object)
           (link-executable object output configuration))
      (when (probe-file object)
        (delete-file object)))))

(defun emit-output (program output &key (configuration (make-codegen-configuration)))
  "Produce LLVM IR, an object, or an executable according to CONFIGURATION."
  (ecase (codegen-configuration-output-kind configuration)
    (:llvm-ir
     (let ((backend (generate-llvm program
                                   :target-configuration (codegen-configuration-target configuration)
                                   :optimization-level (codegen-configuration-optimization-level configuration))))
       (with-open-file (stream output :direction :output :if-exists :supersede)
         (write-string (print-llvm-module backend) stream))
       output))
    (:object
     (let ((backend (generate-llvm program
                                   :target-configuration (codegen-configuration-target configuration)
                                   :optimization-level (codegen-configuration-optimization-level configuration))))
       (emit-object backend output)))
    (:executable (build-executable program output :configuration configuration))))
