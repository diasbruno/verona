(in-package #:termis.compiler)

;;; The driver owns orchestration only.  Frontend analysis remains in TERMIS,
;;; LLVM lowering remains in TERMIS.BACKEND.LLVM, and native commands live in
;;; the Toolchain protocol below.

(define-condition compiler-driver-error (error)
  ((message :initarg :message :reader compiler-driver-error-message))
  (:report (lambda (condition stream)
             (write-string (compiler-driver-error-message condition) stream))))

(define-condition unsupported-artifact (compiler-driver-error) ())
(define-condition unsupported-target (compiler-driver-error) ())
(define-condition llvm-verification-failure (compiler-driver-error) ())
(define-condition object-emission-failure (compiler-driver-error) ())
(define-condition invalid-entry-point (compiler-driver-error) ())

(define-condition toolchain-failure (compiler-driver-error)
  ((tool :initarg :tool :reader toolchain-failure-tool)
   (arguments :initarg :arguments :reader toolchain-failure-arguments)
   (exit-status :initarg :exit-status :reader toolchain-failure-exit-status)
   (stdout :initarg :stdout :reader toolchain-failure-stdout)
   (stderr :initarg :stderr :reader toolchain-failure-stderr)))
(define-condition linker-failure (toolchain-failure) ())
(define-condition archiver-failure (toolchain-failure) ())
(define-condition shared-library-link-failure (linker-failure) ())

(defclass compilation-target ()
  ((triple :initarg :triple :reader compilation-target-triple)
   (cpu :initarg :cpu :reader compilation-target-cpu)
   (features :initarg :features :reader compilation-target-features)
   (data-layout :initarg :data-layout :reader compilation-target-data-layout)
   (pointer-width :initarg :pointer-width :reader compilation-target-pointer-width)
   (object-format :initarg :object-format :reader compilation-target-object-format)
   (platform :initarg :platform :reader compilation-target-platform)))

(defun target-platform (triple)
  (cond ((search "darwin" triple :test #'char-equal) :darwin)
        ((or (search "linux" triple :test #'char-equal)
             (search "gnu" triple :test #'char-equal)) :linux)
        (t nil)))

(defun target-object-format (platform)
  (ecase platform (:darwin :macho) (:linux :elf)))

(defun resolve-compilation-target (&key triple (cpu "generic") (features ""))
  "Resolve NATIVE or an explicit triple once, before semantic analysis.

The LLVM target machine is the authority for data layout and pointer width;
neither value is taken from the Common Lisp host."
  (let* ((triple (or triple (native-target-triple)))
         (platform (target-platform triple)))
    (unless platform
      (error 'unsupported-target :message (format nil "unsupported target ~A" triple)))
    (handler-case
        (let* ((backend (termis.backend.llvm:make-llvm-backend
                         :module-name "termis.target-probe"
                         :target-configuration
                         (make-target-configuration :triple triple :cpu cpu :features features)))
               (width (llvm-backend-pointer-width backend)))
          (make-instance 'compilation-target :triple triple :cpu cpu :features features
                         :data-layout (llvm-backend-data-layout backend)
                         :pointer-width width :platform platform
                         :object-format (target-object-format platform)))
      (error (condition)
        (if (typep condition 'compiler-driver-error)
            (error condition)
            (error 'unsupported-target :message (format nil "cannot resolve target ~A: ~A"
                                                        triple condition)))))))

(defclass link-options ()
  ((libraries :initarg :libraries :initform '() :reader link-options-libraries)
   (library-search-paths :initarg :library-search-paths :initform '()
                         :reader link-options-library-search-paths)
   (frameworks :initarg :frameworks :initform '() :reader link-options-frameworks)))

(defun make-link-options (&key (libraries '()) (library-search-paths '()) (frameworks '()))
  (make-instance 'link-options :libraries libraries
               :library-search-paths (mapcar #'pathname library-search-paths)
               :frameworks frameworks))

(defclass artifact ()
  ((kind :initarg :kind :reader artifact-kind)
   (path :initarg :path :reader artifact-path)
   (target :initarg :target :reader artifact-target)))
(defclass object-artifact (artifact) ())
(defclass executable-artifact (artifact) ())
(defclass static-library-artifact (artifact) ())
(defclass shared-library-artifact (artifact) ())

(defclass toolchain () ())
(defgeneric toolchain-emit-object (toolchain backend output))
(defgeneric toolchain-link-executable (toolchain object output target options))
(defgeneric toolchain-archive-static-library (toolchain object output target))
(defgeneric toolchain-link-shared-library (toolchain object output target options))

(defclass native-toolchain (toolchain)
  ((compiler :initarg :compiler :reader native-toolchain-compiler)
   (archiver :initarg :archiver :reader native-toolchain-archiver)))

(defun make-native-toolchain (&key (compiler (or (uiop:getenv "TERMIS_LINKER") "clang"))
                                   (archiver (or (uiop:getenv "TERMIS_AR") "ar")))
  (make-instance 'native-toolchain :compiler compiler :archiver archiver))

(defun run-tool (failure-class tool arguments)
  (multiple-value-bind (stdout stderr status)
      (uiop:run-program (cons tool arguments) :output :string :error-output :string
                         :ignore-error-status t)
    (unless (zerop status)
      (error failure-class :message (format nil "~A failed" tool) :tool tool
             :arguments arguments :exit-status status :stdout stdout :stderr stderr))))

(defun native-link-arguments (object output target options)
  (when (and (link-options-frameworks options)
             (not (eq (compilation-target-platform target) :darwin)))
    (error 'unsupported-target :message "frameworks are supported only on Darwin targets"))
  (append (list (namestring (pathname object)))
          (loop for directory in (link-options-library-search-paths options)
                append (list "-L" (namestring directory)))
          (loop for library in (link-options-libraries options)
                collect (format nil "-l~A" library))
          (loop for framework in (link-options-frameworks options)
                append (list "-framework" framework))
          (list "-o" (namestring (pathname output)))))

(defmethod toolchain-emit-object ((toolchain native-toolchain) backend output)
  (declare (ignore toolchain))
  (handler-case (emit-object backend output)
    (error (condition)
      (error 'object-emission-failure :message (princ-to-string condition)))))

(defmethod toolchain-link-executable ((toolchain native-toolchain) object output target options)
  (run-tool 'linker-failure (native-toolchain-compiler toolchain)
            (native-link-arguments object output target options))
  output)

(defmethod toolchain-archive-static-library ((toolchain native-toolchain) object output target)
  (declare (ignore target))
  (run-tool 'archiver-failure (native-toolchain-archiver toolchain)
            (list "rcs" (namestring (pathname output)) (namestring (pathname object))))
  output)

(defmethod toolchain-link-shared-library ((toolchain native-toolchain) object output target options)
  (let ((arguments (native-link-arguments object output target options)))
    ;; The compiler driver supplies platform startup differences; LLVM only
    ;; produced a PIC object, and no linker knowledge leaks into semantics.
    (setf arguments
          (append (ecase (compilation-target-platform target)
                    (:darwin (list "-dynamiclib"))
                    (:linux (list "-shared")))
                  arguments))
    (run-tool 'shared-library-link-failure (native-toolchain-compiler toolchain) arguments))
  output)

(defclass compiler-driver ()
  ((search-paths :initarg :search-paths :reader compiler-driver-search-paths)
   (target :initarg :target :reader compiler-driver-target)
   (toolchain :initarg :toolchain :reader compiler-driver-toolchain)))

(defun make-compiler-driver (&key (search-paths '()) target (toolchain (make-native-toolchain)))
  (make-instance 'compiler-driver :search-paths (mapcar #'pathname search-paths)
               :target (or target (resolve-compilation-target)) :toolchain toolchain))

(defun artifact-class (kind)
  (ecase kind
    (:object 'object-artifact) (:executable 'executable-artifact)
    (:static-library 'static-library-artifact) (:shared-library 'shared-library-artifact)))

(defun default-output-path (root kind target)
  (let* ((path (pathname root)) (base (or (pathname-name path) "a.out"))
         (directory (make-pathname :name nil :type nil :defaults path)))
    (merge-pathnames
     (ecase kind
       (:object (format nil "~A.o" base))
       (:executable base)
       (:static-library (format nil "lib~A.a" base))
       (:shared-library (format nil "lib~A.~A" base
                                        (ecase (compilation-target-platform target)
                                          (:darwin "dylib") (:linux "so")))))
     directory)))

(defun temporary-object-path ()
  (merge-pathnames (format nil "termis-~A.o" (gensym "OBJECT-"))
                   (uiop:temporary-directory)))

(defun driver-target-configuration (target &optional relocation-model)
  (make-target-configuration :triple (compilation-target-triple target)
                             :cpu (compilation-target-cpu target)
                             :features (compilation-target-features target)
                             :relocation-model (or relocation-model :default)))

(defun compile-root (driver root &key (artifact-kind :executable) output
                                      (link-options (make-link-options)))
  "Compile ROOT and return an explicit native Artifact.

The frontend receives the already-resolved target, then the driver owns the
in-memory LLVM module, verification, object emission, and toolchain handoff."
  (check-type driver compiler-driver)
  (unless (member artifact-kind '(:object :executable :static-library :shared-library))
    (error 'unsupported-artifact :message (format nil "unsupported artifact ~S" artifact-kind)))
  (let* ((target (compiler-driver-target driver))
         (output (pathname (or output (default-output-path root artifact-kind target))))
         (frontend (make-compiler :search-paths (compiler-driver-search-paths driver)))
         (unit (termis:compile-file frontend root :target target
                                     :pointer-width (compilation-target-pointer-width target)))
         (program (compilation-unit-semantic-program unit))
         (picp (eq artifact-kind :shared-library))
         (backend (handler-case
                      (generate-llvm program
                                     :target-configuration
                                     (driver-target-configuration target (and picp :pic)))
                    (error (condition)
                      (error 'llvm-verification-failure :message (princ-to-string condition))))))
    (when (eq artifact-kind :executable)
      (handler-case (add-platform-entry-wrapper backend program)
        (error (condition)
          (error 'invalid-entry-point :message (princ-to-string condition)))))
    (hide-termis-symbols backend program)
    (handler-case (verify-llvm-module backend)
      (error (condition)
        (error 'llvm-verification-failure :message (princ-to-string condition))))
    (if (eq artifact-kind :object)
        (toolchain-emit-object (compiler-driver-toolchain driver) backend output)
        (let ((object (temporary-object-path)))
          (unwind-protect
               (progn
                 (toolchain-emit-object (compiler-driver-toolchain driver) backend object)
                 (ecase artifact-kind
                   (:executable (toolchain-link-executable (compiler-driver-toolchain driver)
                                                           object output target link-options))
                   (:static-library (toolchain-archive-static-library (compiler-driver-toolchain driver)
                                                                       object output target))
                   (:shared-library (toolchain-link-shared-library (compiler-driver-toolchain driver)
                                                                   object output target link-options))))
            (when (probe-file object) (delete-file object)))))
    (make-instance (artifact-class artifact-kind) :kind artifact-kind :path output :target target)))

(defun compile-file (driver pathname &rest arguments)
  (apply #'compile-root driver pathname arguments))
