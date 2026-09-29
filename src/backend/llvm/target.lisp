(in-package #:termis.backend.llvm)

;;; CL-LLVM currently wraps target data but not LLVM's target-machine C API.
;;; Keep the small missing portion here, beside the backend, rather than
;;; allowing machine details into the Termis semantic layers.

(define-condition llvm-backend-error (error)
  ((message :initarg :message :reader llvm-backend-error-message))
  (:report (lambda (condition stream)
             (write-string (llvm-backend-error-message condition) stream))))

(cffi:defctype llvm-target-machine :pointer)

(cffi:defcfun ("LLVMInitializeAArch64TargetInfo" %initialize-aarch64-target-info) :void)
(cffi:defcfun ("LLVMInitializeAArch64Target" %initialize-aarch64-target) :void)
(cffi:defcfun ("LLVMInitializeAArch64TargetMC" %initialize-aarch64-targetmc) :void)
(cffi:defcfun ("LLVMInitializeAArch64AsmPrinter" %initialize-aarch64-asmprinter) :void)
(cffi:defcfun ("LLVMInitializeX86TargetInfo" %initialize-x86-target-info) :void)
(cffi:defcfun ("LLVMInitializeX86Target" %initialize-x86-target) :void)
(cffi:defcfun ("LLVMInitializeX86TargetMC" %initialize-x86-targetmc) :void)
(cffi:defcfun ("LLVMInitializeX86AsmPrinter" %initialize-x86-asmprinter) :void)
(cffi:defcfun ("LLVMInitializeARMTargetInfo" %initialize-arm-target-info) :void)
(cffi:defcfun ("LLVMInitializeARMTarget" %initialize-arm-target) :void)
(cffi:defcfun ("LLVMInitializeARMTargetMC" %initialize-arm-targetmc) :void)
(cffi:defcfun ("LLVMInitializeARMAsmPrinter" %initialize-arm-asmprinter) :void)
(cffi:defcfun ("LLVMInitializeRISCVTargetInfo" %initialize-riscv-target-info) :void)
(cffi:defcfun ("LLVMInitializeRISCVTarget" %initialize-riscv-target) :void)
(cffi:defcfun ("LLVMInitializeRISCVTargetMC" %initialize-riscv-targetmc) :void)
(cffi:defcfun ("LLVMInitializeRISCVAsmPrinter" %initialize-riscv-asmprinter) :void)

(cffi:defcfun ("LLVMGetDefaultTargetTriple" %default-target-triple) :pointer)
(cffi:defcfun ("LLVMGetTargetFromTriple" %target-from-triple) :int
  (triple :string) (target :pointer) (message :pointer))
(cffi:defcfun ("LLVMCreateTargetMachine" %create-target-machine) llvm-target-machine
  (target :pointer) (triple :string) (cpu :string) (features :string)
  (optimization :int) (relocation :int) (code-model :int))
(cffi:defcfun ("LLVMDisposeTargetMachine" %dispose-target-machine) :void
  (machine llvm-target-machine))
(cffi:defcfun ("LLVMCreateTargetDataLayout" %create-target-data-layout) llvm::target-data
  (machine llvm-target-machine))
(cffi:defcfun ("LLVMTargetMachineEmitToFile" %target-machine-emit-to-file) :int
  (machine llvm-target-machine) (module llvm::module) (path :string)
  (file-type :int) (message :pointer))

(defclass target-configuration ()
  ((triple :initarg :triple :reader target-configuration-triple)
   (cpu :initarg :cpu :initform "generic" :reader target-configuration-cpu)
   (features :initarg :features :initform "" :reader target-configuration-features)
   (relocation-model :initarg :relocation-model :initform :default
                     :reader target-configuration-relocation-model)
   (code-model :initarg :code-model :initform :default
               :reader target-configuration-code-model)))

(define-condition target-configuration-error (llvm-backend-error) ())
(define-condition object-emission-error (llvm-backend-error)
  ((path :initarg :path :reader object-emission-error-path)))

(defun target-fail (control &rest arguments)
  (error 'target-configuration-error :message (apply #'format nil control arguments)))

(defun llvm-message-string (pointer)
  (unless (cffi:null-pointer-p pointer)
    (unwind-protect (cffi:foreign-string-to-lisp pointer)
      (llvm::dispose-message pointer))))

(defun native-target-triple ()
  "Return LLVM's native triple, independently of the Common Lisp host name."
  (let ((pointer (%default-target-triple)))
    (when (cffi:null-pointer-p pointer)
      (target-fail "LLVM could not determine the native target triple"))
    (llvm-message-string pointer)))

(defun make-target-configuration (&key (triple (native-target-triple))
                                        (cpu "generic") (features "")
                                        (relocation-model :default)
                                        (code-model :default))
  "Describe the machine LLVM should target; it is never inferred from Termis."
  (make-instance 'target-configuration :triple triple :cpu cpu :features features
                 :relocation-model relocation-model :code-model code-model))

(defun target-enum (value choices description)
  (or (cdr (assoc value choices))
      (target-fail "unsupported LLVM ~A ~S" description value)))

(defun target-optimization-enum (value)
  (target-enum value '((:none . 0) (:less . 1) (:default . 2) (:aggressive . 3))
               "optimization level"))

(defun target-relocation-enum (value)
  (target-enum value '((:default . 0) (:static . 1) (:pic . 2) (:dynamic-no-pic . 3))
               "relocation model"))

(defun target-code-model-enum (value)
  (target-enum value '((:default . 0) (:jit-default . 1) (:small . 2)
                       (:kernel . 3) (:medium . 4) (:large . 5))
               "code model"))

(defun target-c-api-name (triple)
  "Map an LLVM triple's architecture to the C API target component spelling."
  (let ((architecture (string-downcase (subseq triple 0 (or (position #\- triple)
                                                             (length triple))))))
    (cond ((member architecture '("aarch64" "arm64") :test #'string=) "AArch64")
          ((member architecture '("x86_64" "i386" "i486" "i586" "i686") :test #'string=) "X86")
          ((string= architecture "arm") "ARM")
          ((string= architecture "riscv64") "RISCV")
          (t (target-fail "no LLVM C API target initializer is known for ~A" triple)))))

(defun initialize-target-component (component)
  (ecase component
    (:aarch64 (%initialize-aarch64-target-info) (%initialize-aarch64-target)
              (%initialize-aarch64-targetmc) (%initialize-aarch64-asmprinter))
    (:x86 (%initialize-x86-target-info) (%initialize-x86-target)
          (%initialize-x86-targetmc) (%initialize-x86-asmprinter))
    (:arm (%initialize-arm-target-info) (%initialize-arm-target)
          (%initialize-arm-targetmc) (%initialize-arm-asmprinter))
    (:riscv (%initialize-riscv-target-info) (%initialize-riscv-target)
            (%initialize-riscv-targetmc) (%initialize-riscv-asmprinter))))

(defun target-component (triple)
  (let ((name (target-c-api-name triple)))
    (cond ((string= name "AArch64") :aarch64)
          ((string= name "X86") :x86)
          ((string= name "ARM") :arm)
          ((string= name "RISCV") :riscv))))

(defun initialize-target-machine-support (configuration)
  ;; LLVMInitializeNativeTarget is a C++ convenience macro, not an exported
  ;; symbol.  Call the real target-specific C entry points instead; this also
  ;; permits a configured target distinct from the compiler host.
  (handler-case
      (initialize-target-component
       (target-component (target-configuration-triple configuration)))
    (error (condition)
      (target-fail "LLVM could not initialize target ~A: ~A"
                   (target-configuration-triple configuration) condition))))

(defun create-target-machine (configuration optimization-level)
  (initialize-target-machine-support configuration)
  (cffi:with-foreign-objects ((target :pointer) (message :pointer))
    (let ((status (%target-from-triple (target-configuration-triple configuration)
                                       target message)))
      (unless (zerop status)
        (target-fail "LLVM cannot select target ~A: ~A"
                     (target-configuration-triple configuration)
                     (or (llvm-message-string (cffi:mem-ref message :pointer))
                         "unknown target error")))
      (let ((machine (%create-target-machine
                      (cffi:mem-ref target :pointer)
                      (target-configuration-triple configuration)
                      (target-configuration-cpu configuration)
                      (target-configuration-features configuration)
                      (target-optimization-enum optimization-level)
                      (target-relocation-enum
                       (target-configuration-relocation-model configuration))
                      (target-code-model-enum
                       (target-configuration-code-model configuration)))))
        (when (cffi:null-pointer-p machine)
          (target-fail "LLVM could not create a target machine for ~A"
                       (target-configuration-triple configuration)))
        machine))))

(defun target-machine-data-layout (machine)
  "Return the target data created by MACHINE and its canonical layout string."
  (let ((target-data (%create-target-data-layout machine)))
    (when (cffi:null-pointer-p target-data)
      (target-fail "LLVM could not create target data layout"))
    ;; CL-LLVM's STRING-REPRESENTATION wrapper is unsafe in its LLVM 23 fork;
    ;; LLVMGetDataLayout is a borrowed string and is safe to copy here.
    (values target-data nil)))

(defun attach-target-machine-layout (module machine)
  (multiple-value-bind (target-data ignored-layout) (target-machine-data-layout machine)
    (declare (ignore ignored-layout))
    (llvm:set-module-data-layout module target-data)
    (values target-data (llvm:data-layout module))))

(defun emit-module-object (machine module path)
  (let ((native-path (namestring (pathname path))))
    (cffi:with-foreign-objects ((message :pointer))
      ;; LLVMCodeGenFileType: 0 = assembly, 1 = object.
      (let ((status (%target-machine-emit-to-file machine module native-path 1 message)))
        (unless (zerop status)
          (error 'object-emission-error :path path
                 :message (format nil "LLVM could not emit object ~A: ~A" native-path
                                  (or (llvm-message-string
                                       (cffi:mem-ref message :pointer))
                                      "unknown object-emission error"))))
        path))))
