(require :asdf)

(defparameter *output-path*
  (let ((arguments (uiop:command-line-arguments)))
    (unless (= (length arguments) 1)
      (error "usage: build-executable.lisp OUTPUT-PATH"))
    (first arguments)))

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (merge-pathnames "../" (uiop:pathname-directory-pathname script)))
       (cl-llvm-root (uiop:getenv "TERMIS_CL_LLVM")))
  (unless cl-llvm-root
    (error "TERMIS_CL_LLVM is required; build the executable from the Termis development shell"))
  (asdf:load-asd (merge-pathnames "llvm.asd" (uiop:ensure-directory-pathname cl-llvm-root)))
  (asdf:load-asd (merge-pathnames "termis.asd" root))
  (asdf:load-system :termis/compiler))

(defun termis-image-main ()
  (termis.compiler:main))

(sb-ext:save-lisp-and-die *output-path* :executable t :toplevel #'termis-image-main)
