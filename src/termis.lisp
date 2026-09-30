(require :asdf)

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (merge-pathnames "../" (uiop:pathname-directory-pathname script)))
       (cl-llvm-root (uiop:getenv "TERMIS_CL_LLVM")))
  (unless cl-llvm-root
    (error "TERMIS_CL_LLVM is required; run Termis from its development shell"))
  (asdf:load-asd (merge-pathnames "llvm.asd" (uiop:ensure-directory-pathname cl-llvm-root)))
  (asdf:load-asd (merge-pathnames "termis.asd" root))
  (asdf:load-system :termis/compiler)
  ;; Use SYMBOL-CALL so this source remains readable before the package is
  ;; defined by ASDF loading the compiler system.
  (uiop:symbol-call :termis.compiler :main))
