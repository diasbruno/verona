(require :asdf)

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (merge-pathnames "../" (uiop:pathname-directory-pathname script)))
       (cl-llvm-root (uiop:getenv "TERMIS_CL_LLVM")))
  ;; The LLVM integration runner intentionally uses the pinned Nix systems;
  ;; loading a user's Quicklisp can replace that source registry.
  (unless cl-llvm-root
    (error "TERMIS_CL_LLVM is set by nix develop and is required for LLVM tests"))
  (asdf:load-asd (merge-pathnames "llvm.asd"
                                 (uiop:ensure-directory-pathname cl-llvm-root)))
  (asdf:load-asd (merge-pathnames "termis.asd" root))
  (asdf:load-system :termis/compiler-tests)
  (unless (uiop:symbol-call :termis/tests :run-tests)
    (uiop:quit 1)))
