(require :asdf)

(let* ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
       (script (or *load-truename* *compile-file-truename*))
       (root (merge-pathnames "../" (uiop:pathname-directory-pathname script))))
  (unless (probe-file quicklisp)
    (error "Quicklisp is required; expected its setup file at ~A" quicklisp))
  (load quicklisp)
  (uiop:symbol-call :ql :quickload :fiveam)
  (asdf:load-asd (merge-pathnames "termis.asd" root))
  (asdf:load-system :termis/tests)
  (unless (uiop:symbol-call :termis/tests :run-tests)
    (uiop:quit 1)))

