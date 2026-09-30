(require :asdf)

(let* ((quicklisp (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
       (script (or *load-truename* *compile-file-truename*))
       (root (merge-pathnames "../" (uiop:pathname-directory-pathname script))))
  (unless (probe-file quicklisp)
    (error "Quicklisp is required; expected its setup file at ~A" quicklisp))
  (load quicklisp)
  (uiop:symbol-call :ql :quickload :fiveam)
  (asdf:load-asd (merge-pathnames "verona.asd" root))
  (asdf:load-system :verona/tests)
  (unless (uiop:symbol-call :verona/tests :run-tests)
    (uiop:quit 1)))

