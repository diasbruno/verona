(asdf:defsystem #:termis
  :description "The Termis compiler front-end foundation"
  :serial t
  :components ((:file "src/package")
               (:file "src/source")
               (:file "src/syntax")
               (:file "src/reader")
               (:file "src/compiler")))

(asdf:defsystem #:termis/tests
  :depends-on (#:termis #:fiveam)
  :serial t
  :components ((:file "tests/foundation")))
