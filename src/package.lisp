(defpackage #:termis
  (:use #:cl)
  (:shadow #:compile-file)
  (:export
   #:compiler
   #:make-compiler
   #:source
   #:make-source
   #:source-from-file
   #:source-name
   #:source-contents
   #:source-location
   #:source-location-offset
   #:source-location-line
   #:source-location-column
   #:syntax
   #:syntax-datum
   #:syntax-source
   #:syntax-start
   #:syntax-end
   #:termis-symbol
   #:termis-symbol-name
   #:unit-literal
   #:unit-literal-p
   #:read-source
   #:module
   #:module-source
   #:module-forms
   #:compile-string
   #:compile-file
   #:source-error
   #:termis-read-error
   #:termis-read-error-source
   #:termis-read-error-location))
