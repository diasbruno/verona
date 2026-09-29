(in-package #:termis)

(defclass compiler ()
  ((search-paths :initarg :search-paths :initform '() :reader compiler-search-paths)))

(defun make-compiler (&key (search-paths '()))
  (make-instance 'compiler :search-paths (mapcar #'pathname search-paths)))

(defclass compilation-unit ()
  ((source :initarg :source
           :reader compilation-unit-source
           :reader module-source)
   ;; FORMS preserves the source program independently of declaration
   ;; discovery.  Later phases may decide what to do with non-definition
   ;; top-level forms without losing the original syntax.
   (forms :initarg :forms
          :reader compilation-unit-forms
          :reader module-forms)
   (declarations :initform '()
                 :accessor compilation-unit-declarations
                 :accessor module-declarations)
   ;; This alist is deliberately separate from DECLARATIONS: registration
   ;; order is meaningful, while lookup needs a single namespace.
   (namespace :initform '()
              :accessor compilation-unit-namespace
              :accessor module-namespace)
   (environment :initarg :environment
                :reader compilation-unit-environment
                :reader compilation-unit-compile-time-environment
                :reader module-environment)
   ;; Populated only after declaration collection.  This is intentionally not
   ;; the compile-time ENVIRONMENT above: it contains resolved compiler
   ;; entities rather than evaluator bindings or macros.
   (semantic-program :initform nil
                     :accessor compilation-unit-semantic-program)))

;; MODULE was the name used by the preceding foundation stages.  Keep the
;; legacy class as a compatibility subclass while new callers use
;; COMPILATION-UNIT, which does not prematurely imply package or import
;; semantics.
(defclass module (compilation-unit)
  ((name :initarg :name :reader module-name)
   (pathname :initarg :pathname :initform nil :reader module-pathname)
   (imports :initform '() :accessor module-imports)
   (import-table :initform '() :accessor module-import-table)
   (export-names :initform '() :accessor module-export-names)
   (exports :initform '() :accessor module-exports)
   (identity-explicit-p :initarg :identity-explicit-p :initform t
                        :reader module-identity-explicit-p)))

(defclass import ()
  ((module :initarg :module :reader import-module)
   (alias :initarg :alias :initform nil :reader import-alias)
   (source :initarg :source :reader import-source)))

(defclass module-loader ()
  ((search-paths :initarg :search-paths :reader module-loader-search-paths)
   (loaded-modules :initform '() :accessor module-loader-loaded-modules)
   (loading-stack :initform '() :accessor module-loader-loading-stack)))

(defclass module-graph ()
  ((modules :initarg :modules :reader module-graph-modules)
   (edges :initarg :edges :reader module-graph-edges)))

(define-condition module-error (error)
  ((module :initarg :module :initform nil :reader module-error-module)
   (source :initarg :source :initform nil :reader module-error-source)))
(define-condition module-not-found (module-error) ())
(define-condition duplicate-module (module-error) ())
(define-condition circular-module-dependency (module-error)
  ((cycle :initarg :cycle :reader circular-module-dependency-cycle)))
(define-condition duplicate-import-alias (module-error)
  ((alias :initarg :alias :reader duplicate-import-alias-alias)))
(define-condition unknown-export (module-error)
  ((name :initarg :name :reader unknown-export-name)))

(defun parse-module-name (syntax)
  "Turn import syntax into a ModuleName without making ordinary names modules."
  (let ((datum (syntax-datum syntax)))
    (unless (termis-name-p datum)
      (error 'module-error :source syntax :module nil))
    (let ((text (termis-name-value datum)))
      (when (or (string= text "")
                (some (lambda (component) (string= component ""))
                      (uiop:split-string text :separator ".")))
        (error 'module-error :source syntax :module nil))
      (apply #'make-module-name
             (mapcar #'make-termis-name (uiop:split-string text :separator "."))))))

(defun module-name-from-pathname (pathname)
  (let ((name (pathname-name (pathname pathname))))
    (unless name (error 'module-error :module nil))
    (parse-module-name
     (make-syntax (make-termis-name name)
                  (make-source (namestring pathname) "")
                  (make-source-location) (make-source-location)))))

(defun import-qualifier-name (import)
  (or (import-alias import)
      (make-termis-name (module-name-string (module-name (import-module import))))))

(defun module-find-import (module qualifier)
  (find qualifier (module-imports module) :key #'import-qualifier-name
        :test #'termis-name=))

(defun module-find-export (module name)
  (cdr (assoc name (module-exports module) :test #'termis-name=)))

(defclass declaration (semantic-binding)
  (;; SOURCE is the original complete top-level form, not a resolved compiler
   ;; type or value.  All declaration-specific content remains source-aware.
   (source :initarg :source :reader declaration-source)
   ;; The primitive definition syntax produced by top-level expansion.  It is
   ;; intentionally distinct from SOURCE when a macro produced the definition.
   (expanded-syntax :initarg :expanded-syntax :reader declaration-expanded-syntax)
   (module :initarg :module
           :reader declaration-module
           :reader declaration-compilation-unit)))

(defclass type-declaration (declaration)
  ((body :initarg :body :reader type-declaration-body)))

(defclass function-declaration (declaration)
  ((parameters :initarg :parameters :reader function-declaration-parameters)
   (return-type :initarg :return-type :reader function-declaration-return-type)
   (body :initarg :body :reader function-declaration-body)))

(defclass macro-declaration (declaration)
  ((parameters :initarg :parameters :reader macro-declaration-parameters)
   (body :initarg :body :reader macro-declaration-body)))

(defclass constant-declaration (declaration)
  ((type :initarg :type :reader constant-declaration-type)
   (value :initarg :value :reader constant-declaration-value)))

(defclass variable-declaration (declaration)
  ((type :initarg :type :reader variable-declaration-type)
   (initializer :initarg :initializer :reader variable-declaration-initializer)))

(defclass generic-declaration (declaration)
  ((parameters :initarg :parameters :reader generic-declaration-parameters)
   (arity :initarg :arity :reader generic-declaration-arity)))

(defclass implementation-declaration (declaration)
  ((generic-name :initarg :generic-name :reader implementation-declaration-generic-name)
   (parameters :initarg :parameters :reader implementation-declaration-parameters)
   (return-type :initarg :return-type :reader implementation-declaration-return-type)
   (body :initarg :body :reader implementation-declaration-body)))

(define-condition definition-error (error)
  ((syntax :initarg :syntax :reader definition-error-syntax)
   (message :initarg :message :reader definition-error-message))
  (:report (lambda (condition stream)
             (let* ((syntax (definition-error-syntax condition))
                    (location (syntax-start syntax)))
               (format stream "~A:~D:~D: ~A"
                       (source-name (syntax-source syntax))
                       (source-location-line location)
                       (source-location-column location)
                       (definition-error-message condition))))))

(define-condition duplicate-declaration-error (definition-error)
  ((name :initarg :name :reader duplicate-declaration-error-name)
   (existing :initarg :existing :reader duplicate-declaration-error-existing))
  (:report (lambda (condition stream)
             (let* ((syntax (definition-error-syntax condition))
                    (location (syntax-start syntax))
                    (existing-source
                      (declaration-source
                       (duplicate-declaration-error-existing condition)))
                    (existing-location (syntax-start existing-source)))
               (format stream "~A:~D:~D: duplicate definition `~A`~%~%previous definition:~%~A:~D:~D"
                       (source-name (syntax-source syntax))
                       (source-location-line location)
                       (source-location-column location)
                       (termis-name-value (duplicate-declaration-error-name condition))
                       (source-name (syntax-source existing-source))
                       (source-location-line existing-location)
                       (source-location-column existing-location))))))

(define-condition non-definition-top-level-error (definition-error) ())

(defstruct (top-level-expansion-result
            (:constructor make-top-level-expansion-result (definitions)))
  "The unambiguous, internal result of expanding one top-level source form.

DEFINITIONS is a list of primitive definition syntax objects.  A distinct
result object avoids treating an ordinary list expression as several forms."
  (definitions '() :type list))

(defparameter +definition-form-names+
  '("%type" "%function" "%macro" "%constant" "%variable" "%generic" "%implementation"))

(defun definition-head-name (syntax)
  "Return SYNTAX's definition-form name, or NIL when it is not one."
  (let ((datum (syntax-datum syntax)))
    (when (termis-list-p datum)
      (let ((elements (termis-list-elements datum)))
        (when elements
          (let ((head (syntax-datum (first elements))))
            (and (termis-name-p head)
                 (find (termis-name-value head) +definition-form-names+
                       :test #'string=))))))))

(defun definition-form-p (syntax)
  "Whether SYNTAX has one of the primitive top-level definition heads."
  (check-type syntax syntax)
  (not (null (definition-head-name syntax))))

(defun definition-fail (syntax control &rest arguments)
  (error 'definition-error
         :syntax syntax
         :message (apply #'format nil control arguments)))

(defun definition-name (syntax name-syntax)
  (let ((name (syntax-datum name-syntax)))
    (unless (termis-name-p name)
      (definition-fail syntax "definition name must be a Termis name"))
    name))

(defun definition-elements (syntax expected-name minimum-arguments)
  "Return definition arguments after validating the primitive form's arity."
  (let ((arguments (rest (termis-list-elements (syntax-datum syntax)))))
    (when (< (length arguments) minimum-arguments)
      (definition-fail syntax "%~A requires at least ~D argument~:P"
                       expected-name minimum-arguments))
    arguments))

(defun find-declaration (unit name)
  "Look up NAME in MODULE's declaration namespace.

The primary value is the declaration (or NIL); the secondary value says
whether the name was present, so a future NIL-valued representation remains
unambiguous."
  (check-type unit compilation-unit)
  (check-type name termis-name)
  (let ((binding (assoc name (compilation-unit-namespace unit) :test #'termis-name=)))
    (values (cdr binding) (not (null binding)))))

(defun module-lookup (module name)
  "Compatibility name for FIND-DECLARATION."
  (find-declaration module name))

(defun unit-declarations (unit)
  "Return UNIT's declarations in source discovery order."
  (check-type unit compilation-unit)
  (compilation-unit-declarations unit))

(defun register-declaration (unit declaration)
  ;; Implementations belong to a generic's implementation table, not the
  ;; module's single name namespace.  Their declaration name is retained for
  ;; diagnostics only.
  (when (typep declaration 'implementation-declaration)
    (setf (compilation-unit-declarations unit)
          (append (compilation-unit-declarations unit) (list declaration)))
    (return-from register-declaration declaration))
  (let ((name (declaration-name declaration)))
    (multiple-value-bind (existing foundp) (find-declaration unit name)
      (when foundp
        (error 'duplicate-declaration-error
               :syntax (declaration-source declaration)
               :name name
               :existing existing))
      ;; APPEND preserves program order; the namespace is an implementation
      ;; detail optimized for the tiny front end, not the ordered API.
      (setf (compilation-unit-declarations unit)
            (append (compilation-unit-declarations unit) (list declaration)))
      (push (cons name declaration) (compilation-unit-namespace unit))
      declaration)))

(defun macro-parameter-names (definition parameters)
  "Extract macro parameter names while retaining the parameter syntax itself."
  (unless (termis-list-p (syntax-datum parameters))
    (definition-fail definition "%macro parameters must be a list"))
  (mapcar (lambda (parameter)
            (let ((name (syntax-datum parameter)))
              (unless (termis-name-p name)
                (definition-fail definition "%macro parameters must be Termis names"))
              name))
          (termis-list-elements (syntax-datum parameters))))

(defun generic-parameter-names (definition parameters)
  "Extract the untyped parameter names that establish a generic's arity."
  (unless (termis-list-p (syntax-datum parameters))
    (definition-fail definition "%generic parameters must be a list"))
  (mapcar (lambda (parameter)
            (let ((name (syntax-datum parameter)))
              (unless (termis-name-p name)
                (definition-fail definition "%generic parameters must be Termis names"))
              name))
          (termis-list-elements (syntax-datum parameters))))

(defun declaration-macro (definition parameter-names body environment)
  "Construct the compile-time macro represented by a %MACRO declaration.

Macro bodies are evaluated only when the macro is invoked.  Discovery itself
never evaluates a declaration body."
  (make-termis-macro
   (lambda (&rest arguments)
     (unless (= (length arguments) (length parameter-names))
       (definition-fail definition
                        "%macro expected ~D argument~:P, received ~D"
                        (length parameter-names) (length arguments)))
     (let ((macro-environment (environment-child environment)))
       (loop for name in parameter-names
             for argument in arguments
             do (environment-bind macro-environment name argument))
       (let ((result (evaluate body macro-environment)))
         (unless (or (typep result 'syntax)
                     (typep result 'top-level-expansion-result))
           (definition-fail definition
                            "%macro body must evaluate to syntax or top-level definitions"))
         result)))))

(defun process-definition (context unit source &optional (expanded-syntax source))
  "Turn EXPANDED-SYNTAX into a declaration, retaining its original SOURCE.

CONTEXT is the compile-time evaluator environment.  The evaluator only
expands syntax; this processor is the boundary that creates compiler objects."
  (check-type context environment)
  (check-type unit compilation-unit)
  (check-type source syntax)
  (check-type expanded-syntax syntax)
  (let ((head (definition-head-name expanded-syntax)))
    (unless head
      (error 'non-definition-top-level-error
             :syntax source
             :message "top-level expansion must produce a definition"))
    (flet ((make-declaration (class name &rest initargs)
             (register-declaration
              unit
              (apply #'make-instance class
                     :name name :source source :expanded-syntax expanded-syntax
                     :module unit initargs))))
      (cond
            ((string= head "%type")
             (let* ((arguments (definition-elements expanded-syntax "type" 2))
                    (name (definition-name expanded-syntax (first arguments))))
               (make-declaration 'type-declaration name :body (rest arguments))))
            ((string= head "%function")
             (let ((arguments (definition-elements expanded-syntax "function" 4)))
              (unless (= (length arguments) 4)
                 (definition-fail expanded-syntax "%function requires a name, parameters, return type, and body"))
               (make-declaration 'function-declaration
                                 (definition-name expanded-syntax (first arguments))
                                 :parameters (second arguments)
                                 :return-type (third arguments)
                                 :body (fourth arguments))))
            ((string= head "%macro")
             (let ((arguments (definition-elements expanded-syntax "macro" 3)))
              (unless (= (length arguments) 3)
                 (definition-fail expanded-syntax "%macro requires a name, parameters, and body"))
               (let* ((name (definition-name expanded-syntax (first arguments)))
                      (parameters (second arguments))
                      (body (third arguments))
                      (parameter-names (macro-parameter-names expanded-syntax parameters))
                      (declaration (make-declaration 'macro-declaration name
                                                     :parameters parameters :body body)))
                 ;; Bind only after successful registration so a duplicate
                 ;; definition cannot overwrite the existing macro.
                 (environment-bind context name
                                   (declaration-macro expanded-syntax parameter-names body context))
                 declaration)))
            ((string= head "%constant")
             (let ((arguments (definition-elements expanded-syntax "constant" 3)))
              (unless (= (length arguments) 3)
                 (definition-fail expanded-syntax "%constant requires a name, type, and value"))
               (make-declaration 'constant-declaration
                                 (definition-name expanded-syntax (first arguments))
                                 :type (second arguments) :value (third arguments))))
            ((string= head "%variable")
             (let ((arguments (definition-elements expanded-syntax "variable" 3)))
              (unless (= (length arguments) 3)
                 (definition-fail expanded-syntax "%variable requires a name, type, and initializer"))
               (make-declaration 'variable-declaration
                                 (definition-name expanded-syntax (first arguments))
                                 :type (second arguments) :initializer (third arguments))))
            ((string= head "%generic")
             (let ((arguments (definition-elements expanded-syntax "generic" 2)))
               (unless (= (length arguments) 2)
                 (definition-fail expanded-syntax "%generic requires a name and parameter list"))
               (let* ((name (definition-name expanded-syntax (first arguments)))
                      (parameters (second arguments))
                      (names (generic-parameter-names expanded-syntax parameters)))
                 (make-declaration 'generic-declaration name
                                   :parameters names :arity (length names)))))
            ((string= head "%implementation")
             (let ((arguments (definition-elements expanded-syntax "implementation" 4)))
               (unless (= (length arguments) 4)
                 (definition-fail expanded-syntax "%implementation requires a generic name, parameters, return type, and body"))
               (let ((target (syntax-datum (first arguments))))
                 (unless (or (termis-name-p target) (qualified-name-p target))
                   (definition-fail expanded-syntax "implementation target must be a name"))
                 ;; Implementations do not occupy the ordinary declaration
                 ;; namespace.  Keep a local Name for diagnostics while
                 ;; retaining a structured QualifiedName target for the
                 ;; ownership validation in semantic resolution.
                 (make-declaration 'implementation-declaration
                                   (if (qualified-name-p target)
                                       (qualified-name-name target) target)
                                   :generic-name target
                                   :parameters (second arguments) :return-type (third arguments)
                                   :body (fourth arguments)))))))))

(defun expand-top-level (syntax environment)
  "Expand SYNTAX into a TOP-LEVEL-EXPANSION-RESULT.

Unlike ordinary EXPAND, this protocol permits a macro to return an explicit
TOP-LEVEL-EXPANSION-RESULT containing zero or more definition forms."
  (check-type syntax syntax)
  (check-type environment environment)
  (labels ((expand-one (form)
             (check-type form syntax)
             (let ((macro (macro-at-head form environment)))
               (if (not macro)
                   (list form)
                   (let ((result (let ((*macro-expansion-syntax* form))
                                   (apply (termis-macro-implementation macro)
                                          (rest (termis-list-elements
                                                 (syntax-datum form)))))))
                     (cond ((typep result 'syntax) (expand-one result))
                           ((typep result 'top-level-expansion-result)
                            (mapcan #'expand-one
                                    (top-level-expansion-result-definitions result)))
                           (t
                            (error 'invalid-macro-result-error :value result))))))))
    (make-top-level-expansion-result (expand-one syntax))))

(defun make-compilation-environment ()
  "Create the compile-time environment used while constructing one unit."
  (let ((environment (make-bootstrap-environment)))
    ;; This internal helper is deliberately available only during compilation.
    ;; It gives source-defined macros a precise way to emit zero or more
    ;; definitions without giving an ordinary Termis list a second meaning.
    (environment-bind
     environment (make-termis-name "definitions")
     (make-termis-function
      (lambda (&rest definitions)
        (dolist (definition definitions)
          (check-type definition syntax))
        (make-top-level-expansion-result definitions))))
    environment))

(defun top-level-form-head (form)
  (let ((datum (syntax-datum form)))
    (when (termis-list-p datum)
      (let ((head (first (termis-list-elements datum))))
        (and head (termis-name-p (syntax-datum head))
             (termis-name-value (syntax-datum head)))))))

(defun parse-import-form (form loader)
  (let ((arguments (rest (termis-list-elements (syntax-datum form)))))
    (unless (or (= (length arguments) 1) (= (length arguments) 3))
      (error 'module-error :source form))
    (let ((name (parse-module-name (first arguments)))
          (alias nil))
      (when (= (length arguments) 3)
        (unless (and (termis-name-p (syntax-datum (second arguments)))
                     (string= (termis-name-value (syntax-datum (second arguments))) ":as")
                     (termis-name-p (syntax-datum (third arguments))))
          (error 'module-error :source form))
        (setf alias (syntax-datum (third arguments))))
      (make-instance 'import :module (module-loader-load loader name)
                            :alias alias :source form))))

(defun parse-export-form (form)
  (let ((names (rest (termis-list-elements (syntax-datum form)))))
    (dolist (name names)
      (unless (termis-name-p (syntax-datum name))
        (error 'module-error :source name)))
    (mapcar #'syntax-datum names)))

(defun install-imported-macros (module import)
  "Only macros cross the evaluator boundary; semantic bindings stay separate."
  (dolist (entry (module-exports (import-module import)))
    (let ((declaration (cdr entry)))
      (when (typep declaration 'macro-declaration)
        (let ((local-name
                (make-termis-name
                 (format nil "~A:~A"
                         (termis-name-value (import-qualifier-name import))
                         (termis-name-value (car entry))))))
          (multiple-value-bind (value foundp)
              (environment-find (module-environment (import-module import))
                                (car entry))
            (when foundp
              (environment-bind (module-environment module) local-name value))))))))

(defun register-import (module import)
  (let ((qualifier (import-qualifier-name import)))
    (when (module-find-import module qualifier)
      (error 'duplicate-import-alias :module module :source (import-source import)
             :alias qualifier))
    (push import (module-imports module))
    (push (cons qualifier import) (module-import-table module))
    (install-imported-macros module import)
    import))

(defun resolve-module-exports (module)
  (dolist (name (module-export-names module))
    (multiple-value-bind (declaration foundp) (find-declaration module name)
      (unless foundp
        (error 'unknown-export :module module :name name))
      (push (cons name declaration) (module-exports module))))
  (setf (module-exports module) (nreverse (module-exports module)))
  module)

(defun collect-module (module loader)
  (let ((environment (module-environment module)))
    ;; Only top-level forms reach the definition processor.  Expansion is
    ;; sequential because a preceding %MACRO can affect a following form.
    (dolist (form (module-forms module))
      (cond ((string= (or (top-level-form-head form) "") "import")
             (register-import module (parse-import-form form loader)))
            ((string= (or (top-level-form-head form) "") "export")
             (setf (module-export-names module)
                   (append (module-export-names module) (parse-export-form form))))
            (t (dolist (expanded-syntax
                         (top-level-expansion-result-definitions
                          (expand-top-level form environment)))
                 (process-definition environment module form expanded-syntax)))))
    (resolve-module-exports module)
    module))

(defun module-loader-find (loader name)
  (cdr (assoc name (module-loader-loaded-modules loader) :test #'module-name=)))

(defun module-source-pathname (loader name)
  (let ((filename (format nil "~A.termis" (module-name-string name))))
    (find-if #'probe-file
             (mapcar (lambda (root) (merge-pathnames filename root))
                     (module-loader-search-paths loader)))))

(defun module-loader-load (loader name)
  (let ((position (position name (module-loader-loading-stack loader)
                           :test #'module-name=)))
    (when position
      (error 'circular-module-dependency :module name
             :cycle (append (subseq (module-loader-loading-stack loader) position)
                            (list name))))
    (or (module-loader-find loader name)
        (let ((path (module-source-pathname loader name)))
          (unless path (error 'module-not-found :module name))
          (let* ((source (source-from-file path))
                 (module (make-instance 'module :name name :pathname path
                                         :source source :forms (read-source source)
                                         :environment (make-compilation-environment))))
            ;; Cache before collecting dependencies: identity is stable even
            ;; while its declaration namespace is being assembled.
            (push (cons name module) (module-loader-loaded-modules loader))
            (let ((old-stack (module-loader-loading-stack loader)))
              (unwind-protect
                   (progn
                     (setf (module-loader-loading-stack loader) (append old-stack (list name)))
                     (collect-module module loader))
                (setf (module-loader-loading-stack loader) old-stack)))
            module)))))

(defun compile-source (source)
  (let* ((name (make-module-name (make-termis-name "string")))
         (module (make-instance 'module :name name :identity-explicit-p nil
                                :source source :forms (read-source source)
                                :environment (make-compilation-environment))))
    (collect-module module (make-instance 'module-loader :search-paths '()))
    (resolve-program module (list module))
    module))

(defun compile-string (compiler contents &key (name "<string>"))
  "Read and discover primitive top-level declarations in CONTENTS."
  (check-type compiler compiler)
  (compile-source (make-source name contents)))

(defun compile-file (compiler pathname)
  "Read and discover primitive top-level declarations in PATHNAME."
  (check-type compiler compiler)
  (let* ((path (pathname pathname))
         (entry-name (module-name-from-pathname path))
         (loader (make-instance 'module-loader
                                :search-paths
                                (cons (make-pathname :name nil :type nil :defaults path)
                                      (compiler-search-paths compiler))))
         (entry (module-loader-load loader entry-name))
         ;; Recursive loading pushes a dependency after its importer has been
         ;; cached, so the cache's final order is already dependencies-first.
         (modules (mapcar #'cdr (module-loader-loaded-modules loader))))
    (resolve-program entry modules)
    entry))

(defun compile-module (compiler name)
  "Compile module NAME from COMPILER's ordered module search paths."
  (check-type compiler compiler)
  (let* ((module-name (if (module-name-p name) name
                          (parse-module-name
                           (make-syntax (make-termis-name name)
                                        (make-source "<module>" "")
                                        (make-source-location) (make-source-location)))))
         (loader (make-instance 'module-loader :search-paths (compiler-search-paths compiler)))
         (entry (module-loader-load loader module-name))
         (modules (mapcar #'cdr (module-loader-loaded-modules loader))))
    (resolve-program entry modules)
    entry))
