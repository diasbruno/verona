(in-package #:verona)

;;; Conditions in this file are the small, dependency-free error boundary for
;;; the whole front end.  Individual phases retain their useful specialised
;;; condition classes, but all failures caused by a Verona program inherit
;;; USER-COMPILATION-ERROR and can therefore be rendered uniformly.
(define-condition user-compilation-error (error)
  ((diagnostic :initarg :diagnostic :initform nil :reader condition-diagnostic))
  (:documentation "A problem in user input, configuration, or a tool invocation."))

(define-condition compiler-bug (error)
  ((message :initarg :message :reader compiler-bug-message)
   (context :initarg :context :initform nil :reader compiler-bug-context))
  (:report (lambda (condition stream)
             (format stream "Verona compiler invariant failed: ~A~@[ (~S)~]"
                     (compiler-bug-message condition)
                     (compiler-bug-context condition)))))

(defmacro compiler-assert (test control &rest arguments)
  "Assert an internal compiler invariant without misclassifying it as user input.

This deliberately does not catch host conditions: an unexpected host failure
is valuable SBCL debugging information, not a fabricated Verona diagnostic."
  `(unless ,test
     (error 'compiler-bug :message (format nil ,control ,@arguments))))

(defstruct (source-range (:constructor make-source-range (start end)))
  "An authoritative half-open source range, expressed in source offsets."
  (start (error "source range needs a start") :type source-location)
  (end (error "source range needs an end") :type source-location))

(defstruct (diagnostic (:constructor make-diagnostic
                         (&key severity code message primary-location
                               secondary-locations notes data)))
  (severity :error :type symbol)
  (code "E0000" :type string)
  (message "" :type string)
  primary-location
  (secondary-locations '() :type list)
  (notes '() :type list)
  ;; DATA carries structured phase-specific facts (for example expected and
  ;; actual types) without making the renderer parse prose.
  (data '() :type list))

(defparameter +error-severity+ :error)
(defparameter +warning-severity+ :warning)

(define-condition source-error (user-compilation-error)
  ((message :initarg :message :reader source-error-message))
  (:report (lambda (condition stream)
             (write-string (source-error-message condition) stream))))

(defclass source ()
  ((name :initarg :name :reader source-name)
   (contents :initarg :contents :reader source-contents)))

(defun make-source (name contents)
  "Create source owned by the compiler front end."
  (check-type name string)
  (check-type contents string)
  (make-instance 'source :name name :contents contents))

(defun source-from-file (pathname)
  "Load PATHNAME into a source, retaining its namestring for diagnostics."
  (let ((path (pathname pathname)))
    (handler-case
        (with-open-file (stream path :direction :input :element-type 'character)
          (make-source (namestring path)
                       (with-output-to-string (contents)
                         (loop for character = (read-char stream nil nil)
                               while character
                               do (write-char character contents)))))
      (file-error (condition)
        (error 'source-error
               :message (format nil "Unable to read source file ~A: ~A" path condition))))))

(defstruct source-location
  source
  (offset 0 :type (integer 0 *))
  (line 1 :type (integer 1 *))
  (column 1 :type (integer 1 *)))

(defun source-location-at (source offset)
  (let ((line 1)
        (column 1)
        (contents (source-contents source)))
    (loop for index below offset
          for character = (char contents index)
          do (if (char= character #\Newline)
                 (setf line (1+ line)
                       column 1)
                 (incf column)))
    (make-source-location :source source :offset offset :line line :column column)))

(defun syntax-source-range (syntax)
  "Return SYNTAX's half-open source range."
  (make-source-range (syntax-start syntax) (syntax-end syntax)))

(defgeneric diagnostic-code-for (condition)
  (:documentation "Return CONDITION's stable diagnostic identity."))

(defmethod diagnostic-code-for ((condition user-compilation-error))
  (declare (ignore condition)) "E0000")

(defgeneric condition-primary-range (condition)
  (:documentation "Return CONDITION's primary source range when it has one."))

(defmethod condition-primary-range ((condition condition))
  "Best-effort source range for an existing specialised condition."
  (let ((syntax (ignore-errors (slot-value condition 'syntax))))
    (when (typep syntax 'syntax) (syntax-source-range syntax))))

(defgeneric diagnostic-for-condition (condition)
  (:documentation "Translate CONDITION to the common diagnostic representation."))

(defmethod diagnostic-for-condition ((condition condition))
  "Translate a user-facing condition to the common structured representation.

Specific condition types remain public API; this adapter keeps the migration
incremental and lets tests assert code/range rather than report wording."
  (or (and (typep condition 'user-compilation-error)
           (condition-diagnostic condition))
      (when (typep condition 'user-compilation-error)
        (make-diagnostic
         :severity +error-severity+
         :code (diagnostic-code-for condition)
         :message (princ-to-string condition)
         :primary-location (condition-primary-range condition)))))

(defun render-diagnostic (diagnostic &optional (stream *error-output*))
  "Render DIAGNOSTIC for the CLI while keeping its data independent of text."
  (let* ((range (diagnostic-primary-location diagnostic))
         (start (and range (source-range-start range)))
         (source (and start (source-location-source start))))
    (when start
      (format stream "~A:~D:~D: " (if source (source-name source) "<unknown>")
              (source-location-line start) (source-location-column start)))
    (format stream "~(~A~)[~A]: ~A~%" (diagnostic-severity diagnostic)
            (diagnostic-code diagnostic) (diagnostic-message diagnostic))
    (when (and source start)
      (let* ((contents (source-contents source))
             (offset (source-location-offset start))
             (line-start (or (position #\Newline contents :end offset :from-end t) -1))
             (line-start (1+ line-start))
             (line-end (or (position #\Newline contents :start offset) (length contents)))
             (width (max 1 (if range
                                (- (source-location-offset (source-range-end range)) offset)
                                1))))
        (format stream "~%    ~A~%    ~A~A~%"
                (subseq contents line-start line-end)
                (make-string (max 0 (- offset line-start)) :initial-element #\Space)
                (make-string width :initial-element #\^))))
    (dolist (secondary (diagnostic-secondary-locations diagnostic))
      (format stream "note: ~A~%" secondary))
    (dolist (note (diagnostic-notes diagnostic))
      (format stream "note: ~A~%" note))
    diagnostic))
