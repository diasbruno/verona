(in-package #:termis)

(define-condition termis-read-error (error)
  ((source :initarg :source :reader termis-read-error-source)
   (location :initarg :location :reader termis-read-error-location)
   (message :initarg :message :reader termis-read-error-message))
  (:report (lambda (condition stream)
             (let ((location (termis-read-error-location condition)))
               (format stream "~A:~D:~D: ~A"
                       (source-name (termis-read-error-source condition))
                       (source-location-line location)
                       (source-location-column location)
                       (termis-read-error-message condition))))))

(defstruct (reader-state (:constructor make-reader-state (source)))
  source
  (offset 0 :type (integer 0 *)))

(defun reader-contents (state)
  (source-contents (reader-state-source state)))

(defun reader-at-end-p (state)
  (>= (reader-state-offset state) (length (reader-contents state))))

(defun reader-peek (state)
  (unless (reader-at-end-p state)
    (char (reader-contents state) (reader-state-offset state))))

(defun reader-advance (state)
  (prog1 (reader-peek state)
    (incf (reader-state-offset state))))

(defun reader-location (state)
  (source-location-at (reader-state-source state) (reader-state-offset state)))

(defun reader-fail (state message &optional (offset (reader-state-offset state)))
  (error 'termis-read-error
         :source (reader-state-source state)
         :location (source-location-at (reader-state-source state) offset)
         :message message))

(defun termis-whitespace-p (character)
  (and character
       (member character '(#\Space #\Tab #\Newline #\Return) :test #'char=)))

(defun termis-delimiter-p (character)
  (or (null character)
      (termis-whitespace-p character)
      (find character "()\"" :test #'char=)))

(defun skip-whitespace (state)
  (loop while (termis-whitespace-p (reader-peek state))
        do (reader-advance state)))

(defun decimal-digits-p (text start end)
  (and (< start end)
       (loop for index from start below end
             always (digit-char-p (char text index)))))

(defun integer-literal-p (text)
  (let ((start (if (and (> (length text) 0)
                        (find (char text 0) "+-" :test #'char=))
                   1
                   0)))
    (decimal-digits-p text start (length text))))

(defun float-literal-p (text)
  (let* ((sign-end (if (and (> (length text) 0)
                            (find (char text 0) "+-" :test #'char=))
                       1
                       0))
         (dot (position #\. text :start sign-end)))
    (and dot
         (null (position #\. text :start (1+ dot)))
         (decimal-digits-p text sign-end dot)
         (decimal-digits-p text (1+ dot) (length text)))))

(defun read-atom (state start)
  (let ((text (with-output-to-string (output)
                (loop for character = (reader-peek state)
                      until (termis-delimiter-p character)
                      do (write-char (reader-advance state) output)))))
    (when (string= text "")
      (reader-fail state "expected a form" start))
    (cond ((string= text "unit")
           ;; UNIT is the one source spelling shared by UnitType and its
           ;; only inhabitant.  The semantic phase assigns its meaning from
           ;; context; the reader records the atom without host symbols.
           (make-unit-literal))
          ((string= text "true") (make-termis-boolean-literal t))
          ((string= text "false") (make-termis-boolean-literal nil))
          ((integer-literal-p text)
           (handler-case
               (parse-integer text)
             (error () (reader-fail state "integer literal is out of range" start))))
          ((float-literal-p text)
           ;; The grammar has no exponent notation; appending D0 makes the
           ;; resulting Common Lisp number the language's f64 representation.
           (read-from-string (concatenate 'string text "d0")))
          ((find #\. text)
           (reader-fail state
                        (if (some #'digit-char-p text)
                            "invalid numeric literal"
                            "'.' is not valid Termis syntax; use `unit`")
                        start))
          (t (make-termis-name text)))))

(defun read-string-literal (state start)
  (reader-advance state)
  (let ((value (with-output-to-string (output)
                 (loop for character = (reader-peek state)
                       do (when (null character)
                            (reader-fail state "unterminated string literal" start))
                          (reader-advance state)
                          (cond ((char= character #\") (return))
                                ((char= character #\\)
                                 (let ((escaped (reader-peek state)))
                                   (when (null escaped)
                                     (reader-fail state "unterminated string literal" start))
                                   (reader-advance state)
                                   (case escaped
                                     (#\n (write-char #\Newline output))
                                     (#\t (write-char #\Tab output))
                                     (#\" (write-char #\" output))
                                     (#\\ (write-char #\\ output))
                                     (otherwise (reader-fail state "unsupported string escape")))))
                                (t (write-char character output)))))))
    value))

(defun read-list (state start)
  (reader-advance state)
  (let ((elements '()))
    (loop do (skip-whitespace state)
              (when (reader-at-end-p state)
                (reader-fail state "unterminated list" start))
              (when (char= (reader-peek state) #\))
                (reader-advance state)
                (return (apply #'make-termis-list (nreverse elements))))
              (push (read-form state) elements))))

(defun read-form (state)
  (skip-whitespace state)
  (when (reader-at-end-p state)
    (reader-fail state "unexpected end of input"))
  (let* ((start (reader-state-offset state))
         (character (reader-peek state))
         (datum
           (cond ((char= character #\()
                  (read-list state start))
                 ((char= character #\))
                  (reader-fail state "unexpected ')'"))
                 ((char= character #\")
                  (read-string-literal state start))
                 ((char= character #\.)
                  (if (and (< (1+ start) (length (reader-contents state)))
                           (digit-char-p (char (reader-contents state) (1+ start))))
                      (reader-fail state "floating-point literals must start with a digit")
                      (reader-fail state "'.' is not valid Termis syntax; use `unit`" start)))
                 (t (read-atom state start)))))
    (make-instance 'syntax
                   :datum datum
                   :source (reader-state-source state)
                   :start (source-location-at (reader-state-source state) start)
                   :end (reader-location state))))

(defun read-source (source)
  "Read every top-level form in SOURCE without interpreting any form head."
  (check-type source source)
  (let ((state (make-reader-state source))
        (forms '()))
    (loop do (skip-whitespace state)
              (when (reader-at-end-p state)
                (return (nreverse forms)))
              (push (read-form state) forms))))
