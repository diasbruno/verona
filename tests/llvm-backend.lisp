(in-package #:termis/tests)

(in-suite :termis)

(defun native-test-path (type)
  (merge-pathnames (format nil "termis-native-~A.~A" (gensym "TEST-") type)
                   (uiop:temporary-directory)))

(defun compile-and-run-native (source)
  (let* ((executable (native-test-path "program"))
         (unit (compile-string (make-compiler) source))
         (program (compilation-unit-semantic-program unit)))
    (unwind-protect
         (progn
           (termis.backend.llvm:build-executable program executable)
           (nth-value 2 (uiop:run-program (list (namestring executable))
                                           :output :string :error-output :string
                                           :ignore-error-status t)))
      (when (probe-file executable)
        (delete-file executable)))))

(test creates-and-prints-an-empty-llvm-module
  (let ((backend (termis.backend.llvm:make-llvm-backend :module-name "empty")))
    (is (search "ModuleID = 'empty'" (termis.backend.llvm:print-llvm-module backend)))
    (is (eq backend (termis.backend.llvm:verify-llvm-module backend)))))

(test lowers-forward-function-calls-through-cl-llvm
  (let* ((unit (compile-string
                (make-compiler)
                (format nil
                        "(function add ((a i64) (b i64)) i64 (%+-primitive-i64 a b))~%(function main () i64 (add 20 22))")))
         (backend (termis.backend.llvm:generate-llvm
                   (compilation-unit-semantic-program unit)))
         (ir (termis.backend.llvm:print-llvm-module backend)))
    (is (search "define i64 @__termis_000061000064000064" ir))
    (is (search "add i64" ir))
    (is (search "call i64 @__termis_000061000064000064(i64 20, i64 22)" ir))))

(test lowers-unit-to-the-target-pointer-width
  (let* ((unit (compile-string (make-compiler) "(function noop () unit unit)"))
         (backend (termis.backend.llvm:generate-llvm
                   (compilation-unit-semantic-program unit)))
         (ir (termis.backend.llvm:print-llvm-module backend)))
    (is (= 64 (termis.backend.llvm:llvm-backend-pointer-width backend)))
    (is (search "define i64 @__termis_00006E00006F00006F000070()" ir))
    (is (search "ret i64 0" ir))))

(test emits-a-native-object-file
  (let* ((object (native-test-path "o"))
         (unit (compile-string (make-compiler) "(function answer () i64 42)"))
         (backend (termis.backend.llvm:generate-llvm
                   (compilation-unit-semantic-program unit))))
    (unwind-protect
         (progn
           (termis.backend.llvm:emit-object backend object)
           (is (probe-file object))
           (is (< 0 (with-open-file (stream object :direction :input :element-type '(unsigned-byte 8))
                      (file-length stream)))))
      (when (probe-file object)
        (delete-file object)))))

(test executes-native-termis-programs
  (is (= 0 (compile-and-run-native "(function main () i64 0)")))
  (is (= 42 (compile-and-run-native "(function main () i64 42)")))
  (is (= 42 (compile-and-run-native
             "(function main () i64 (%+-primitive-i64 20 22))")))
  (is (= 0 (compile-and-run-native
             (format nil "(function noop () unit unit)~%
                          (function main () i64 (do (noop) (%+-primitive-i64 0 0)))")))))

(test executes-explicit-conversions-natively
  (is (= 42 (compile-and-run-native
             (format nil "(function widen ((value i32)) i64 (%sext-primitive-i32-i64 value))~%
                          (function main () i64 (widen 42))")))))

(test lowers-and-executes-boolean-matches
  (let* ((source (format nil
			 "(function max ((a i64) (b i64)) i64~%
                            (match (%>-primitive-i64 a b)~%
                              (true a)~%
                              (false b)))~%
                          (function main () i64 (max 20 42))"))
	 (unit (compile-string (make-compiler) source))
	 (backend (termis.backend.llvm:generate-llvm
		   (compilation-unit-semantic-program unit)))
	 (ir (termis.backend.llvm:print-llvm-module backend)))
    (is (search "br i1" ir))
    (is (search "phi i64" ir))
    (is (= 42 (compile-and-run-native source)))))

(test executes-terminating-and-integer-match-cases
  (is (= 0 (compile-and-run-native
	    "(function normalize ((x i64)) i64 (match (%<-primitive-i64 x 0) (true (return 0)) (false x))) (function main () i64 (normalize -10))")))
  (is (= 42 (compile-and-run-native
	    "(function normalize ((x i64)) i64 (match (%<-primitive-i64 x 0) (true (return 0)) (false x))) (function main () i64 (normalize 42))")))
  (is (= 20 (compile-and-run-native
	    "(function classify ((x i64)) i64 (match x (0 10) (1 20) (_ 30))) (function main () i64 (classify 1))"))))

(test validates-the-executable-entry-contract
  (let ((unit (compile-string (make-compiler) "(function main () i32 0)")))
    (signals termis.backend.llvm:entry-point-error
      (termis.backend.llvm:build-executable
       (compilation-unit-semantic-program unit) (native-test-path "program")))))
