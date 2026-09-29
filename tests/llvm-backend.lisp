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

(test lowers-and-executes-generic-dispatch
  (let ((source
          "(generic combine (left right))
           (implementation combine ((a i64) (b i64)) i64 (+ a b))
           (function twenty () i64 20)
           (function twenty-two () i64 22)
           (function main () i64 (combine (twenty) (twenty-two)))"))
    (is (= 42 (compile-and-run-native source)))))

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

(test lowers-let-bindings-as-ssa-values-and-executes-them
  (let* ((source (format nil
                         "(function sequential () i64~%
                            (let ((x i64 20)~%
                                  (y i64 (%+-primitive-i64 x 22)))~%
                              y))~%
                          (function shadow () i64~%
                            (let ((x i64 10))~%
                              (let ((x i64 42)) x)))~%
                          (function max-plus-ten ((a i64) (b i64)) i64~%
                            (match (%>-primitive-i64 a b)~%
                              (true (let ((x i64 (%+-primitive-i64 a 10))) x))~%
                              (false (let ((x i64 (%+-primitive-i64 b 10))) x))))~%
                          (function main () i64 (max-plus-ten (sequential) (shadow)))"))
         (unit (compile-string (make-compiler) source))
         (backend (termis.backend.llvm:generate-llvm
                   (compilation-unit-semantic-program unit)))
         (ir (termis.backend.llvm:print-llvm-module backend)))
    (is (search "add i64" ir))
    ;; Only parameter lowering creates storage in the current backend.  The
    ;; two parameter-free LET functions must therefore remain SSA-only.
    (is (not (search "alloca" (subseq ir 0 (or (search "define i64 @__termis_00006D" ir)
                                                 (length ir))))))
    (is (= 52 (compile-and-run-native source)))))

(test lowers-products-as-ssa-aggregates-and-executes-them
  (let* ((source
	  "(type point ((x i64) (y i64)))
             (type line ((start point) (end point)))
             (function make-point ((x i64) (y i64)) point (point x y))
             (function end-y ((value line)) i64 (field (field value end) y))
             (function main () i64
               (let ((value line (line (make-point 10 20) (make-point 30 42))))
                 (end-y value)))")
	 (unit (compile-string (make-compiler) source))
	 (backend (termis.backend.llvm:generate-llvm
		   (compilation-unit-semantic-program unit)))
	 (ir (termis.backend.llvm:print-llvm-module backend)))
    (is (search "insertvalue" ir))
    (is (search "extractvalue" ir))
    (is (= 42 (compile-and-run-native source)))))

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

(test lowers-and-executes-sum-construction-and-constructor-patterns
  (is (= 42 (compile-and-run-native
             "(type option (sum (none) (some i64)))
              (function unwrap ((value option)) i64
                (match value ((none) 0) ((some x) x)))
              (function main () i64 (unwrap (some 42)))")))
  (is (= 42 (compile-and-run-native
             "(type status (sum (success) (failure)))
              (function value ((state status)) i64
                (match state ((success) 42) ((failure) 0)))
              (function main () i64 (value (success)))")))
  (is (= 42 (compile-and-run-native
             "(type result (sum (ok i64 i64) (error i64)))
              (function value ((result result)) i64
                (match result
                  ((ok x y) (%+-primitive-i64 x y))
                  ((error _) 0)))
              (function main () i64 (value (ok 20 22)))")))
  (is (= 42 (compile-and-run-native
             "(type pair (product (left i64) (right i64)))
              (type result (sum (ok pair) (error i64)))
              (function unwrap ((result result)) i64
                (match result
                  ((ok pair) (%+-primitive-i64 (field pair left) (field pair right)))
                  ((error _) 0)))
              (function main () i64 (unwrap (ok (pair 20 22))))")))
  (is (= 10 (compile-and-run-native
             "(type option (sum (none) (some i64)))
              (function classify ((value option)) i64
                (match value ((some 0) 10) ((some x) x) ((none) 0)))
              (function main () i64 (classify (some 0)))")))
  (is (= 42 (compile-and-run-native
             "(type option (sum (none) (some i64)))
              (type response (product (status i64) (value option)))
              (function unwrap ((value option)) i64
                (match value ((none) 0) ((some x) x)))
              (function main () i64 (unwrap (field (response 0 (some 42)) value)))"))))
