(in-package #:termis/tests)

(in-suite :termis)

(test compiler-driver-produces-native-artifacts
  (let* ((source (merge-pathnames (format nil "termis-driver-~A.termis" (gensym "TEST-"))
                                  (uiop:temporary-directory)))
         (object (merge-pathnames (format nil "termis-driver-~A.o" (gensym "TEST-"))
                                  (uiop:temporary-directory)))
         (driver (termis.compiler:make-compiler-driver)))
    (unwind-protect
         (progn
           (with-open-file (stream source :direction :output :if-exists :supersede)
             (write-string "(function main () unit unit)" stream))
           (let ((artifact (termis.compiler:compile-root driver source
                                                          :artifact-kind :object :output object)))
             (is (typep artifact 'termis.compiler:object-artifact))
             (is (probe-file (termis.compiler:artifact-path artifact)))
             (is (= (termis.compiler:compilation-target-pointer-width
                     (termis.compiler:artifact-target artifact))
                    (termis:type-context-pointer-width
                     (termis:semantic-program-type-context
                      (termis:compilation-unit-semantic-program
                       (termis:compile-file (termis:make-compiler) source)))))))
      (when (probe-file source) (delete-file source))
      (when (probe-file object) (delete-file object))))))

(test compiler-driver-links-unit-main
  (let* ((source (merge-pathnames (format nil "termis-driver-~A.termis" (gensym "TEST-"))
                                  (uiop:temporary-directory)))
         (executable (merge-pathnames (format nil "termis-driver-~A" (gensym "TEST-"))
                                      (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream source :direction :output :if-exists :supersede)
             (write-string "(function main () unit unit)" stream))
           (termis.compiler:compile-root (termis.compiler:make-compiler-driver) source
                                         :artifact-kind :executable :output executable)
           (is (zerop (nth-value 2 (uiop:run-program (list (namestring executable))
                                                     :ignore-error-status t)))))
      (when (probe-file source) (delete-file source))
      (when (probe-file executable) (delete-file executable)))))

(test compiler-driver-produces-libraries-without-main
  (let* ((source (merge-pathnames (format nil "termis-driver-~A.termis" (gensym "TEST-"))
                                  (uiop:temporary-directory)))
         (static (merge-pathnames (format nil "libtermis-driver-~A.a" (gensym "TEST-"))
                                  (uiop:temporary-directory)))
         (shared (merge-pathnames (format nil "libtermis-driver-~A.~A" (gensym "TEST-")
                                          (if (search "darwin" (termis.compiler:compilation-target-triple
                                                                (termis.compiler:resolve-compilation-target))
                                                      :test #'char-equal)
                                              "dylib" "so"))
                                  (uiop:temporary-directory)))
         (driver (termis.compiler:make-compiler-driver)))
    (unwind-protect
         (progn
           (with-open-file (stream source :direction :output :if-exists :supersede)
             (write-string "(function add ((a i32) (b i32)) i32 (+ a b))" stream))
           (is (probe-file (termis.compiler:artifact-path
                            (termis.compiler:compile-root driver source
                                                          :artifact-kind :static-library :output static))))
           (is (probe-file (termis.compiler:artifact-path
                            (termis.compiler:compile-root driver source
                                                          :artifact-kind :shared-library :output shared)))))
      (when (probe-file source) (delete-file source))
      (when (probe-file static) (delete-file static))
      (when (probe-file shared) (delete-file shared)))))

(test compiler-driver-links-an-explicit-c-export-from-a-static-library
  (let* ((directory (uiop:temporary-directory))
         (source (merge-pathnames (format nil "termis-export-~A.termis" (gensym "TEST-")) directory))
         (library (merge-pathnames (format nil "libtermis-export-~A.a" (gensym "TEST-")) directory))
         (shared (merge-pathnames (format nil "libtermis-export-~A.~A" (gensym "TEST-")
                                          (if (search "darwin" (termis.compiler:compilation-target-triple
                                                                (termis.compiler:resolve-compilation-target))
                                                      :test #'char-equal)
                                              "dylib" "so")) directory))
         (c-source (merge-pathnames (format nil "termis-export-~A.c" (gensym "TEST-")) directory))
         (executable (merge-pathnames (format nil "termis-export-~A" (gensym "TEST-")) directory))
         (shared-executable (merge-pathnames (format nil "termis-export-shared-~A" (gensym "TEST-")) directory)))
    (unwind-protect
         (progn
           (with-open-file (stream source :direction :output :if-exists :supersede)
             (write-string "(function add ((a i32) (b i32)) i32 (+ a b)) (native-export add)" stream))
           (termis.compiler:compile-root (termis.compiler:make-compiler-driver) source
                                         :artifact-kind :static-library :output library)
           (termis.compiler:compile-root (termis.compiler:make-compiler-driver) source
                                         :artifact-kind :shared-library :output shared)
           (with-open-file (stream c-source :direction :output :if-exists :supersede)
             (write-string "int add(int, int); int main(void) { return add(20, 22) == 42 ? 0 : 1; }" stream))
           (multiple-value-bind (stdout stderr status)
               (uiop:run-program (list (or (uiop:getenv "TERMIS_LINKER") "clang")
                                       (namestring c-source) (namestring library)
                                       "-o" (namestring executable))
                                 :output :string :error-output :string :ignore-error-status t)
             (declare (ignore stdout))
             (is (zerop status) stderr))
           (is (zerop (nth-value 2 (uiop:run-program (list (namestring executable))
                                                     :ignore-error-status t)))))
           (multiple-value-bind (stdout stderr status)
               (uiop:run-program (list (or (uiop:getenv "TERMIS_LINKER") "clang")
                                       (namestring c-source) (namestring shared)
                                       (format nil "-Wl,-rpath,~A" (namestring directory))
                                       "-o" (namestring shared-executable))
                                 :output :string :error-output :string :ignore-error-status t)
             (declare (ignore stdout))
             (is (zerop status) stderr))
           (is (zerop (nth-value 2 (uiop:run-program (list (namestring shared-executable))
                                                     :ignore-error-status t)))))
      (dolist (path (list source library shared c-source executable shared-executable))
        (when (probe-file path) (delete-file path)))))
