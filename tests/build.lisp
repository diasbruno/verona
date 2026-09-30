(in-package #:termis/tests)

(in-suite :termis)

(test parses-declarative-build-targets
  (let* ((source
           (make-source
            "termis.build"
            "(executable app
                (root app.main)
                (module-path \"src\")
                (module-path \"dependencies\")
                (target native)
                (optimize 2)
                (library \"sqlite3\")
                (library-path \"vendor/lib\")
                (framework \"CoreFoundation\"))
              (static-library core (root core))
              (shared-library plugin (root plugin) (target \"x86_64-unknown-linux-gnu\"))"))
         (file (termis.compiler:parse-build-source source :directory #P"/private/tmp/build-config/"))
         (app (termis.compiler:find-build-target file "app"))
         (core (termis.compiler:find-build-target file "core"))
         (plugin (termis.compiler:find-build-target file "plugin")))
    (is (typep app 'termis.compiler:executable-target))
    (is (typep core 'termis.compiler:static-library-target))
    (is (typep plugin 'termis.compiler:shared-library-target))
    (is (string= "app" (termis.compiler:build-name-value
                          (termis.compiler:build-target-name app))))
    (is (string= "app.main" (module-name-string
                               (termis.compiler:build-target-root-module app))))
    (is (eq :native (termis.compiler:build-target-compilation-target app)))
    (is (= 2 (termis.compiler:build-target-optimization app)))
    (is (= 2 (length (termis.compiler:build-target-module-paths app))))
    (is (equal '("sqlite3")
               (termis.compiler:link-options-libraries
                (termis.compiler:build-target-link-options app))))
    (is (equal '("CoreFoundation")
               (termis.compiler:link-options-frameworks
                (termis.compiler:build-target-link-options app))))
    (is (string= "x86_64-unknown-linux-gnu"
                 (termis.compiler:build-target-compilation-target plugin)))
    (is (equal :native (termis.compiler:build-target-compilation-target core)))
    (is (= 0 (termis.compiler:build-target-optimization core)))))

(test validates-build-declarations-before-compilation
  (flet ((parse (contents)
           (termis.compiler:parse-build-source
            (make-source "termis.build" contents) :directory #P"/private/tmp/build-config/")))
    (signals termis.compiler:build-parse-error
      (parse "(executable app (module-path \"src\"))"))
    (signals termis.compiler:build-parse-error
      (parse "(executable app (root app) (target native) (target \"x86_64-unknown-linux-gnu\"))"))
    (signals termis.compiler:unknown-build-option-error
      (parse "(executable app (root app) (output \"ignored\"))"))
    (signals termis.compiler:duplicate-build-target-error
      (parse "(executable app (root app)) (static-library app (root core))"))))

(test keeps-build-invocation-output-separate-from-target
  (let ((invocation (termis.compiler:make-build-invocation "app" "/private/tmp/termis-build-output")))
    (is (typep (termis.compiler:build-invocation-target-name invocation)
               'termis.compiler:build-name))
    (is (search "termis-build-output"
                (namestring (termis.compiler:build-invocation-output-directory invocation))))))

(test builds-all-native-artifact-kinds-from-a-build-file
  (let* ((directory (merge-pathnames (format nil "termis-build-~A/" (gensym "TEST-"))
                                     (uiop:temporary-directory)))
         (source-directory (merge-pathnames "src/" directory))
         (build-path (merge-pathnames "termis.build" directory))
         (app-output (merge-pathnames "dist/bin/" directory))
         (library-output (merge-pathnames "dist/lib/" directory)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames ".directory" source-directory))
           (with-open-file (stream (merge-pathnames "app.main.termis" source-directory)
                                   :direction :output :if-exists :supersede)
             (write-string "(function main () unit unit)" stream))
           (dolist (name '("core.termis" "plugin.termis"))
             (with-open-file (stream (merge-pathnames name source-directory)
                                     :direction :output :if-exists :supersede)
               (write-string "(function add ((a i32) (b i32)) i32 (+ a b))" stream)))
           (with-open-file (stream build-path :direction :output :if-exists :supersede)
             (write-string
              "(executable app (root app.main) (module-path \"src\") (optimize 1))
               (static-library core (root core) (module-path \"src\"))
               (shared-library plugin (root plugin) (module-path \"src\"))"
              stream))
           (let* ((file (termis.compiler:parse-build-file build-path))
                  (app (termis.compiler:execute-build
                        file (termis.compiler:make-build-invocation "app" app-output)))
                  (core (termis.compiler:execute-build
                         file (termis.compiler:make-build-invocation "core" library-output)))
                  (plugin (termis.compiler:execute-build
                           file (termis.compiler:make-build-invocation "plugin" library-output))))
             (is (typep app 'termis.compiler:executable-artifact))
             (is (probe-file (termis.compiler:artifact-path app)))
             (is (typep core 'termis.compiler:static-library-artifact))
             (is (string= "libcore.a" (file-namestring (termis.compiler:artifact-path core))))
             (is (probe-file (termis.compiler:artifact-path core)))
             (is (typep plugin 'termis.compiler:shared-library-artifact))
             (is (string= (format nil "libplugin.~A"
                                   (if (eq :darwin
                                           (termis.compiler:compilation-target-platform
                                            (termis.compiler:artifact-target plugin)))
                                       "dylib" "so"))
                          (file-namestring (termis.compiler:artifact-path plugin))))
             (is (probe-file (termis.compiler:artifact-path plugin)))))
      (when (probe-file directory)
        (uiop:delete-directory-tree directory :validate t)))))
