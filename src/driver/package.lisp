(defpackage #:termis.compiler
  (:use #:cl)
  (:shadow #:compile-file)
  (:import-from #:termis
                #:make-compiler #:compiler-search-paths
                #:compilation-unit-semantic-program #:program-target)
  (:import-from #:termis.backend.llvm
                #:make-target-configuration #:native-target-triple
                #:target-configuration-triple #:target-configuration-cpu
                #:target-configuration-features #:generate-llvm
                #:verify-llvm-module #:emit-object #:add-platform-entry-wrapper
                #:hide-termis-symbols
                #:llvm-backend-pointer-width #:llvm-backend-data-layout)
  (:export
   #:compiler-driver #:make-compiler-driver #:compiler-driver-search-paths
   #:compiler-driver-target #:compiler-driver-toolchain
   #:compilation-target #:resolve-compilation-target
   #:compilation-target-triple #:compilation-target-cpu
   #:compilation-target-features #:compilation-target-data-layout
   #:compilation-target-pointer-width #:compilation-target-object-format
   #:compilation-target-platform
   #:artifact #:artifact-kind #:artifact-path #:artifact-target
   #:object-artifact #:executable-artifact #:static-library-artifact
   #:shared-library-artifact
   #:link-options #:make-link-options #:link-options-libraries
   #:link-options-library-search-paths #:link-options-frameworks
   #:toolchain #:native-toolchain #:make-native-toolchain
   #:toolchain-emit-object #:toolchain-link-executable
   #:toolchain-archive-static-library #:toolchain-link-shared-library
   #:compile-root #:compile-file #:default-output-path
   #:compiler-driver-error #:unsupported-artifact #:unsupported-target
   #:llvm-verification-failure #:object-emission-failure #:invalid-entry-point
   #:toolchain-failure #:toolchain-failure-tool #:toolchain-failure-arguments
   #:toolchain-failure-exit-status #:toolchain-failure-stdout
   #:toolchain-failure-stderr #:linker-failure #:archiver-failure
   #:shared-library-link-failure #:main))
