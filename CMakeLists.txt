cmake_minimum_required(VERSION 3.25)

project(Termis
  VERSION 0.1.0
  DESCRIPTION "Termis programming language compiler"
  LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(TERMIS_LLVM_CONFIG "" CACHE FILEPATH "Path to llvm-config")

if(NOT TERMIS_LLVM_CONFIG AND DEFINED ENV{LLVM_CONFIG})
  set(TERMIS_LLVM_CONFIG "$ENV{LLVM_CONFIG}")
endif()

if(NOT TERMIS_LLVM_CONFIG)
  find_program(TERMIS_LLVM_CONFIG llvm-config)
endif()

if(TERMIS_LLVM_CONFIG)
  execute_process(
    COMMAND ${TERMIS_LLVM_CONFIG} --includedir
    OUTPUT_VARIABLE TERMIS_LLVM_INCLUDE_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY)
  execute_process(
    COMMAND ${TERMIS_LLVM_CONFIG} --libdir
    OUTPUT_VARIABLE TERMIS_LLVM_LIBRARY_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY)
  execute_process(
    COMMAND ${TERMIS_LLVM_CONFIG} --libs core support nativecodegen target mc asmprinter
    OUTPUT_VARIABLE TERMIS_LLVM_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY)
  execute_process(
    COMMAND ${TERMIS_LLVM_CONFIG} --cxxflags
    OUTPUT_VARIABLE TERMIS_LLVM_CXXFLAGS
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY)
  separate_arguments(TERMIS_LLVM_CXXFLAGS_LIST NATIVE_COMMAND "${TERMIS_LLVM_CXXFLAGS}")
  foreach(flag IN LISTS TERMIS_LLVM_CXXFLAGS_LIST)
    if(flag MATCHES "^-D")
      list(APPEND TERMIS_LLVM_DEFINITIONS "${flag}")
    endif()
  endforeach()
  separate_arguments(TERMIS_LLVM_LIBS NATIVE_COMMAND "${TERMIS_LLVM_LIBS}")
else()
  find_package(LLVM REQUIRED CONFIG)
  llvm_map_components_to_libnames(TERMIS_LLVM_LIBS
    asmprinter
    core
    mc
    nativecodegen
    target
    support)
  set(TERMIS_LLVM_INCLUDE_DIR "${LLVM_INCLUDE_DIRS}")
  set(TERMIS_LLVM_DEFINITIONS "${LLVM_DEFINITIONS}")
endif()

add_library(termis_core
  src/codegen.cpp
  src/layout.cpp
  src/reader.cpp
  src/semantic.cpp
  src/type.cpp)

target_include_directories(termis_core PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR}/src)

target_include_directories(termis_core SYSTEM PRIVATE
  ${TERMIS_LLVM_INCLUDE_DIR})

target_compile_definitions(termis_core PRIVATE
  ${TERMIS_LLVM_DEFINITIONS})

if(APPLE AND CMAKE_OSX_DEPLOYMENT_TARGET)
  target_compile_definitions(termis_core PRIVATE
    TERMIS_MACOS_DEPLOYMENT_TARGET="${CMAKE_OSX_DEPLOYMENT_TARGET}")
endif()

if(TERMIS_LLVM_LIBRARY_DIR)
  target_link_directories(termis_core PUBLIC
    ${TERMIS_LLVM_LIBRARY_DIR})
endif()

target_link_libraries(termis_core PUBLIC
  ${TERMIS_LLVM_LIBS})

target_compile_options(termis_core PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

add_executable(termisc
  src/main.cpp)

target_link_libraries(termisc PRIVATE termis_core)

target_compile_options(termisc PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

enable_testing()

add_test(NAME termisc_version
  COMMAND termisc --version)

add_test(NAME termisc_help
  COMMAND termisc --help)

add_test(NAME termisc_dump_llvm
  COMMAND termisc --dump-llvm ${CMAKE_CURRENT_SOURCE_DIR}/examples/add.termis)

add_test(NAME termisc_compile_binary
  COMMAND ${CMAKE_COMMAND}
    -D TERMIS_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
    -D TERMIS_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}
    -D TERMISC=$<TARGET_FILE:termisc>
    -P ${CMAKE_CURRENT_SOURCE_DIR}/tests/compile_binary.cmake)

add_executable(reader_tests
  tests/reader_tests.cpp)

target_link_libraries(reader_tests PRIVATE termis_core)

target_compile_options(reader_tests PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

add_test(NAME reader_tests
  COMMAND reader_tests)

add_executable(semantic_tests
  tests/semantic_tests.cpp)

target_link_libraries(semantic_tests PRIVATE termis_core)

target_compile_options(semantic_tests PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

add_test(NAME semantic_tests
  COMMAND semantic_tests)

add_executable(type_tests
  tests/type_tests.cpp)

target_link_libraries(type_tests PRIVATE termis_core)

target_compile_options(type_tests PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

add_test(NAME type_tests
  COMMAND type_tests)

add_executable(layout_tests
  tests/layout_tests.cpp)

target_link_libraries(layout_tests PRIVATE termis_core)

target_compile_options(layout_tests PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

add_test(NAME layout_tests
  COMMAND layout_tests)

add_executable(codegen_tests
  tests/codegen_tests.cpp)

target_link_libraries(codegen_tests PRIVATE termis_core)

target_compile_options(codegen_tests PRIVATE
  $<$<CXX_COMPILER_ID:Clang,GNU>:-Wall -Wextra -Wpedantic>)

add_test(NAME codegen_tests
  COMMAND codegen_tests)
