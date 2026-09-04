set(output "${TERMIS_BINARY_DIR}/termis-add")
set(stdlib_output "${TERMIS_BINARY_DIR}/termis-stdlib")
set(ffi_output "${TERMIS_BINARY_DIR}/termis-ffi")
set(zero_terminated_memory_output "${TERMIS_BINARY_DIR}/termis-zero-terminated-memory")
set(hello_output "${TERMIS_BINARY_DIR}/termis-hello")

execute_process(
  COMMAND "${TERMISC}" -I "${TERMIS_SOURCE_DIR}/std" --dump-llvm "${TERMIS_SOURCE_DIR}/examples/add.termis"
  RESULT_VARIABLE std_dump_result
  OUTPUT_VARIABLE std_dump_output
  ERROR_VARIABLE std_dump_error)

if(NOT std_dump_result EQUAL 0)
  message(FATAL_ERROR "termisc failed while loading std modules:\n${std_dump_output}\n${std_dump_error}")
endif()

if(NOT std_dump_output MATCHES "declare ptr @malloc")
  message(FATAL_ERROR "std.memory did not declare malloc after dependency loading")
endif()

if(NOT std_dump_output MATCHES "declare i64 @strlen\\(ptr\\)")
  message(FATAL_ERROR "std.memory did not declare strlen after dependency loading")
endif()

execute_process(
  COMMAND "${TERMISC}" -o "${output}" "${TERMIS_SOURCE_DIR}/examples/add.termis"
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_output
  ERROR_VARIABLE compile_error)

if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "termisc failed:\n${compile_output}\n${compile_error}")
endif()

execute_process(
  COMMAND "${output}"
  RESULT_VARIABLE run_result)

if(NOT run_result EQUAL 42)
  message(FATAL_ERROR "compiled binary returned ${run_result}, expected 42")
endif()

execute_process(
  COMMAND "${TERMISC}" -I "${TERMIS_SOURCE_DIR}/std" -o "${stdlib_output}" "${TERMIS_SOURCE_DIR}/examples/stdlib.termis"
  RESULT_VARIABLE stdlib_compile_result
  OUTPUT_VARIABLE stdlib_compile_output
  ERROR_VARIABLE stdlib_compile_error)

if(NOT stdlib_compile_result EQUAL 0)
  message(FATAL_ERROR "termisc failed for stdlib example:\n${stdlib_compile_output}\n${stdlib_compile_error}")
endif()

execute_process(
  COMMAND "${stdlib_output}"
  RESULT_VARIABLE stdlib_run_result)

if(NOT stdlib_run_result EQUAL 42)
  message(FATAL_ERROR "stdlib example returned ${stdlib_run_result}, expected 42")
endif()

execute_process(
  COMMAND "${TERMISC}" -o "${ffi_output}" "${TERMIS_SOURCE_DIR}/examples/ffi.termis"
  RESULT_VARIABLE ffi_compile_result
  OUTPUT_VARIABLE ffi_compile_output
  ERROR_VARIABLE ffi_compile_error)

if(NOT ffi_compile_result EQUAL 0)
  message(FATAL_ERROR "termisc failed for FFI example:\n${ffi_compile_output}\n${ffi_compile_error}")
endif()

execute_process(
  COMMAND "${ffi_output}"
  RESULT_VARIABLE ffi_run_result)

if(NOT ffi_run_result EQUAL 42)
  message(FATAL_ERROR "FFI example returned ${ffi_run_result}, expected 42")
endif()

execute_process(
  COMMAND "${TERMISC}" -I "${TERMIS_SOURCE_DIR}/std" -o "${zero_terminated_memory_output}" "${TERMIS_SOURCE_DIR}/examples/zero-terminated-memory.termis"
  RESULT_VARIABLE zero_terminated_memory_compile_result
  OUTPUT_VARIABLE zero_terminated_memory_compile_output
  ERROR_VARIABLE zero_terminated_memory_compile_error)

if(NOT zero_terminated_memory_compile_result EQUAL 0)
  message(FATAL_ERROR "termisc failed for zero-terminated memory example:\n${zero_terminated_memory_compile_output}\n${zero_terminated_memory_compile_error}")
endif()

execute_process(
  COMMAND "${zero_terminated_memory_output}"
  RESULT_VARIABLE zero_terminated_memory_run_result)

if(NOT zero_terminated_memory_run_result EQUAL 0)
  message(FATAL_ERROR "zero-terminated memory example returned ${zero_terminated_memory_run_result}, expected 0")
endif()

execute_process(
  COMMAND "${TERMISC}" -I "${TERMIS_SOURCE_DIR}/std" -o "${hello_output}" "${TERMIS_SOURCE_DIR}/examples/hello.termis"
  RESULT_VARIABLE hello_compile_result
  OUTPUT_VARIABLE hello_compile_output
  ERROR_VARIABLE hello_compile_error)

if(NOT hello_compile_result EQUAL 0)
  message(FATAL_ERROR "termisc failed for hello example:\n${hello_compile_output}\n${hello_compile_error}")
endif()

execute_process(
  COMMAND "${hello_output}"
  RESULT_VARIABLE hello_run_result
  OUTPUT_VARIABLE hello_run_output)

if(NOT hello_run_output STREQUAL "Hello from Termis\n")
  message(FATAL_ERROR "hello example printed '${hello_run_output}', expected 'Hello from Termis\\n'")
endif()
