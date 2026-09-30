set(output "${VERONA_BINARY_DIR}/verona-add")
set(stdlib_output "${VERONA_BINARY_DIR}/verona-stdlib")
set(ffi_output "${VERONA_BINARY_DIR}/verona-ffi")
set(zero_terminated_memory_output "${VERONA_BINARY_DIR}/verona-zero-terminated-memory")
set(hello_output "${VERONA_BINARY_DIR}/verona-hello")

execute_process(
  COMMAND "${VERONAC}" -I "${VERONA_SOURCE_DIR}/std" --dump-llvm "${VERONA_SOURCE_DIR}/examples/add.vrn"
  RESULT_VARIABLE std_dump_result
  OUTPUT_VARIABLE std_dump_output
  ERROR_VARIABLE std_dump_error)

if(NOT std_dump_result EQUAL 0)
  message(FATAL_ERROR "veronac failed while loading std modules:\n${std_dump_output}\n${std_dump_error}")
endif()

if(NOT std_dump_output MATCHES "declare ptr @malloc")
  message(FATAL_ERROR "std.memory did not declare malloc after dependency loading")
endif()

if(NOT std_dump_output MATCHES "declare i64 @strlen\\(ptr\\)")
  message(FATAL_ERROR "std.memory did not declare strlen after dependency loading")
endif()

execute_process(
  COMMAND "${VERONAC}" -o "${output}" "${VERONA_SOURCE_DIR}/examples/add.vrn"
  RESULT_VARIABLE compile_result
  OUTPUT_VARIABLE compile_output
  ERROR_VARIABLE compile_error)

if(NOT compile_result EQUAL 0)
  message(FATAL_ERROR "veronac failed:\n${compile_output}\n${compile_error}")
endif()

execute_process(
  COMMAND "${output}"
  RESULT_VARIABLE run_result)

if(NOT run_result EQUAL 42)
  message(FATAL_ERROR "compiled binary returned ${run_result}, expected 42")
endif()

execute_process(
  COMMAND "${VERONAC}" -I "${VERONA_SOURCE_DIR}/std" -o "${stdlib_output}" "${VERONA_SOURCE_DIR}/examples/stdlib.vrn"
  RESULT_VARIABLE stdlib_compile_result
  OUTPUT_VARIABLE stdlib_compile_output
  ERROR_VARIABLE stdlib_compile_error)

if(NOT stdlib_compile_result EQUAL 0)
  message(FATAL_ERROR "veronac failed for stdlib example:\n${stdlib_compile_output}\n${stdlib_compile_error}")
endif()

execute_process(
  COMMAND "${stdlib_output}"
  RESULT_VARIABLE stdlib_run_result)

if(NOT stdlib_run_result EQUAL 42)
  message(FATAL_ERROR "stdlib example returned ${stdlib_run_result}, expected 42")
endif()

execute_process(
  COMMAND "${VERONAC}" -o "${ffi_output}" "${VERONA_SOURCE_DIR}/examples/ffi.vrn"
  RESULT_VARIABLE ffi_compile_result
  OUTPUT_VARIABLE ffi_compile_output
  ERROR_VARIABLE ffi_compile_error)

if(NOT ffi_compile_result EQUAL 0)
  message(FATAL_ERROR "veronac failed for FFI example:\n${ffi_compile_output}\n${ffi_compile_error}")
endif()

execute_process(
  COMMAND "${ffi_output}"
  RESULT_VARIABLE ffi_run_result)

if(NOT ffi_run_result EQUAL 42)
  message(FATAL_ERROR "FFI example returned ${ffi_run_result}, expected 42")
endif()

execute_process(
  COMMAND "${VERONAC}" -I "${VERONA_SOURCE_DIR}/std" -o "${zero_terminated_memory_output}" "${VERONA_SOURCE_DIR}/examples/zero-terminated-memory.vrn"
  RESULT_VARIABLE zero_terminated_memory_compile_result
  OUTPUT_VARIABLE zero_terminated_memory_compile_output
  ERROR_VARIABLE zero_terminated_memory_compile_error)

if(NOT zero_terminated_memory_compile_result EQUAL 0)
  message(FATAL_ERROR "veronac failed for zero-terminated memory example:\n${zero_terminated_memory_compile_output}\n${zero_terminated_memory_compile_error}")
endif()

execute_process(
  COMMAND "${zero_terminated_memory_output}"
  RESULT_VARIABLE zero_terminated_memory_run_result)

if(NOT zero_terminated_memory_run_result EQUAL 0)
  message(FATAL_ERROR "zero-terminated memory example returned ${zero_terminated_memory_run_result}, expected 0")
endif()

execute_process(
  COMMAND "${VERONAC}" -I "${VERONA_SOURCE_DIR}/std" -o "${hello_output}" "${VERONA_SOURCE_DIR}/examples/hello.vrn"
  RESULT_VARIABLE hello_compile_result
  OUTPUT_VARIABLE hello_compile_output
  ERROR_VARIABLE hello_compile_error)

if(NOT hello_compile_result EQUAL 0)
  message(FATAL_ERROR "veronac failed for hello example:\n${hello_compile_output}\n${hello_compile_error}")
endif()

execute_process(
  COMMAND "${hello_output}"
  RESULT_VARIABLE hello_run_result
  OUTPUT_VARIABLE hello_run_output)

if(NOT hello_run_output STREQUAL "Hello from Verona\n")
  message(FATAL_ERROR "hello example printed '${hello_run_output}', expected 'Hello from Verona\\n'")
endif()
