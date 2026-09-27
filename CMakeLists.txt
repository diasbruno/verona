cmake_minimum_required(VERSION 3.25)

project(Termis LANGUAGES NONE)

find_program(SBCL sbcl REQUIRED)

add_custom_target(test
  COMMAND ${SBCL} --script ${CMAKE_CURRENT_SOURCE_DIR}/tests/run.lisp
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMENT "Running the Termis FiveAM suite")
