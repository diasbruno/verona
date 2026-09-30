VERONA_EXECUTABLE := build/verona
VERONA_SOURCES := $(shell find src -type f -name '*.lisp' -print)

.PHONY: help build test test-llvm

help:
	@printf '%s\n' 'Verona development commands:'
	@printf '%s\n' '  make build Build the standalone verona executable'
	@printf '%s\n' '  make test  Run the FiveAM front-end test suite'
	@printf '%s\n' '  make test-llvm  Run the FiveAM LLVM backend test suite'

build: $(VERONA_EXECUTABLE)

$(VERONA_EXECUTABLE): verona.asd scripts/build-executable.lisp $(VERONA_SOURCES)
	@mkdir -p $(@D)
	sbcl --script scripts/build-executable.lisp $@

test:
	sbcl --script tests/run.lisp

test-llvm:
	sbcl --script tests/run-llvm.lisp
