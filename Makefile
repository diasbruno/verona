.PHONY: help test

help:
	@printf '%s\n' 'Termis development commands:'
	@printf '%s\n' '  make test  Run the FiveAM front-end test suite'
	@printf '%s\n' '  make test-llvm  Run the FiveAM LLVM backend test suite'

test:
	sbcl --script tests/run.lisp

test-llvm:
	sbcl --script tests/run-llvm.lisp
