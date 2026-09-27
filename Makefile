.PHONY: help test

help:
	@printf '%s\n' 'Termis development commands:'
	@printf '%s\n' '  make test  Run the FiveAM front-end test suite'

test:
	sbcl --script tests/run.lisp
