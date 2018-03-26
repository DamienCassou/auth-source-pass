SRCS = auth-source-pass.el
TESTS = test/auth-source-pass-tests.el

LOAD_PATH = -L . -L ../package-lint

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH) \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))" \
		--funcall package-initialize

.PHONY: all check test lint

all: check

ci-dependencies:
	# Install dependencies in ~/.emacs.d/elpa
	$(BATCH) \
	--funcall package-refresh-contents \
	--eval "(package-install 'package-lint)"

check: test lint

test:
	$(BATCH) --eval "(progn\
	(load-file \"test/auth-source-pass-tests.el\")\
	(ert-run-tests-batch-and-exit))"

lint :
	# Byte compile all and stop on any warning or error
	$(BATCH) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile ${SRCS} ${TESTS}

	# Run package-lint to check for packaging mistakes
	$(BATCH) \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit ${SRCS}
