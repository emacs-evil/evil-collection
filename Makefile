EMACS ?= emacs

compile:
	cask exec $(EMACS) -Q -batch			\
	-L .						\
	--eval '(setq evil-want-integration nil)'	\
	--eval '(setq byte-compile-error-on-warn t)'	\
	-f batch-byte-compile *.el modes/*/*.el

lint:
	cask exec $(EMACS) -Q -batch							\
	--eval "(require 'package)"							\
	--eval "(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)"							\
	--eval "(package-refresh-contents)"						\
	-l package-lint.el								\
	-f package-lint-batch-and-exit *.el modes/*/*.el

test:
	cask exec ert-runner

.PHONY: test compile
