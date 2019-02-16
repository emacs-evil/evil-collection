EMACS ?= emacs

compile:
	cask exec $(EMACS) -Q -batch			\
	-L .						\
	--eval '(setq byte-compile-error-on-warn t)'	\
	-f batch-byte-compile *.el

lint:
	cask exec $(EMACS) -Q -batch							\
	--eval "(require 'package)"							\
	--eval "(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)"							\
	--eval "(package-refresh-contents)"						\
	-l package-lint.el								\
	--eval "(advice-add 'package-lint--check-eval-after-load :around 'ignore)" \
	-f package-lint-batch-and-exit *.el

test:
	cask exec ert-runner

.PHONY: compile lint test
