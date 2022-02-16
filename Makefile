EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .
TESTPATH = -L ./test

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

compile:
	$(CASK) exec $(EMACS) -Q -batch							\
	-L .										\
	--eval "(setq evil-want-integration nil)"					\
	--eval "(setq evil-want-keybinding nil)"					\
	--eval "(setq byte-compile-docstring-max-column 200)"				\
	--eval "(setq byte-compile-error-on-warn t)"					\
	-f batch-byte-compile *.el modes/*/*.el

lint:
	$(CASK) exec $(EMACS) -Q -batch							\
	--eval "(require 'package)"							\
	--eval "(push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)"							\
	--eval "(package-refresh-contents)"						\
	-l package-lint.el								\
	--eval "(advice-add 'package-lint--check-eval-after-load :around 'ignore)"	\
	--eval "(advice-add 'package-lint--check-version-regexp-list :around 'ignore)"	\
	--eval "(advice-add 'package-lint--check-symbol-separators :around 'ignore)"	\
	--eval "(advice-add 'package-lint--check-defs-prefix :around 'ignore)"		\
	--eval "(advice-add 'package-lint--check-provide-form :around 'ignore)"		\
	-f package-lint-batch-and-exit *.el modes/*/*.el

test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(TESTPATH) \
-l evil-collection-test.el -l evil-collection-magit-tests.el -f ert-run-tests-batch-and-exit

magit-test: elpa
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(TESTPATH) \
-l evil-collection-magit-tests.el -f ert-run-tests-batch-and-exit

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	mkdir -p $(ELPA_DIR)
	touch $@

.PHONY: compile lint test elpa

# Local Variables:
# tab-width: 8
# End:
