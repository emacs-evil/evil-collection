EMACS ?= emacs

DEPS := evil package-lint annalist magit

ALL_MODE_FILES := $(wildcard modes/*/*.el)
COMPILE_FILES  := evil-collection.el \
                  $(filter-out modes/magit/% modes/magit-repos/%,$(ALL_MODE_FILES))

LINT_IGNORES := package-lint--error-at-bol \
                package-lint--check-eval-after-load \
                package-lint--check-version-regexp-list \
                package-lint--check-symbol-separators \
                package-lint--check-defs-prefix \
                package-lint--check-provide-form

SETUP := --eval "(require 'package)" \
         --eval "(setq network-security-level 'low)" \
         --eval "(setq magit-credential-cache-daemon-socket nil)" \
         --eval "(setq package-user-dir (expand-file-name \".elpa\"))" \
         --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\") (\"melpa\" . \"https://melpa.org/packages/\")))" \
         --eval "(package-initialize)"

BATCH := $(EMACS) --batch -Q -L . $(SETUP)

install:
	$(BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  $(foreach dep,$(DEPS),--eval "(unless (package-installed-p '$(dep)) (package-install '$(dep)))")

compile: install
	$(BATCH) \
	  --eval "(setq evil-want-integration nil)" \
	  --eval "(setq evil-want-keybinding nil)" \
	  --eval "(setq byte-compile-docstring-max-column 200)" \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(COMPILE_FILES)

lint: install
	-$(BATCH) \
	  --eval "(require 'package-lint)" \
	  $(foreach fn,$(LINT_IGNORES),--eval "(advice-add '$(fn) :around #'ignore)") \
	  -f package-lint-batch-and-exit $(COMPILE_FILES)

test: install
	$(BATCH) -l test/test-helper.el \
	  -l test/evil-collection-test.el \
	  -f ert-run-tests-batch-and-exit

magit-test: install
	$(BATCH) -l test/test-helper.el \
	  -l test/evil-collection-magit-tests.el \
	  -f ert-run-tests-batch-and-exit

.PHONY: install compile lint test magit-test

# Local Variables:
# tab-width: 8
# End:
