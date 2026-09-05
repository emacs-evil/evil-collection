EMACS ?= emacs

DEPS := evil package-lint annalist magit

# Enumerate the package's .el files in elisp so we don't blow past
# Windows' cmd.exe command-line length limit (~8191 chars) when
# passing all mode files on the command line.
FILES_EXPR := (cons \"evil-collection.el\" \
                    (seq-remove \
                     (lambda (f) \
                       (or (string-prefix-p \"modes/magit/\" f) \
                           (string-prefix-p \"modes/magit-repos/\" f))) \
                     (directory-files-recursively \"modes\" \"\\\\.el\\\\'\")))

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
         --eval "(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\")))" \
         --eval "(package-initialize)"

BATCH := $(EMACS) --batch -Q -L . $(SETUP)

# `compat' lives on GNU ELPA only, and Emacs on Windows CI runners
# often can't complete the TLS handshake against elpa.gnu.org. Install
# it via `package-vc-install' from the `emacs-straight/compat' mirror
# (updated daily from GNU ELPA) so we never touch elpa.gnu.org. Guard
# on `package-alist' rather than `package-installed-p' - Emacs 30 ships
# a built-in `compat' stub that makes `package-installed-p' return t
# but doesn't satisfy magit's dependency check.
install:
	$(BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  --eval "(unless (assq 'compat package-alist) (package-vc-install \"https://github.com/emacs-straight/compat\"))" \
	  $(foreach dep,$(DEPS),--eval "(unless (package-installed-p '$(dep)) (package-install '$(dep)))")

compile: install
	$(BATCH) \
	  --eval "(setq evil-want-integration nil)" \
	  --eval "(setq evil-want-keybinding nil)" \
	  --eval "(setq byte-compile-docstring-max-column 200)" \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(let ((command-line-args-left $(FILES_EXPR))) (batch-byte-compile))"

lint: install
	-$(BATCH) \
	  --eval "(require 'package-lint)" \
	  $(foreach fn,$(LINT_IGNORES),--eval "(advice-add '$(fn) :around #'ignore)") \
	  --eval "(kill-emacs (if (package-lint-batch-and-exit-1 $(FILES_EXPR)) 0 1))"

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
