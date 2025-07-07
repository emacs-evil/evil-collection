;; -*- mode: eask; lexical-binding: t -*-

(package "evil-collection"
         "0.0.2"
         "A set of keybindings for Evil mode")

(website-url "https://github.com/emacs-evil/evil-collection")
(keywords "evil" "tools")

(package-file "evil-collection.el")
(files "modes/*/*.el" '(:exclude "modes/magit/*.el" "modes/magit-repos/*.el"))

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.2")
(depends-on "evil")
(depends-on "annalist")

(development
 (depends-on "f")
 (depends-on "ert-runner")
 (depends-on "package-lint")
 (depends-on "magit"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda (&rest _)
             (setq evil-want-integration nil)
             (setq evil-want-keybinding nil)
             (setq byte-compile-docstring-max-column 200)
             (setq byte-compile-error-on-warn t)))

(add-hook 'eask-before-lint/package-hook
           (lambda (&rest _)
             (advice-add 'package-lint--error-at-bol :around 'ignore)  ; ignore headers
             (advice-add 'package-lint--check-eval-after-load :around 'ignore)
             (advice-add 'package-lint--check-version-regexp-list :around 'ignore)
             (advice-add 'package-lint--check-symbol-separators :around 'ignore)
             (advice-add 'package-lint--check-defs-prefix :around 'ignore)
             (advice-add 'package-lint--check-provide-form :around 'ignore)))
