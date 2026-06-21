;;; evil-collection-cider.el --- Evil bindings for Cider -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2024 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, cider, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for Cider.

;;; Code:
(require 'cl-lib)
(require 'cider nil t)
(require 'evil-collection)

(defvar cider-use-xref)

(declare-function cider-debug-mode-send-reply "cider-debug")

(defconst evil-collection-cider-maps '(cider-mode-map
                                       cider-repl-mode-map
                                       cider-repl-history-mode-map
                                       cider-test-report-mode-map
                                       cider-macroexpansion-mode-map
                                       cider-connections-buffer-mode-map))

(defun evil-collection-cider-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(defmacro evil-collection-cider-make-debug-command (&rest cider-commands)
  "Make functions that wrap `cider-debug' commands.

Cider debug commands are sent through `cider-debug-mode-send-reply'.

ex. \(cider-debug-mode-send-reply \":next\"\)"
  (let ((commands (if (consp cider-commands)
                      cider-commands
                    (list cider-commands))))
    `(progn
       ,@(cl-loop
          for command in commands
          collect
          (let ((funsymbol
                 (intern (format "evil-collection-cider-debug-%s" command))))
            `(defun ,funsymbol ()
               ,(format
                 "Send :%s to `cider-debug-mode-send-reply'." command)
               (interactive)
               (cider-debug-mode-send-reply ,(format ":%s" command))))))))

(evil-collection-cider-make-debug-command "continue"
                                          "continue-all"
                                          "next"
                                          "in"
                                          "out"
                                          "force-out"
                                          "eval"
                                          "inspect"
                                          "inspect-prompt"
                                          "locals"
                                          "inject"
                                          "stacktrace"
                                          "trace"
                                          "quit")

;;;###autoload
(defun evil-collection-cider-setup ()
  "Set up `evil' bindings for `cider'."
  (unless evil-move-beyond-eol
    (advice-add 'cider-eval-last-sexp :around 'evil-collection-cider-last-sexp)
    (advice-add 'cider-eval-last-sexp-and-replace :around 'evil-collection-cider-last-sexp)
    (advice-add 'cider-eval-last-sexp-to-repl :around 'evil-collection-cider-last-sexp)
    (with-eval-after-load 'cider-eval-sexp-fu
      (advice-add 'cider-esf--bounds-of-last-sexp :around 'evil-collection-cider-last-sexp)))

  (when evil-collection-setup-debugger-keys
    (add-hook 'cider-mode-hook #'evil-normalize-keymaps)
    (add-hook 'cider--debug-mode-hook #'evil-normalize-keymaps)
    (evil-collection-define-key 'normal 'cider-mode-map
      [f6] 'cider-browse-instrumented-defs)

    (evil-collection-define-key 'normal 'cider--debug-mode-map
      "C" 'evil-collection-cider-debug-continue-all
      "O" 'evil-collection-cider-debug-force-out
      "H" 'cider-debug-move-here
      "p" 'evil-collection-cider-debug-inspect
      "P" 'evil-collection-cider-debug-inspect-prompt
      "J" 'evil-collection-cider-debug-inject
      "S" 'evil-collection-cider-debug-stacktrace
      "t" 'evil-collection-cider-debug-trace)
    (evil-collection-bind 'cider--debug-mode-map 'quit 'evil-collection-cider-debug-quit))

  (evil-collection-bind 'cider--debug-mode-map   'debug-continue 'evil-collection-cider-debug-continue)
  (evil-collection-bind 'cider--debug-mode-map  'debug-step-over 'evil-collection-cider-debug-next)
  (evil-collection-bind 'cider--debug-mode-map  'debug-step-into 'evil-collection-cider-debug-in)
  (evil-collection-bind 'cider--debug-mode-map   'debug-step-out 'evil-collection-cider-debug-out)
  (evil-collection-bind 'cider--debug-mode-map 'debug-breakpoint 'cider-debug-defun-at-point)
  (evil-collection-bind 'cider-mode-map 'debug-breakpoint 'cider-debug-defun-at-point)
  (evil-collection-bind 'cider--debug-mode-map       'debug-eval 'evil-collection-cider-debug-eval)
  (evil-collection-bind 'cider--debug-mode-map     'debug-locals 'evil-collection-cider-debug-locals)

  (evil-collection-bind 'cider-mode-map 'lookup-doc 'cider-doc)
  (evil-collection-bind 'cider-mode-map  'goto-repl 'cider-switch-to-repl-buffer)
  (evil-collection-bind 'cider-mode-map  'find-file 'cider-find-resource)

  (unless cider-use-xref
    (evil-collection-bind 'cider-mode-map 'find-definition 'cider-find-var)
    (evil-collection-bind 'cider-mode-map  'pop-definition 'cider-pop-back))

  (evil-collection-bind 'cider-repl-mode-map    'refresh 'cider-refresh)
  (evil-collection-bind 'cider-repl-mode-map 'lookup-doc 'cider-doc)
  ;; FIXME: This seems to get overwritten by `cider-switch-to-repl-buffer'.
  (evil-collection-bind 'cider-repl-mode-map  'goto-repl 'cider-switch-to-last-clojure-buffer)
  (evil-collection-bind 'cider-repl-mode-map  'find-file 'cider-find-resource)

  (evil-collection-bind 'cider-repl-mode-map        'repl-submit 'cider-repl-return)
  (evil-collection-bind 'cider-repl-mode-map       'repl-newline 'newline)
  (evil-collection-bind 'cider-repl-mode-map 'repl-force-newline 'newline)

  (unless cider-use-xref
    (evil-collection-bind 'cider-repl-mode-map 'find-definition 'cider-find-var)
    (evil-collection-bind 'cider-repl-mode-map  'pop-definition 'cider-pop-back))

  (evil-collection-define-key '(normal visual) 'cider-repl-history-mode-map
    "u" 'cider-repl-history-undo-other-window)
  (evil-collection-bind 'cider-repl-history-mode-map       'action 'cider-repl-history-insert-and-quit)
  (evil-collection-bind 'cider-repl-history-mode-map    'next-item 'cider-repl-history-forward)
  (evil-collection-bind 'cider-repl-history-mode-map    'prev-item 'cider-repl-history-previous)
  (evil-collection-bind 'cider-repl-history-mode-map 'next-section 'cider-repl-history-forward)
  (evil-collection-bind 'cider-repl-history-mode-map 'prev-section 'cider-repl-history-previous)
  (evil-collection-bind 'cider-repl-history-mode-map    'quit 'cider-repl-history-quit)
  (evil-collection-bind 'cider-repl-history-mode-map 'refresh 'cider-repl-history-update)

  (evil-collection-define-key 'normal 'cider-test-report-mode-map
    (kbd "C-c ,") 'cider-test-commands-map
    (kbd "C-c C-t") 'cider-test-commands-map
    (kbd "M-p") 'cider-test-previous-result
    (kbd "M-n") 'cider-test-next-result

    ;; goto
    "gd" 'cider-test-jump

    "t" 'cider-test-jump
    "d" 'cider-test-ediff
    "e" 'cider-test-stacktrace
    "f" 'cider-test-rerun-failed-tests
    "n" 'cider-test-run-ns-tests
    "L" 'cider-test-run-loaded-tests
    "p" 'cider-test-run-project-tests)
  (evil-collection-bind 'cider-test-report-mode-map         'action 'cider-test-jump)
  (evil-collection-bind 'cider-test-report-mode-map           'quit 'cider-popup-buffer-quit-function)
  (evil-collection-bind 'cider-test-report-mode-map        'refresh 'cider-test-run-test)
  (evil-collection-bind 'cider-test-report-mode-map     'cycle-next 'cider-test-next-result)
  (evil-collection-bind 'cider-test-report-mode-map 'cycle-previous 'cider-test-previous-result)

  (evil-collection-bind 'cider-macroexpansion-mode-map 'lookup-doc 'cider-doc)

  (evil-collection-define-key 'normal 'cider-macroexpansion-mode-map
    "r" 'cider-macroexpand-again
    "J" 'cider-javadoc
    "." (if cider-use-xref 'xref-find-definitions 'cider-find-var)
    "m" 'cider-macroexpand-1-inplace
    "a" 'cider-macroexpand-all-inplace
    "u" 'cider-macroexpand-undo
    [remap undo] 'cider-macroexpand-undo)
  (evil-collection-bind 'cider-macroexpansion-mode-map 'quit 'cider-popup-buffer-quit-function)

  (evil-collection-define-key 'normal 'cider-connections-buffer-mode-map
    "d" 'cider-connections-make-default
    "c" 'cider-connection-browser
    "x" 'cider-connections-close-connection)
  (evil-collection-bind 'cider-connections-buffer-mode-map        'action 'cider-connections-goto-connection)
  (evil-collection-bind 'cider-connections-buffer-mode-map 'describe-mode 'describe-mode)

  (evil-set-initial-state 'cider-stacktrace-mode 'normal)
  (evil-collection-define-key 'normal 'cider-stacktrace-mode-map
    "gd" 'cider-stacktrace-jump
    "J" 'cider-stacktrace-toggle-java
    "C" 'cider-stacktrace-toggle-clj
    "R" 'cider-stacktrace-toggle-repl
    "T" 'cider-stacktrace-toggle-tooling
    "D" 'cider-stacktrace-toggle-duplicates
    "P" 'cider-stacktrace-show-only-project
    "A" 'cider-stacktrace-toggle-all
    "1" 'cider-stacktrace-cycle-cause-1
    "2" 'cider-stacktrace-cycle-cause-2
    "3" 'cider-stacktrace-cycle-cause-3
    "4" 'cider-stacktrace-cycle-cause-4
    "5" 'cider-stacktrace-cycle-cause-5
    "0" 'cider-stacktrace-cycle-all-causes
    (kbd "TAB") 'cider-stacktrace-cycle-current-cause
    [backtab] 'cider-stacktrace-cycle-all-causes)
  (evil-collection-bind 'cider-stacktrace-mode-map    'next-item 'cider-stacktrace-next-cause)
  (evil-collection-bind 'cider-stacktrace-mode-map    'prev-item 'cider-stacktrace-previous-cause)
  (evil-collection-bind 'cider-stacktrace-mode-map 'next-section 'cider-stacktrace-next-cause)
  (evil-collection-bind 'cider-stacktrace-mode-map 'prev-section 'cider-stacktrace-previous-cause)
  (evil-collection-bind 'cider-stacktrace-mode-map 'quit 'cider-popup-buffer-quit-function)

  (add-hook 'cider-inspector-mode-hook #'evil-normalize-keymaps)
  (evil-collection-bind 'cider-inspector-mode-map    'next-item 'cider-inspector-next-inspectable-object)
  (evil-collection-bind 'cider-inspector-mode-map    'prev-item 'cider-inspector-previous-inspectable-object)
  (evil-collection-bind 'cider-inspector-mode-map 'next-section 'cider-inspector-next-inspectable-object)
  (evil-collection-bind 'cider-inspector-mode-map   'prev-section 'cider-inspector-previous-inspectable-object)
  (evil-collection-bind 'cider-inspector-mode-map 'next-section-2 'cider-inspector-next-page)
  (evil-collection-bind 'cider-inspector-mode-map 'prev-section-2 'cider-inspector-prev-page)
  (evil-collection-define-key 'normal 'cider-inspector-mode-map
    [mouse-1] 'cider-inspector-operate-on-click
    "L" 'cider-inspector-pop
    " " 'cider-inspector-next-page
    "s" 'cider-inspector-set-page-size)
  (evil-collection-bind 'cider-inspector-mode-map  'action 'cider-inspector-operate-on-point)
  (evil-collection-bind 'cider-inspector-mode-map    'quit 'quit-window)
  (evil-collection-bind 'cider-inspector-mode-map 'refresh 'cider-inspector-refresh))

(provide 'evil-collection-cider)
;;; evil-collection-cider.el ends here
