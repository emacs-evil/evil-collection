;;; evil-collection-slime.el --- Evil bindings for `slime' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, slime, tools

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
;; Evil bindings for `slime-mode'.

;;; Code:
(require 'evil-collection)
(require 'slime nil t)

(defvar slime-parent-map)
(defvar sldb-mode-map)
(defvar slime-inspector-mode-map)
(defvar slime-mode-map)
(defvar slime-popup-buffer-mode-map)
(defvar slime-thread-control-mode-map)
(defvar slime-xref-mode-map)

(defconst evil-collection-slime-maps '(slime-parent-map
                                       sldb-mode-map
                                       slime-inspector-mode-map
                                       slime-mode-map
                                       slime-popup-buffer-mode-map
                                       slime-thread-control-mode-map
                                       slime-xref-mode-map))

(defun evil-collection-slime-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

;;;###autoload
(defun evil-collection-slime-setup ()
  "Set up `evil' bindings for `slime'."
  (unless evil-move-beyond-eol
    (advice-add 'slime-eval-last-expression :around 'evil-collection-slime-last-sexp)
    (advice-add 'slime-pprint-eval-last-expression :around 'evil-collection-slime-last-sexp)
    (advice-add 'slime-eval-print-last-expression :around 'evil-collection-slime-last-sexp)
    (advice-add 'slime-eval-last-expression-in-repl
                :around 'evil-collection-slime-last-sexp))

  (evil-set-initial-state 'sldb-mode 'normal)
  (evil-set-initial-state 'slime-inspector-mode 'normal)
  (evil-set-initial-state 'slime-popup-buffer-mode 'normal)
  (evil-set-initial-state 'slime-thread-control-mode 'normal)
  (evil-set-initial-state 'slime-xref-mode 'normal)

  (evil-collection-bind 'slime-parent-map
                        'find-definition 'slime-edit-definition
                        'pop-definition 'slime-pop-find-definition-stack)

  (evil-collection-bind 'sldb-mode-map 'action 'sldb-default-action)
  (evil-collection-define-key 'normal 'sldb-mode-map
    [mouse-2]  'sldb-default-action/mouse
    [follow-link] 'mouse-face
    "\C-i" 'sldb-cycle
    "S" 'sldb-show-source
    "d" 'sldb-pprint-eval-in-frame
    "D" 'sldb-disassemble
    "gi" 'sldb-inspect-in-frame
    (kbd "M-j") 'sldb-details-down
    (kbd "M-k") 'sldb-details-up
    "gg" 'sldb-beginning-of-backtrace
    "G" 'sldb-end-of-backtrace
    "I" 'sldb-invoke-restart-by-name
    "a" 'sldb-abort
    "A" 'sldb-break-with-system-debugger
    "B" 'sldb-break-with-default-debugger
    "P" 'sldb-print-condition
    "C" 'sldb-inspect-condition
    "g:" 'slime-interactive-eval
    "0" 'sldb-invoke-restart-0
    "1" 'sldb-invoke-restart-1
    "2" 'sldb-invoke-restart-2
    "3" 'sldb-invoke-restart-3
    "4" 'sldb-invoke-restart-4
    "5" 'sldb-invoke-restart-5
    "6" 'sldb-invoke-restart-6
    "7" 'sldb-invoke-restart-7
    "8" 'sldb-invoke-restart-8
    "9" 'sldb-invoke-restart-9)
  (evil-collection-bind 'sldb-mode-map
                        'next-section 'sldb-details-down
                        'prev-section 'sldb-details-up
                        'quit 'sldb-quit
                        'describe-mode 'describe-mode
                        'refresh 'sldb-restart-frame
                        'toggle 'sldb-toggle-details
                        'debug-continue 'sldb-continue
                        'debug-step-over 'sldb-next
                        'debug-step-into 'sldb-step
                        'debug-step-out 'sldb-out
                        'debug-breakpoint 'sldb-break-on-return
                        'debug-eval 'sldb-eval-in-frame
                        'debug-restart 'sldb-return-from-frame
                        'debug-frame-up 'sldb-up
                        'debug-frame-down 'sldb-down)

  (evil-collection-bind 'slime-inspector-mode-map 'action 'slime-inspector-operate-on-point)
  (evil-collection-define-key 'normal 'slime-inspector-mode-map
    [mouse-1] 'slime-inspector-operate-on-click
    [mouse-2] 'slime-inspector-operate-on-click
    [mouse-6] 'slime-inspector-pop
    [mouse-7] 'slime-inspector-next
    ;; TODO: `slime-inspector-next' and `slime-inspector-pop' should probably
    ;; just be bound to C-i and C-o.
    (kbd "C-o") 'slime-inspector-pop
    (kbd "C-i") 'slime-inspector-next
    "p" 'slime-inspector-pprint
    "e" 'slime-inspector-eval
    "M-p" 'slime-inspector-history
    "gv" 'slime-inspector-toggle-verbose
    (kbd "C-i") 'slime-inspector-next-inspectable-object
    "." 'slime-inspector-show-source)
  (evil-collection-bind 'slime-inspector-mode-map
                        'cycle-next 'slime-inspector-next-inspectable-object
                        'cycle-previous 'slime-inspector-previous-inspectable-object
                        'next-item 'slime-inspector-next
                        'prev-item 'slime-inspector-pop
                        'next-section 'slime-inspector-next
                        'prev-section 'slime-inspector-pop
                        'quit 'slime-inspector-quit
                        'lookup-doc 'slime-inspector-describe
                        'refresh 'slime-inspector-reinspect
                        'refresh-all 'slime-inspector-fetch-all
                        'find-definition 'slime-inspector-show-source)

  (evil-collection-bind 'slime-mode-map
                        'find-usages 'slime-edit-uses
                        'find-definition 'slime-edit-definition
                        'pop-definition 'slime-pop-find-definition-stack
                        'lookup-doc 'slime-describe-symbol
                        'goto-repl 'slime-switch-to-output-buffer)

  (evil-collection-bind 'slime-popup-buffer-mode-map
                        'quit 'quit-window
                        'find-definition 'slime-edit-definition
                        'pop-definition 'slime-pop-find-definition-stack)

  (evil-collection-inhibit-insert-state 'slime-thread-control-mode-map)
  (evil-collection-define-key 'normal 'slime-thread-control-mode-map
    "a" 'slime-thread-attach
    "d" 'slime-thread-debug)
  (evil-collection-bind 'slime-thread-control-mode-map
                        'refresh 'slime-update-threads-buffer
                        'delete-2 'slime-thread-kill)

  (evil-collection-define-key 'normal 'slime-xref-mode-map
    "r" 'slime-xref-retract)
  (evil-collection-bind 'slime-xref-mode-map
                        'action 'slime-goto-xref
                        'action-other 'slime-goto-xref
                        'action-stay 'slime-show-xref
                        'next-item 'slime-xref-next-line
                        'prev-item 'slime-xref-prev-line
                        'next-section 'slime-xref-next-line
                        'prev-section 'slime-xref-prev-line
                        'refresh 'slime-recompile-xref
                        'refresh-all 'slime-recompile-all-xrefs)

  (evil-collection-bind 'slime-repl-mode-map
                        'repl-submit 'slime-repl-return
                        'repl-newline 'slime-repl-newline-and-indent
                        'repl-force-newline 'slime-repl-newline-and-indent
                        'next-item 'slime-repl-next-prompt
                        'prev-item 'slime-repl-previous-prompt
                        'next-section 'slime-repl-next-prompt
                        'prev-section 'slime-repl-previous-prompt
                        'history-previous 'slime-repl-previous-input
                        'history-next     'slime-repl-next-input)

  (add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-slime)
;;; evil-collection-slime.el ends here
