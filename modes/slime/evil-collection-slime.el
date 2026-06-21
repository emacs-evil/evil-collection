;;; evil-collection-slime.el --- Evil bindings for `slime' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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

  (evil-collection-theme-bind 'find-definition 'slime-parent-map 'slime-edit-definition)
  (evil-collection-theme-bind 'pop-definition  'slime-parent-map 'slime-pop-find-definition-stack)

  (evil-collection-define-key 'normal 'sldb-mode-map
    (kbd "RET") 'sldb-default-action
    [mouse-2]  'sldb-default-action/mouse
    [follow-link] 'mouse-face
    "\C-i" 'sldb-cycle
    "S" 'sldb-show-source
    "e" 'sldb-eval-in-frame
    "d" 'sldb-pprint-eval-in-frame
    "D" 'sldb-disassemble
    "gi" 'sldb-inspect-in-frame
    (kbd "M-j") 'sldb-details-down
    (kbd "M-k") 'sldb-details-up
    "gg" 'sldb-beginning-of-backtrace
    "G" 'sldb-end-of-backtrace
    "t" 'sldb-toggle-details
    "I" 'sldb-invoke-restart-by-name
    "R" 'sldb-return-from-frame
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
  (evil-collection-theme-bind 'next-item        'sldb-mode-map 'sldb-down)
  (evil-collection-theme-bind 'prev-item        'sldb-mode-map 'sldb-up)
  (evil-collection-theme-bind 'next-section     'sldb-mode-map 'sldb-details-down)
  (evil-collection-theme-bind 'prev-section     'sldb-mode-map 'sldb-details-up)
  (evil-collection-theme-bind 'next-section-2   'sldb-mode-map 'sldb-down)
  (evil-collection-theme-bind 'prev-section-2   'sldb-mode-map 'sldb-up)
  (evil-collection-theme-bind 'quit             'sldb-mode-map 'sldb-quit)
  (evil-collection-theme-bind 'describe-mode    'sldb-mode-map 'describe-mode)
  (evil-collection-theme-bind 'refresh          'sldb-mode-map 'sldb-restart-frame)
  (evil-collection-theme-bind 'debug-continue   'sldb-mode-map 'sldb-continue)
  (evil-collection-theme-bind 'debug-step-over  'sldb-mode-map 'sldb-next)
  (evil-collection-theme-bind 'debug-step-into  'sldb-mode-map 'sldb-step)
  (evil-collection-theme-bind 'debug-step-out   'sldb-mode-map 'sldb-out)
  (evil-collection-theme-bind 'debug-breakpoint 'sldb-mode-map 'sldb-break-on-return)

  (evil-collection-define-key 'normal 'slime-inspector-mode-map
    (kbd "RET") 'slime-inspector-operate-on-point
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
    (kbd "<tab>") 'slime-inspector-next-inspectable-object
    (kbd "C-i") 'slime-inspector-next-inspectable-object
    (kbd "<S-tab>") 'slime-inspector-previous-inspectable-object ; Emacs translates S-TAB
    (kbd "<backtab>") 'slime-inspector-previous-inspectable-object ; to BACKTAB on X.
    "." 'slime-inspector-show-source
    "gd" 'slime-inspector-show-source)
  (evil-collection-theme-bind 'next-item    'slime-inspector-mode-map 'slime-inspector-next)
  (evil-collection-theme-bind 'prev-item    'slime-inspector-mode-map 'slime-inspector-pop)
  (evil-collection-theme-bind 'next-section 'slime-inspector-mode-map 'slime-inspector-next)
  (evil-collection-theme-bind 'prev-section 'slime-inspector-mode-map 'slime-inspector-pop)
  (evil-collection-theme-bind 'quit        'slime-inspector-mode-map 'slime-inspector-quit)
  (evil-collection-theme-bind 'lookup-doc  'slime-inspector-mode-map 'slime-inspector-describe)
  (evil-collection-theme-bind 'refresh     'slime-inspector-mode-map 'slime-inspector-reinspect)
  (evil-collection-theme-bind 'refresh-all 'slime-inspector-mode-map 'slime-inspector-fetch-all)

  (evil-collection-theme-bind 'find-usages     'slime-mode-map 'slime-edit-uses)
  (evil-collection-theme-bind 'find-definition 'slime-mode-map 'slime-edit-definition)
  (evil-collection-theme-bind 'pop-definition  'slime-mode-map 'slime-pop-find-definition-stack)
  (evil-collection-theme-bind 'lookup-doc      'slime-mode-map 'slime-describe-symbol)
  (evil-collection-theme-bind 'goto-repl       'slime-mode-map 'slime-switch-to-output-buffer)

  (evil-collection-theme-bind 'quit 'slime-popup-buffer-mode-map 'quit-window)

  (evil-collection-theme-bind 'find-definition 'slime-popup-buffer-mode-map 'slime-edit-definition)
  (evil-collection-theme-bind 'pop-definition  'slime-popup-buffer-mode-map 'slime-pop-find-definition-stack)

  (evil-collection-inhibit-insert-state 'slime-thread-control-mode-map)
  (evil-collection-define-key 'normal 'slime-thread-control-mode-map
    "a" 'slime-thread-attach
    "d" 'slime-thread-debug
    "x" 'slime-thread-kill)
  (evil-collection-theme-bind 'refresh 'slime-thread-control-mode-map 'slime-update-threads-buffer)

  (evil-collection-define-key 'normal 'slime-xref-mode-map
    (kbd "RET") 'slime-goto-xref
    (kbd "S-<return>") 'slime-goto-xref
    "go" 'slime-show-xref
    "r" 'slime-xref-retract)
  (evil-collection-theme-bind 'next-item    'slime-xref-mode-map 'slime-xref-next-line)
  (evil-collection-theme-bind 'prev-item    'slime-xref-mode-map 'slime-xref-prev-line)
  (evil-collection-theme-bind 'next-section 'slime-xref-mode-map 'slime-xref-next-line)
  (evil-collection-theme-bind 'prev-section 'slime-xref-mode-map 'slime-xref-prev-line)
  (evil-collection-theme-bind 'refresh     'slime-xref-mode-map 'slime-recompile-xref)
  (evil-collection-theme-bind 'refresh-all 'slime-xref-mode-map 'slime-recompile-all-xrefs)

  (evil-collection-theme-bind 'repl-submit        'slime-repl-mode-map 'slime-repl-return)
  (evil-collection-theme-bind 'repl-newline       'slime-repl-mode-map 'slime-repl-newline-and-indent)
  (evil-collection-theme-bind 'repl-force-newline 'slime-repl-mode-map 'slime-repl-newline-and-indent)

  (evil-collection-theme-bind 'next-item    'slime-repl-mode-map 'slime-repl-next-prompt)
  (evil-collection-theme-bind 'prev-item    'slime-repl-mode-map 'slime-repl-previous-prompt)
  (evil-collection-theme-bind 'next-section 'slime-repl-mode-map 'slime-repl-next-prompt)
  (evil-collection-theme-bind 'prev-section 'slime-repl-mode-map 'slime-repl-previous-prompt)
  (evil-collection-define-key 'normal 'slime-repl-mode-map
    (kbd "C-p") 'slime-repl-previous-input
    (kbd "C-n") 'slime-repl-next-input)

  (add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-slime)
;;; evil-collection-slime.el ends here
