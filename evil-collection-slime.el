;;; evil-collection-slime.el --- Evil bindings for `slime' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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
(require 'evil)
(require 'slime nil t)

(defvar slime-parent-map)
(defvar sldb-mode-map)
(defvar slime-inspector-mode-map)
(defvar slime-mode-map)
(defvar slime-popup-buffer-mode-map)
(defvar slime-xref-mode-map)

(defconst evil-collection-slime-maps '(slime-parent-map
                                       sldb-mode-map
                                       slime-inspector-mode-map
                                       slime-mode-map
                                       slime-popup-buffer-mode-map
                                       slime-xref-mode-map ))

(defun evil-collection-slime-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

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
  (evil-set-initial-state 'slime-xref-mode 'normal)

  (evil-define-key 'normal slime-parent-map
    "gd" 'slime-edit-definition
    (kbd "C-t") 'slime-pop-find-definition-stack)

  (evil-define-key 'normal sldb-mode-map
    (kbd "RET") 'sldb-default-action
    (kbd "C-m") 'sldb-default-action
    [return] 'sldb-default-action
    [mouse-2]  'sldb-default-action/mouse
    [follow-link] 'mouse-face
    "\C-i" 'sldb-cycle
    "g?" 'describe-mode
    "S" 'sldb-show-source
    "e" 'sldb-eval-in-frame
    "d" 'sldb-pprint-eval-in-frame
    "D" 'sldb-disassemble
    "i" 'sldb-inspect-in-frame
    "gj" 'sldb-down
    "gk" 'sldb-up
    (kbd "C-j") 'sldb-down
    (kbd "C-k") 'sldb-up
    "]" 'sldb-details-down
    "[" 'sldb-details-up
    (kbd "M-j") 'sldb-details-down
    (kbd "M-k") 'sldb-details-up
    "gg" 'sldb-beginning-of-backtrace
    "G" 'sldb-end-of-backtrace
    "t" 'sldb-toggle-details
    "gr" 'sldb-restart-frame
    "I" 'sldb-invoke-restart-by-name
    "R" 'sldb-return-from-frame
    "c" 'sldb-continue
    "s" 'sldb-step
    "n" 'sldb-next
    "o" 'sldb-out
    "b" 'sldb-break-on-return
    "a" 'sldb-abort
    "q" 'sldb-quit
    "A" 'sldb-break-with-system-debugger
    "B" 'sldb-break-with-default-debugger
    "P" 'sldb-print-condition
    "C" 'sldb-inspect-condition
    "g:" 'slime-interactive-eval)

  (evil-define-key 'normal slime-inspector-mode-map
    [return] 'slime-inspector-operate-on-point
    (kbd "C-m") 'slime-inspector-operate-on-point
    [mouse-1] 'slime-inspector-operate-on-click
    [mouse-2] 'slime-inspector-operate-on-click
    [mouse-6] 'slime-inspector-pop
    [mouse-7] 'slime-inspector-next
    "gk" 'slime-inspector-pop
    (kbd "C-k") 'slime-inspector-pop
    "gj" 'slime-inspector-next
    "j" 'slime-inspector-next
    "k" 'slime-inspector-previous-inspectable-object
    "K" 'slime-inspector-describe
    "p" 'slime-inspector-pprint
    "e" 'slime-inspector-eval
    "h" 'slime-inspector-history
    "gr" 'slime-inspector-reinspect
    "gv" 'slime-inspector-toggle-verbose
    "\C-i" 'slime-inspector-next-inspectable-object
    [(shift tab)]
    'slime-inspector-previous-inspectable-object ; Emacs translates S-TAB
    [backtab] 'slime-inspector-previous-inspectable-object ; to BACKTAB on X.
    "." 'slime-inspector-show-source
    "gR" 'slime-inspector-fetch-all
    "q" 'slime-inspector-quit)

  (evil-define-key 'normal slime-mode-map
    (kbd "K") 'slime-describe-symbol
    (kbd "C-t") 'slime-pop-find-definition-stack
    ;; goto
    "gd" 'slime-edit-definition)

  (evil-define-key 'normal slime-popup-buffer-mode-map
    ;; quit
    "q" 'quit-window

    (kbd "C-t") 'slime-pop-find-definition-stack

    ;; goto
    "gd" 'slime-edit-definition)

  (evil-define-key 'normal slime-xref-mode-map
    (kbd "RET") 'slime-goto-xref
    (kbd "S-<return>") 'slime-goto-xref
    "go" 'slime-show-xref
    "gj" 'slime-xref-next-line
    "gk" 'slime-xref-prev-line
    (kbd "C-j") 'slime-xref-next-line
    (kbd "C-k") 'slime-xref-prev-line
    "]" 'slime-xref-next-line
    "[" 'slime-xref-prev-line
    "gr" 'slime-recompile-xref
    "gR" 'slime-recompile-all-xrefs
    "r" 'slime-xref-retract)

  (add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-slime)
;;; evil-collection-slime.el ends here
