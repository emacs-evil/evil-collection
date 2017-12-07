;;; evil-collection-slime.el --- Evil bindings for `slime' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
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
(require 'evil-collection-util)
(require 'slime nil t)

(defvar sldb-mode-map)
(defvar slime-inspector-mode-map)

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

  (evil-collection-util-evilify-map
   sldb-mode-map
   :mode sldb-mode
   :bindings
   "H" 'describe-mode
   (kbd "C-j") 'sldb-down
   (kbd "C-k") 'sldb-up
   (kbd "M-j") 'sldb-details-down
   (kbd "M-k") 'sldb-details-up
   "gb" 'sldb-break-on-return
   "gB" 'sldb-break-with-default-debugger)

  (evil-collection-util-evilify-map
   slime-inspector-mode-map
   :mode slime-inspector-mode
   :bindings
   ;; refresh
   "gr" 'slime-inspector-reinspect)

  (evil-define-key 'normal slime-mode-map
    (kbd "C-t") 'slime-pop-find-definition-stack
    ;; goto
    "gd" 'slime-edit-definition)

  (evil-define-key 'normal slime-popup-buffer-mode-map
    ;; quit
    "q" 'quit-window

    (kbd "C-t") 'slime-pop-find-definition-stack

    ;; goto
    "gd" 'slime-edit-definition)

  (evil-set-initial-state 'slime-xref-mode 'normal)
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
