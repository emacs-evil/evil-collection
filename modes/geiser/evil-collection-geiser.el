;;; evil-collection-geiser.el --- Bindings for `geiser' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, geiser, tools

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
;;; Bindings for `geiser'.

;;; Code:
(require 'evil-collection)
(require 'geiser nil t)

(defvar geiser-debug-mode-map)
(defvar geiser-doc-mode-map)

(defconst evil-collection-geiser-maps '(geiser-debug-mode-map
                                        geiser-doc-mode-map
                                        geiser-repl-mode-map
                                        geiser-mode-map))

(defun evil-collection-geiser-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

;;;###autoload
(defun evil-collection-geiser-setup ()
  "Set up bindings for `geiser'."
  (unless evil-move-beyond-eol
    (advice-add 'geiser-eval-last-sexp :around 'evil-collection-geiser-last-sexp)
    (advice-add 'geiser-eval-last-sexp-and-print :around 'evil-collection-geiser-last-sexp))

  (evil-set-initial-state 'geiser-debug-mode 'normal)
  (evil-set-initial-state 'geiser-doc-mode 'normal)

  (evil-collection-bind 'geiser-debug-mode-map 'quit 'quit-window)

  (evil-collection-bind 'geiser-doc-mode-map
                        'find-definition 'geiser-edit-symbol-at-point
                        'pop-definition 'geiser-pop-symbol-stack
                        'goto-repl 'geiser-doc-switch-to-repl)

  (evil-collection-define-key 'normal 'geiser-doc-mode-map
    ">" 'geiser-doc-next
    "<" 'geiser-doc-previous
    "X" 'geiser-doc-clean-history)
  (evil-collection-bind 'geiser-doc-mode-map
                        'next-button 'forward-button
                        'previous-button 'backward-button
                        'next-item 'forward-button
                        'prev-item 'backward-button
                        'next-section 'geiser-doc-next-section
                        'prev-section 'geiser-doc-previous-section
                        'next-section-2 'forward-button
                        'prev-section-2 'backward-button
                        'quit 'View-quit
                        'refresh 'geiser-doc-refresh
                        'delete 'geiser-doc-kill-page
                        'delete-2 'geiser-doc-kill-page)

  (evil-collection-bind 'geiser-repl-mode-map
                        'repl-submit 'geiser-repl-maybe-send
                        'repl-newline 'geiser-repl--newline-and-indent
                        'repl-force-newline 'geiser-repl--newline-and-indent
                        'find-definition 'geiser-edit-symbol-at-point
                        'pop-definition 'geiser-pop-symbol-stack
                        'lookup-doc 'geiser-doc-symbol-at-point
                        'next-item 'geiser-repl-next-prompt
                        'prev-item 'geiser-repl-previous-prompt
                        'next-section 'geiser-repl-next-prompt
                        'prev-section 'geiser-repl-previous-prompt)

  (evil-collection-bind 'geiser-mode-map
                        'find-definition 'geiser-edit-symbol-at-point
                        'pop-definition 'geiser-pop-symbol-stack
                        'lookup-doc 'geiser-doc-symbol-at-point
                        'goto-repl 'geiser-mode-switch-to-repl)

  (evil-collection-define-key 'normal 'geiser-mode-map
    "gZ" 'geiser-mode-switch-to-repl-and-enter))

(provide 'evil-collection-geiser)
;;; evil-collection-geiser.el ends here
