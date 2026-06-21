;;; evil-collection-geiser.el --- Bindings for `geiser' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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

  (evil-collection-bind 'quit 'geiser-debug-mode-map 'quit-window)

  (evil-collection-bind 'find-definition 'geiser-doc-mode-map 'geiser-edit-symbol-at-point)
  (evil-collection-bind 'pop-definition  'geiser-doc-mode-map 'geiser-pop-symbol-stack)

  (evil-collection-bind 'goto-repl 'geiser-doc-mode-map 'geiser-doc-switch-to-repl)

  (evil-collection-define-key 'normal 'geiser-doc-mode-map
    (kbd "<tab>") 'forward-button
    (kbd "<S-tab>") 'backward-button
    ">" 'geiser-doc-next
    "<" 'geiser-doc-previous
    "x" 'geiser-doc-kill-page
    "X" 'geiser-doc-clean-history)
  (evil-collection-bind 'next-item    'geiser-doc-mode-map 'forward-button)
  (evil-collection-bind 'prev-item    'geiser-doc-mode-map 'backward-button)
  (evil-collection-bind 'next-section 'geiser-doc-mode-map 'geiser-doc-next-section)
  (evil-collection-bind 'prev-section   'geiser-doc-mode-map 'geiser-doc-previous-section)
  (evil-collection-bind 'next-section-2 'geiser-doc-mode-map 'forward-button)
  (evil-collection-bind 'prev-section-2 'geiser-doc-mode-map 'backward-button)
  (evil-collection-bind 'quit    'geiser-doc-mode-map 'View-quit)
  (evil-collection-bind 'refresh 'geiser-doc-mode-map 'geiser-doc-refresh)

  (evil-collection-bind 'repl-submit        'geiser-repl-mode-map 'geiser-repl-maybe-send)
  (evil-collection-bind 'repl-newline       'geiser-repl-mode-map 'geiser-repl--newline-and-indent)
  (evil-collection-bind 'repl-force-newline 'geiser-repl-mode-map 'geiser-repl--newline-and-indent)

  (evil-collection-bind 'find-definition 'geiser-repl-mode-map 'geiser-edit-symbol-at-point)
  (evil-collection-bind 'pop-definition  'geiser-repl-mode-map 'geiser-pop-symbol-stack)

  (evil-collection-bind 'lookup-doc 'geiser-repl-mode-map 'geiser-doc-symbol-at-point)

  (evil-collection-bind 'next-item    'geiser-repl-mode-map 'geiser-repl-next-prompt)
  (evil-collection-bind 'prev-item    'geiser-repl-mode-map 'geiser-repl-previous-prompt)
  (evil-collection-bind 'next-section 'geiser-repl-mode-map 'geiser-repl-next-prompt)
  (evil-collection-bind 'prev-section 'geiser-repl-mode-map 'geiser-repl-previous-prompt)

  (evil-collection-bind 'find-definition 'geiser-mode-map 'geiser-edit-symbol-at-point)
  (evil-collection-bind 'pop-definition  'geiser-mode-map 'geiser-pop-symbol-stack)
  (evil-collection-bind 'lookup-doc      'geiser-mode-map 'geiser-doc-symbol-at-point)

  (evil-collection-bind 'goto-repl 'geiser-mode-map 'geiser-mode-switch-to-repl)

  (evil-collection-define-key 'normal 'geiser-mode-map
    "gZ" 'geiser-mode-switch-to-repl-and-enter))

(provide 'evil-collection-geiser)
;;; evil-collection-geiser.el ends here
