;;; evil-collection-edebug.el --- Evil bindings for Edebug -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, edebug, tools

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
;; Evil bindings for `edebug-mode'.

;;; Code:
(require 'edebug)
(require 'evil-collection)

(defconst evil-collection-edebug-maps
  '(edebug-mode-map
    edebug-x-instrumented-function-list-mode-map
    edebug-x-breakpoint-list-mode-map))

;;;###autoload
(defun evil-collection-edebug-setup ()
  "Set up `evil' bindings for `edebug'."
  (evil-set-initial-state 'edebug-mode 'normal)

  (add-hook 'edebug-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key nil 'edebug-mode-map
    "g" nil
    "G" nil
    "h" nil)

  ;; FIXME: Seems like other minor modes will readily clash with `edebug'.
  ;; `lispyville' and `edebug' 's' key?
  (evil-collection-define-key 'normal 'edebug-mode-map
    ;; control
    "gs" 'edebug-step-mode
    "go" 'edebug-go-mode
    "gO" 'edebug-Go-nonstop-mode
    "t" 'edebug-trace-mode
    "T" 'edebug-Trace-fast-mode
    "C" 'edebug-Continue-fast-mode

    "f" 'edebug-forward-sexp
    "H" 'edebug-goto-here
    "I" 'edebug-instrument-callee

    ;; quit
    "Q" 'edebug-top-level-nonstop
    "a" 'abort-recursive-edit
    "S" 'edebug-stop

    ;; breakpoints
    "u" 'edebug-unset-breakpoint
    "B" 'edebug-next-breakpoint
    "x" 'edebug-set-conditional-breakpoint
    "X" 'edebug-set-global-break-condition

    ;; evaluation
    "r" 'edebug-previous-result
    (kbd "C-x C-e") 'edebug-eval-last-sexp
    "EL" 'edebug-visit-eval-list

    ;; views
    "WW" 'edebug-where
    "p" 'edebug-bounce-point
    "P" 'edebug-view-outside ;; same as v
    "WS" 'edebug-toggle-save-windows

    ;; misc
    "d" 'edebug-backtrace

    "-" 'negative-argument

    ;; statistics
    "=" 'edebug-temp-display-freq-count

    ;; GUD bindings
    (kbd "C-c C-s") 'edebug-step-mode
    (kbd "C-c C-n") 'edebug-next-mode
    (kbd "C-c C-c") 'edebug-go-mode

    (kbd "C-x SPC") 'edebug-set-breakpoint
    (kbd "C-c C-d") 'edebug-unset-breakpoint
    (kbd "C-c C-t") (lambda () (interactive) (edebug-set-breakpoint t))
    (kbd "C-c C-l") 'edebug-where)
  (evil-collection-bind 'edebug-mode-map
                        'quit 'top-level
                        'describe-mode 'edebug-help
                        'debug-continue 'edebug-continue-mode
                        'debug-step-over 'edebug-next-mode
                        'debug-step-into 'edebug-step-in
                        'debug-step-out 'edebug-step-out
                        'debug-breakpoint 'edebug-set-breakpoint
                        'debug-eval 'edebug-eval-expression)

  (with-eval-after-load 'edebug-x
    (evil-collection-define-key 'normal 'edebug-x-instrumented-function-list-mode-map
      "E" 'edebug-x-evaluate-function
      "Q" 'edebug-x-clear-data)
    (evil-collection-bind 'edebug-x-instrumented-function-list-mode-map
                          'quit 'quit-window
                          'action 'edebug-x-find-function)
    (evil-collection-define-key 'normal 'edebug-x-breakpoint-list-mode-map
      "Q" 'edebug-x-clear-data)
    (evil-collection-bind 'edebug-x-breakpoint-list-mode-map
                          'quit 'quit-window
                          'action 'edebug-x-visit-breakpoint
                          'delete 'edebug-x-kill-breakpoint
                          'delete-2 'edebug-x-kill-breakpoint)))

(provide 'evil-collection-edebug)
;;; evil-collection-edebug.el ends here
