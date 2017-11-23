;;; evil-edebug.el --- Evil bindings for Edebug -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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
(require 'evil-collection-util)

(evil-collection-util-set-initial-state edebug-mode motion)

(defun evil-edebug-setup ()
  "Set up `evil' bindings for `edebug'."
  (evil-define-key 'motion edebug-mode-map
    ;; control
    " " nil
    "s" 'edebug-step-mode
    "n" 'edebug-next-mode
    "go" 'edebug-go-mode
    "gn" 'edebug-Go-nonstop-mode
    "t" 'edebug-trace-mode
    "T" 'edebug-Trace-fast-mode
    "c" 'edebug-continue-mode
    "C" 'edebug-Continue-fast-mode
    "f" 'edebug-forward-sexp
    "H" 'edebug-goto-here
    "I" 'edebug-instrument-callee
    "i" 'edebug-step-in
    "o" 'edebug-step-out

    ;; quitting and stopping
    "q" 'top-level
    "Q" 'edebug-top-level-nonstop
    "a" 'abort-recursive-edit
    "S" 'edebug-stop

    ;; breakpoints
    "b" 'edebug-set-breakpoint
    "u" 'edebug-unset-breakpoint
    "B" 'edebug-next-breakpoint
    "x" 'edebug-set-conditional-breakpoint
    "X" 'edebug-set-global-break-condition

    ;; evaluation
    "r" 'edebug-previous-result
    "EE" 'edebug-eval-expression
    (kbd "C-x C-e") 'edebug-eval-last-sexp
    "EL" 'edebug-visit-eval-list

    ;; views
    "WW" 'edebug-where
    "p" 'edebug-bounce-point
    "P" 'edebug-view-outside ;; same as v
    "WS" 'edebug-toggle-save-windows

    ;; misc
    "?" 'edebug-help
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

  (with-eval-after-load 'edebug-x
    (evil-define-key 'motion edebug-x-instrumented-function-list-mode-map
      "E" 'edebug-x-evaluate-function
      "Q" 'edebug-x-clear-data
      (kbd "<return>") 'edebug-x-find-function
      "q" 'kill-current-buffer-and-its-windows)
    (evil-define-key 'motion edebug-x-breakpoint-list-mode-map
      (kbd "<return>") 'edebug-x-visit-breakpoint
      "x" 'edebug-x-kill-breakpoint
      "Q" 'edebug-x-clear-data
      "q" 'kill-current-buffer-and-its-windows)))

(provide 'evil-edebug)
;;; evil-edebug.el ends here
