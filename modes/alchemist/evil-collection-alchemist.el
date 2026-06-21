;;; evil-collection-alchemist.el --- Bindings for `alchemist' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, alchemist, tools

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
;;; Bindings for `alchemist'.

;;; Code:
(require 'evil-collection)
(require 'alchemist nil t)

(defconst evil-collection-alchemist-maps '(alchemist-compile-mode-map
                                           alchemist-eval-mode-map
                                           alchemist-execute-mode-map
                                           alchemist-message-mode-map
                                           alchemist-help-minor-mode-map
                                           alchemist-macroexpand-mode-map
                                           alchemist-mix-mode-map
                                           alchemist-test-report-mode-map
                                           alchemist-mode-map))


;;;###autoload
(defun evil-collection-alchemist-setup ()
  "Set up `evil' bindings for `alchemist'."
  (evil-set-initial-state 'alchemist-compile-mode 'normal)
  (evil-set-initial-state 'alchemist-eval-mode 'normal)
  (evil-set-initial-state 'alchemist-execute-mode 'normal)
  (evil-set-initial-state 'alchemist-message-mode 'normal)
  (evil-set-initial-state 'alchemist-help-minor-mode 'normal)
  (evil-set-initial-state 'alchemist-macroexpand-mode 'normal)
  (evil-set-initial-state 'alchemist-refcard-mode 'normal)
  (evil-set-initial-state 'alchemist-mix-mode 'normal)
  (evil-set-initial-state 'alchemist-test-mode 'normal)
  (evil-set-initial-state 'alchemist-test-report-mode 'normal)

  (evil-collection-theme-bind 'quit 'alchemist-compile-mode-map 'quit-window)

  (evil-collection-theme-bind 'quit 'alchemist-eval-mode-map 'quit-window)

  (evil-collection-theme-bind 'quit 'alchemist-execute-mode-map 'quit-window)

  (evil-collection-theme-bind 'quit 'alchemist-message-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-help-minor-mode-map
    "m" 'alchemist-help-module
    "s" 'alchemist-help
    "gh" 'alchemist-help-history)
  (evil-collection-theme-bind 'quit          'alchemist-help-minor-mode-map 'quit-window)
  (evil-collection-theme-bind 'describe-mode 'alchemist-help-minor-mode-map 'alchemist-help-minor-mode-key-binding-summary)

  (evil-collection-theme-bind 'find-definition 'alchemist-help-minor-mode-map 'alchemist-goto-definition-at-point)
  (evil-collection-theme-bind 'lookup-doc      'alchemist-help-minor-mode-map 'alchemist-help-search-at-point)

  (evil-collection-theme-bind 'quit 'alchemist-macroexpand-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-refcard-mode-map
    "gd" 'alchemist-refcard--describe-funtion-at-point
    "g?" 'alchemist-refcard--describe-funtion-at-point)
  (evil-collection-theme-bind 'quit 'alchemist-refcard-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-mix-mode-map
    "i" 'alchemist-mix-send-input-to-mix-process
    "gr" 'alchemist-mix-rerun-last-task)
  (evil-collection-theme-bind 'quit 'alchemist-mix-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-test-report-mode-map
    "t" 'toggle-truncate-lines
    "gr" 'alchemist-mix-rerun-last-test
    "gj" 'alchemist-test-next-result
    "gk" 'alchemist-test-previous-result
    (kbd "C-j") 'alchemist-test-next-result
    (kbd "C-k") 'alchemist-test-previous-result
    "]]" 'alchemist-test-next-stacktrace-file
    "[[" 'alchemist-test-previous-stacktrace-file
    (kbd "C-c C-k") 'alchemist-report-interrupt-current-process)
  (evil-collection-theme-bind 'quit 'alchemist-test-report-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-mode-map
    (kbd "C-j") 'alchemist-goto-jump-to-next-def-symbol
    (kbd "C-k") 'alchemist-goto-jump-to-previous-def-symbol)

  (evil-collection-theme-bind 'find-definition 'alchemist-mode-map 'alchemist-goto-definition-at-point)
  (evil-collection-theme-bind 'pop-definition  'alchemist-mode-map 'alchemist-goto-jump-back)
  (evil-collection-theme-bind 'lookup-doc      'alchemist-mode-map 'alchemist-help-search-at-point)
  (evil-collection-theme-bind 'goto-repl       'alchemist-mode-map 'alchemist-iex-run)
  (evil-collection-theme-bind 'describe-mode   'alchemist-mode-map 'alchemist-help))

(provide 'evil-collection-alchemist)
;;; evil-collection-alchemist.el ends here
