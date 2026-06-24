;;; evil-collection-alchemist.el --- Bindings for `alchemist' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

  (evil-collection-bind 'alchemist-compile-mode-map 'quit 'quit-window)

  (evil-collection-bind 'alchemist-eval-mode-map 'quit 'quit-window)

  (evil-collection-bind 'alchemist-execute-mode-map 'quit 'quit-window)

  (evil-collection-bind 'alchemist-message-mode-map 'quit 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-help-minor-mode-map
    "m" 'alchemist-help-module
    "s" 'alchemist-help
    "gh" 'alchemist-help-history)
  (evil-collection-bind 'alchemist-help-minor-mode-map
                        'quit 'quit-window
                        'describe-mode 'alchemist-help-minor-mode-key-binding-summary
                        'find-definition 'alchemist-goto-definition-at-point
                        'lookup-doc 'alchemist-help-search-at-point)

  (evil-collection-bind 'alchemist-macroexpand-mode-map 'quit 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-refcard-mode-map
    "gd" 'alchemist-refcard--describe-funtion-at-point)
  (evil-collection-bind 'alchemist-refcard-mode-map
                        'quit 'quit-window
                        'describe-mode 'alchemist-refcard--describe-funtion-at-point)

  (evil-collection-define-key 'normal 'alchemist-mix-mode-map
    "i" 'alchemist-mix-send-input-to-mix-process)
  (evil-collection-bind 'alchemist-mix-mode-map
                        'quit 'quit-window
                        'refresh 'alchemist-mix-rerun-last-task)

  (evil-collection-bind 'alchemist-test-report-mode-map
                        'next-item 'alchemist-test-next-result
                        'prev-item 'alchemist-test-previous-result
                        'next-section 'alchemist-test-next-stacktrace-file
                        'prev-section 'alchemist-test-previous-stacktrace-file)
  (evil-collection-define-key 'normal 'alchemist-test-report-mode-map
    (kbd "C-c C-k") 'alchemist-report-interrupt-current-process)
  (evil-collection-bind 'alchemist-test-report-mode-map
                        'next-section-2 'alchemist-test-next-result
                        'prev-section-2 'alchemist-test-previous-result
                        'quit 'quit-window
                        'refresh 'alchemist-mix-rerun-last-test
                        'toggle 'toggle-truncate-lines)

  (evil-collection-bind 'alchemist-mode-map
                        'next-section 'alchemist-goto-jump-to-next-def-symbol
                        'prev-section 'alchemist-goto-jump-to-previous-def-symbol
                        'find-definition 'alchemist-goto-definition-at-point
                        'pop-definition 'alchemist-goto-jump-back
                        'lookup-doc 'alchemist-help-search-at-point
                        'goto-repl 'alchemist-iex-run
                        'describe-mode 'alchemist-help))

(provide 'evil-collection-alchemist)
;;; evil-collection-alchemist.el ends here
