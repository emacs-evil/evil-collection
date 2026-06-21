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

  (evil-collection-bind 'quit 'alchemist-compile-mode-map 'quit-window)

  (evil-collection-bind 'quit 'alchemist-eval-mode-map 'quit-window)

  (evil-collection-bind 'quit 'alchemist-execute-mode-map 'quit-window)

  (evil-collection-bind 'quit 'alchemist-message-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-help-minor-mode-map
    "m" 'alchemist-help-module
    "s" 'alchemist-help
    "gh" 'alchemist-help-history)
  (evil-collection-bind 'quit          'alchemist-help-minor-mode-map 'quit-window)
  (evil-collection-bind 'describe-mode 'alchemist-help-minor-mode-map 'alchemist-help-minor-mode-key-binding-summary)

  (evil-collection-bind 'find-definition 'alchemist-help-minor-mode-map 'alchemist-goto-definition-at-point)
  (evil-collection-bind 'lookup-doc      'alchemist-help-minor-mode-map 'alchemist-help-search-at-point)

  (evil-collection-bind 'quit 'alchemist-macroexpand-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-refcard-mode-map
    "gd" 'alchemist-refcard--describe-funtion-at-point
    "g?" 'alchemist-refcard--describe-funtion-at-point)
  (evil-collection-bind 'quit 'alchemist-refcard-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'alchemist-mix-mode-map
    "i" 'alchemist-mix-send-input-to-mix-process)
  (evil-collection-bind 'quit    'alchemist-mix-mode-map 'quit-window)
  (evil-collection-bind 'refresh 'alchemist-mix-mode-map 'alchemist-mix-rerun-last-task)

  (evil-collection-bind 'next-item    'alchemist-test-report-mode-map 'alchemist-test-next-result)
  (evil-collection-bind 'prev-item    'alchemist-test-report-mode-map 'alchemist-test-previous-result)
  (evil-collection-bind 'next-section 'alchemist-test-report-mode-map 'alchemist-test-next-stacktrace-file)
  (evil-collection-bind 'prev-section 'alchemist-test-report-mode-map 'alchemist-test-previous-stacktrace-file)
  (evil-collection-define-key 'normal 'alchemist-test-report-mode-map
    "t" 'toggle-truncate-lines
    (kbd "C-c C-k") 'alchemist-report-interrupt-current-process)
  (evil-collection-bind 'next-section-2 'alchemist-test-report-mode-map 'alchemist-test-next-result)
  (evil-collection-bind 'prev-section-2 'alchemist-test-report-mode-map 'alchemist-test-previous-result)
  (evil-collection-bind 'quit    'alchemist-test-report-mode-map 'quit-window)
  (evil-collection-bind 'refresh 'alchemist-test-report-mode-map 'alchemist-mix-rerun-last-test)

  (evil-collection-bind 'next-section 'alchemist-mode-map 'alchemist-goto-jump-to-next-def-symbol)
  (evil-collection-bind 'prev-section 'alchemist-mode-map 'alchemist-goto-jump-to-previous-def-symbol)

  (evil-collection-bind 'find-definition 'alchemist-mode-map 'alchemist-goto-definition-at-point)
  (evil-collection-bind 'pop-definition  'alchemist-mode-map 'alchemist-goto-jump-back)
  (evil-collection-bind 'lookup-doc      'alchemist-mode-map 'alchemist-help-search-at-point)
  (evil-collection-bind 'goto-repl       'alchemist-mode-map 'alchemist-iex-run)
  (evil-collection-bind 'describe-mode   'alchemist-mode-map 'alchemist-help))

(provide 'evil-collection-alchemist)
;;; evil-collection-alchemist.el ends here
