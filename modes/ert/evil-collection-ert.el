;;; evil-collection-ert.el --- Bindings for `ert' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools

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
;; Bindings for `ert'.

;;; Code:
(require 'ert)
(require 'evil-collection)

(defconst evil-collection-ert-maps '(ert-results-mode-map))

;;;###autoload
(defun evil-collection-ert-setup ()
  "Set up `evil' bindings for `ert'."
  (evil-collection-inhibit-insert-state 'ert-results-mode-map)
  (evil-set-initial-state 'ert-results-mode 'normal)

  (evil-collection-define-key 'normal 'ert-results-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    ;; Stuff that's not in the menu.
    "J" 'ert-results-jump-between-summary-and-result
    "L" 'ert-results-toggle-printer-limits-for-test-at-point
    ;; Stuff that is in the menu.
    "R" 'ert-results-rerun-all-tests
    "r" 'ert-results-rerun-test-at-point
    "d" 'ert-results-rerun-test-at-point-debugging-errors
    "." 'ert-results-find-test-at-point-other-window
    "B" 'ert-results-pop-to-backtrace-for-test-at-point
    "M" 'ert-results-pop-to-messages-for-test-at-point
    "s" 'ert-results-pop-to-should-forms-for-test-at-point
    "T" 'ert-results-pop-to-timings)

  (evil-collection-bind 'ert-results-mode-map
                        'next-item 'ert-results-next-test
                        'prev-item 'ert-results-previous-test
                        'next-section 'ert-results-next-test
                        'prev-section 'ert-results-previous-test
                        'lookup-doc 'ert-results-describe-test-at-point
                        'describe-mode 'ert-results-describe-test-at-point
                        'refresh 'ert-results-rerun-all-tests
                        'find-definition 'ert-results-find-test-at-point-other-window
                        'delete-2 'ert-delete-test))

(provide 'evil-collection-ert)
;;; evil-collection-ert.el ends here
