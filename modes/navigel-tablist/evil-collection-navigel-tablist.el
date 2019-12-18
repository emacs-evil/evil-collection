;;; evil-collection-navigel-tablist.el --- Evil bindings for Navigel Tablist -*- lexical-binding: t -*-

;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, navigel, tablist, tools

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
;; Evil bindings for Navigel Tablist.

;;; Code:
(require 'navigel nil t)
(require 'evil-collection)

(defconst evil-collection-navigel-tablist-maps '(navigel-tablist-mode-map))

(defvar tablist-mode-regexp-map)
(defvar tablist-mode-mark-map)
(defvar tablist-mode-filter-map)

;;;###autoload
(defun evil-collection-tablist-setup ()
  "Set up `evil' bindings for `tablist'."

  (evil-collection-define-key 'normal 'navigel-tablist-mode-map
    "*" 'tablist-mark-prompt
    "^" 'navigel-open-parent
    "<" 'tablist-shrink-column
    ">" 'tablist-enlarge-column
    "K" 'tablist-do-kill-lines
    "S" 'tabulated-list-sort
    "U" 'tablist-unmark-all-marks
    "\t" 'tablist-forward-column
    "g%" tablist-mode-regexp-map
    "g*" tablist-mode-mark-map
    "g/" tablist-mode-filter-map
    "gr" 'tablist-revert
    "m" 'tablist-mark-forward
    "q" 'tablist-quit
    "s" 'tablist-sort
    "t" 'tablist-toggle-marks
    "u" 'tablist-unmark-forward
    (kbd "TAB") 'tablist-forward-column
    [backtab] 'tablist-backward-column))

(provide 'evil-collection-navigel-tablist)
;;; evil-collection-navigel-tablist.el ends here
