;;; evil-collection-tablist.el --- Evil bindings for Tablist -*- lexical-binding: t -*-

;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tablist, tools

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
;; Evil bindings for Tablist.

;;; Code:
(require 'tablist nil t)
(require 'evil-collection)

(defconst evil-collection-tablist-maps '(tablist-mode-map tablist-minor-mode-map))

(defvar tablist-mode-regexp-map)
(defvar tablist-mode-mark-map)
(defvar tablist-mode-filter-map)

;;;###autoload
(defun evil-collection-tablist-setup ()
  "Set up `evil' bindings for `tablist'."

  (let ((common-bindings
         (list
          "U"  'tablist-unmark-all-marks
          "g%" tablist-mode-regexp-map
          "g*" tablist-mode-mark-map
          "g/" tablist-mode-filter-map
          "gr" 'tablist-revert
          "K"  'tablist-do-kill-lines
          "m"  'tablist-mark-forward
          "S"  'tablist-sort
          "t"  'tablist-toggle-marks
          "u"  'tablist-unmark-forward)))

    (apply #'evil-collection-define-key 'normal 'tablist-mode-map
           (kbd "RET") 'tablist-find-entry
           "d" 'tablist-flag-forward
           "f" 'tablist-find-entry
           "X" 'tablist-do-delete
           "x" 'tablist-do-flagged-delete
           common-bindings)
    
    (apply #'evil-collection-define-key 'normal 'tablist-minor-mode-map
           "q" 'tablist-quit
           common-bindings)))

(provide 'evil-collection-tablist)
;;; evil-collection-tablist.el ends here
