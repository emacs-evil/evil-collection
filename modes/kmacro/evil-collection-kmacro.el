;;; evil-collection-kmacro.el --- Evil bindings for kmacro menu -*- lexical-binding: t -*-

;; Copyright (C) 2024 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "30"))
;; Keywords: evil, kmacro, tools

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
;; Evil bindings for kmacro menu.

;;; Code:

(require 'evil-collection)
(require 'kmacro)

(defconst evil-collection-kmacro-maps '(kmacro-menu-mode-map))

(defun evil-collection-kmacro-setup ()
  "Set up `evil' bindings to `tab-bar'."
  (evil-set-initial-state 'kmacro-menu-mode 'normal)
  (evil-collection-bind 'kmacro-menu-mode-map 'edit 'kmacro-menu-edit-keys)
  (evil-collection-define-key 'normal 'kmacro-menu-mode-map
    ;; Edit
    "#" 'kmacro-menu-edit-position
    "c" 'kmacro-menu-edit-counter
    "f" 'kmacro-menu-edit-format

    "C" 'kmacro-menu-do-copy
    "D" 'kmacro-menu-do-delete)
  (evil-collection-bind 'kmacro-menu-mode-map
                        'mark 'kmacro-menu-mark
                        'unmark 'kmacro-menu-unmark
                        'unmark-all 'kmacro-menu-unmark-all
                        'mark-delete 'kmacro-menu-flag-for-deletion
                        'execute-marks 'kmacro-menu-do-flagged-delete
                        'action 'kmacro-menu-edit-column))

(provide 'evil-collection-kmacro)
;;; evil-collection-kmacro.el ends here
