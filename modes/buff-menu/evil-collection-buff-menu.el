;;; evil-collection-buff-menu.el --- Bindings for `buff-menu' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

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
;;; Bindings for `buff-menu'.

;;; Code:
(require 'evil-collection)

(defconst evil-collection-buff-menu-maps '(Buffer-menu-mode-map))

;;;###autoload
(defun evil-collection-buff-menu-setup ()
  "Set up `evil' bindings for `buff-menu'.."
  (evil-set-initial-state 'Buffer-menu-mode 'normal)

  (evil-collection-define-key 'normal 'Buffer-menu-mode-map
    "s" 'Buffer-menu-save
    [mouse-2] 'Buffer-menu-mouse-select
    [follow-link] 'mouse-face
    "o" 'tabulated-list-sort
    "gv" 'Buffer-menu-select
    "gV" 'Buffer-menu-view
    "v" 'evil-visual-char

    "X" 'Buffer-menu-bury

    ;; Default ones, unchanged. Redundant ones commented
    "2" 'Buffer-menu-2-window
    "1" 'Buffer-menu-1-window
    (kbd "C-k") 'Buffer-menu-delete
    (kbd "C-d") 'Buffer-menu-delete-backwards
    (kbd "<delete>") 'Buffer-menu-backup-unmark
    "~" 'Buffer-menu-not-modified
    "t" 'Buffer-menu-visit-tags-table
    "%" 'Buffer-menu-toggle-read-only
    "T" 'Buffer-menu-toggle-files-only
    (kbd "M-s a C-s") 'Buffer-menu-isearch-buffers
    (kbd "M-s a M-C-s") 'Buffer-menu-isearch-buffers-regexp
    (kbd "M-s a C-o") 'Buffer-menu-multi-occur)
  (evil-collection-bind 'Buffer-menu-mode-map
                        'mark 'Buffer-menu-mark
                        'unmark 'Buffer-menu-unmark
                        'unmark-all 'Buffer-menu-unmark-all
                        'mark-delete 'Buffer-menu-delete
                        'execute-marks 'Buffer-menu-execute
                        'action 'Buffer-menu-this-window
                        'action-other 'Buffer-menu-other-window
                        'quit-save 'quit-window
                        'quit-cancel 'evil-quit
                        'refresh 'revert-buffer))

(provide 'evil-collection-buff-menu)
;;; evil-collection-buff-menu.el ends here
