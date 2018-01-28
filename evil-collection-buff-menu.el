;;; evil-collection-buff-menu.el --- Bindings for `buff-menu'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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
(defun evil-collection-Buffer-menu-unmark-all ()
  "Unmark all marked items."
  ) ;; TODO: add this function

(defun evil-collection-buff-menu-setup ()
  "Set up `evil' bindings for `buff-menu'.."

  (evil-set-initial-state 'Buffer-menu-mode 'normal)
  (evil-add-hjkl-bindings Buffer-menu-mode-map 'normal)

  (evil-define-key 'normal Buffer-menu-mode-map
    "ZQ" 'evil-quit
    "ZZ" 'quit-window
    "gr" 'revert-buffer
    "go" 'Buffer-menu-other-window
    "d"  'Buffer-menu-delete
    "u"  'Buffer-menu-unmark 
    "U"  'evil-collection-Buffer-menu-unmark-all ;; TODO: add this
    "m"  'Buffer-menu-mark
    "s" 'Buffer-menu-save
    [mouse-2] 'Buffer-menu-mouse-select
    [follow-link] 'mouse-face
    "x" 'Buffer-menu-execute

    ;; Ones I am not sure about
    "o" 'Buffer-menu-other-window
    ;; The one I don't want
    ;; " " 'next-line
    
    ;; Default ones
    "v" 'Buffer-menu-select
    "2" 'Buffer-menu-2-window
    "1" 'Buffer-menu-1-window
    "f" 'Buffer-menu-this-window
    "e" 'Buffer-menu-this-window
    "\C-m" 'Buffer-menu-this-window
    "\C-k" 'Buffer-menu-delete
    "\C-d" 'Buffer-menu-delete-backwards
    "\177" 'Buffer-menu-backup-unmark
    "~" 'Buffer-menu-not-modified
    "t" 'Buffer-menu-visit-tags-table
    "%" 'Buffer-menu-toggle-read-only
    "b" 'Buffer-menu-bury
    "V" 'Buffer-menu-view
    "T" 'Buffer-menu-toggle-files-only
    (kbd "M-s a C-s")   'Buffer-menu-isearch-buffers
    (kbd "M-s a M-C-s") 'Buffer-menu-isearch-buffers-regexp
    (kbd "M-s a C-o") 'Buffer-menu-multi-occur))

(provide 'evil-collection-buff-menu)
;;; evil-collection-buff-menu.el ends here
