;;; evil-collection-bookmark.el --- Evil bindings for bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, bookmark, tools

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
;; Evil bindings for bookmarks.

;;; Code:
(require 'evil-collection)
(require 'bookmark)

(defconst evil-collection-bookmark-maps '(bookmark-bmenu-mode-map))

(defun evil-collection-bookmark-setup ()
  "Set up `evil' bindings for `bookmark'."
  (evil-set-initial-state 'bookmark-bmenu-mode 'normal)

  (evil-collection-define-key 'normal 'bookmark-bmenu-mode-map
    "q" 'quit-window
    "gr" 'revert-buffer
    "g?" 'describe-mode

    "j" 'next-line
    "p" 'previous-line
    "J" 'bookmark-bmenu-this-window
    "2" 'bookmark-bmenu-2-window
    "1" 'bookmark-bmenu-1-window
    "x" 'bookmark-bmenu-execute-deletions
    "d" 'bookmark-bmenu-delete
    "/" 'bookmark-bmenu-search
    "r" 'bookmark-bmenu-rename
    "R" 'bookmark-bmenu-relocate
    "L" 'bookmark-bmenu-load
    "t" 'bookmark-bmenu-toggle-filenames
    "a" 'bookmark-bmenu-show-annotation
    "A" 'bookmark-bmenu-show-all-annotations
    "s" 'bookmark-bmenu-save
    "W" 'bookmark-bmenu-locate
    "E" 'bookmark-bmenu-edit-annotation
    "D" 'bookmark-bmenu-delete-backwards

    ;; mark
    "u" 'bookmark-bmenu-unmark
    "m" 'bookmark-bmenu-mark

    ;; open
    "o" 'bookmark-bmenu-select
    "O" 'bookmark-bmenu-other-window
    "go" 'bookmark-bmenu-other-window
    "gO" 'bookmark-bmenu-switch-other-window
    (kbd "<return>") 'bookmark-bmenu-this-window
    (kbd "S-<return>") 'bookmark-bmenu-other-window
    (kbd "M-<return>") 'bookmark-bmenu-switch-other-window))


(provide 'evil-collection-bookmark)
;;; evil-collection-bookmark.el ends here
