;;; evil-collection-vc-git.el --- Evil bindings for Vc-Git -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, vc-git, tools

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
;; Evil bindings for Vc-Git.

;;; Code:
(require 'vc-git)
(require 'evil-collection)

(defconst evil-collection-vc-git-maps '(vc-git-mode-map))

;;;###autoload
(defun evil-collection-vc-git-setup ()
  "Set up `evil' bindings for `vc-git'."
  (evil-set-initial-state 'vc-git-log-view-mode 'normal)
  (evil-collection-theme-bind 'quit 'vc-git-log-view-mode-map 'quit-window)
  (evil-collection-define-key 'normal 'vc-git-log-view-mode-map
    "d" 'log-view-diff
    "D" 'log-view-diff-changeset
    (kbd "<tab>") 'log-view-toggle-entry-display)
  (evil-collection-theme-bind 'next-item    'vc-git-log-view-mode-map 'log-view-msg-next)
  (evil-collection-theme-bind 'prev-item    'vc-git-log-view-mode-map 'log-view-msg-prev)
  (evil-collection-theme-bind 'next-section 'vc-git-log-view-mode-map 'log-view-msg-next)
  (evil-collection-theme-bind 'prev-section 'vc-git-log-view-mode-map 'log-view-msg-prev))

(provide 'evil-collection-vc-git)
;;; evil-collection-vc-git.el ends here
