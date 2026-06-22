;;; evil-collection-fj.el --- Evil bindings for fj -*- lexical-binding: t -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, fj, tools

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
;; Evil bindings for fj.el.

;;; Code:
(require 'evil-collection)
(require 'fj nil t)

(defvar fj-commits-mode-map)
(defvar fj-compose-comment-mode)
(defvar fj-compose-comment-mode-map)
(defvar fj-compose-mode)
(defvar fj-compose-mode-map)
(defvar fj-generic-map)
(defvar fj-generic-tl-map)
(defvar fj-issue-tl-mode-map)
(defvar fj-item-view-mode-map)
(defvar fj-notifications-mode-map)
(defvar fj-owned-issues-tl-mode-map)
(defvar fj-repo-tl-map)
(defvar fj-repo-tl-mode-map)
(defvar fj-user-repo-tl-mode-map)
(defvar fj-users-mode-map)

(defconst evil-collection-fj-maps
  '(fj-commits-mode-map
    fj-compose-comment-mode-map
    fj-compose-mode-map
    fj-generic-map
    fj-generic-tl-map
    fj-issue-tl-mode-map
    fj-item-view-mode-map
    fj-notifications-mode-map
    fj-owned-issues-tl-mode-map
    fj-repo-tl-map
    fj-repo-tl-mode-map
    fj-user-repo-tl-mode-map
    fj-users-mode-map))

(defconst evil-collection-fj-view-maps
  '(fj-commits-mode-map
    fj-item-view-mode-map
    fj-notifications-mode-map
    fj-users-mode-map))

(defconst evil-collection-fj-tabulated-list-maps
  '(fj-issue-tl-mode-map
    fj-owned-issues-tl-mode-map
    fj-repo-tl-map
    fj-repo-tl-mode-map
    fj-user-repo-tl-mode-map))

(defun evil-collection-fj--setup-compose-buffer ()
  "Set up Evil bindings for the current fj compose buffer."
  (evil-collection-bind-local 'quit-save   'fj-compose-send)
  (evil-collection-bind-local 'quit-cancel 'fj-compose-cancel)
  (evil-insert-state))

;;;###autoload
(defun evil-collection-fj-setup ()
  "Set up `evil' bindings for fj.el."
  (dolist (mode '(fj-commits-mode
                  fj-issue-tl-mode
                  fj-item-view-mode
                  fj-notifications-mode
                  fj-owned-issues-tl-mode
                  fj-repo-tl-mode
                  fj-user-repo-tl-mode
                  fj-users-mode))
    (evil-set-initial-state mode 'normal))

  (dolist (map evil-collection-fj-view-maps)
    (evil-collection-set-readonly-bindings map))
  (dolist (map evil-collection-fj-tabulated-list-maps)
    (evil-collection-set-readonly-bindings map))

  (evil-collection-bind 'fj-issue-tl-mode-map 'edit 'fj-item-edit)
  (evil-collection-bind 'fj-item-view-mode-map 'edit 'fj-item-edit)

  (evil-collection-define-key 'normal 'fj-generic-map
    "gb" 'fj-switch-to-buffer)
  (evil-collection-bind 'fj-generic-map
                        'cycle-next 'fj-next-tab-item
                        'cycle-previous 'fj-prev-tab-item
                        'action-other 'fj-browse-view
                        'next-item 'fj-item-next
                        'prev-item 'fj-item-prev
                        'next-section 'fj-item-next
                        'prev-section 'fj-item-prev)

  (evil-collection-define-key 'normal 'fj-generic-tl-map
    "gb" 'fj-switch-to-buffer
    "gm" 'imenu
    ">" 'fj-next-page
    "<" 'fj-prev-page)
  (evil-collection-bind 'fj-generic-tl-map
                        'cycle-next 'fj-next-tab-item
                        'cycle-previous 'fj-prev-tab-item
                        'action-other 'fj-tl-browse-entry)
  (evil-collection-bind 'fj-generic-map 'refresh    'fj-view-reload)
  (evil-collection-bind 'fj-generic-tl-map 'refresh 'fj-view-reload)

  (evil-collection-define-key 'normal 'fj-repo-tl-map
    "C" 'fj-create-issue
    "r" 'fj-repo-readme
    "g/" 'fj-repo-search
    "gm" 'imenu)
  (evil-collection-bind 'fj-repo-tl-map
                        'action 'fj-repo-list-issues
                        'action-other 'fj-tl-browse-entry
                        'action-stay 'fj-repo-list-pulls)
  (evil-collection-define-operator-key 'yank 'fj-repo-tl-map
    "c" 'fj-repo-copy-clone-url)

  (evil-collection-define-key 'normal 'fj-issue-tl-mode-map
    "c" 'fj-item-comment
    "C" 'fj-create-issue
    "E" 'fj-item-edit-title
    "g/" 'fj-list-issues-search
    "x" 'fj-item-close
    "X" 'fj-item-delete
    "u" 'fj-item-reopen
    "al" 'fj-item-label-add
    "gL" 'fj-repo-commit-log
    "gm" 'imenu)
  (evil-collection-bind 'fj-issue-tl-mode-map
                        'action 'fj-issues-tl-view
                        'action-other 'fj-tl-browse-entry)
  (evil-collection-define-operator-key 'yank 'fj-issue-tl-mode-map
    "c" 'fj-repo-copy-clone-url
    "u" 'fj-copy-item-url)

  (evil-collection-define-key 'normal 'fj-item-view-mode-map
    "c" 'fj-item-comment
    "E" 'fj-item-edit-title
    "r" 'fj-add-reaction
    "g/" 'fj-list-issues-search
    "x" 'fj-item-close
    "X" 'fj-item-delete
    "u" 'fj-item-reopen
    "al" 'fj-item-label-add
    "ar" 'fj-add-reaction
    "gd" 'fj-view-pull-diff
    "gL" 'fj-repo-commit-log
    "gM" 'fj-merge-pull
    ">" 'fj-item-view-more)
  (evil-collection-define-operator-key 'yank 'fj-item-view-mode-map
    "u" 'fj-copy-item-url)

  (evil-collection-define-key 'normal 'fj-notifications-mode-map
    "g/" 'fj-list-issues-search
    ">" 'fj-next-page
    "<" 'fj-prev-page)

  (evil-collection-define-key 'normal 'fj-commits-mode-map
    "g/" 'fj-list-issues-search
    "gL" 'fj-repo-commit-log)

  (evil-collection-define-key 'normal 'fj-users-mode-map
    "g/" 'fj-list-issues-search
    "gL" 'fj-repo-commit-log
    ">" 'fj-next-page
    "<" 'fj-prev-page)

  (add-hook 'fj-compose-comment-mode-hook
            #'evil-collection-fj--setup-compose-buffer)
  (add-hook 'fj-compose-mode-hook
            #'evil-collection-fj--setup-compose-buffer))

(provide 'evil-collection-fj)
;;; evil-collection-fj.el ends here
