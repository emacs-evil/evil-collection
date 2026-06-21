;;; evil-collection-elfeed.el --- Evil bindings for elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, elfeed, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for elfeed.

;;; Code:
(require 'elfeed nil t)
(require 'evil-collection)

(defvar elfeed-search-mode-map)
(defvar elfeed-show-mode-map)
(defvar elfeed-tree-mode-map)

(defconst evil-collection-elfeed-maps '(elfeed-search-mode-map
                                        elfeed-show-mode-map
                                        elfeed-tree-mode-map))

;;;###autoload
(defun evil-collection-elfeed-setup ()
  "Set up `evil' bindings for `elfeed'."

  (evil-collection-set-readonly-bindings 'elfeed-search-mode-map)
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "y" 'elfeed-search-yank

    ;; filter
    "s" 'elfeed-search-live-filter
    "S" 'elfeed-search-set-filter
    "c" 'elfeed-search-clear-filter

    ;; quit -- `q'/`ZZ' default to `quit-window' via
    ;; `evil-collection-set-readonly-bindings'.
    "ZQ" 'quit-window)

  (evil-collection-bind 'elfeed-search-mode-map  'scroll-down 'scroll-up-command)
  (evil-collection-bind 'elfeed-search-mode-map    'scroll-up 'scroll-down-command)
  (evil-collection-bind 'elfeed-search-mode-map       'action 'elfeed-search-show-entry)
  (evil-collection-bind 'elfeed-search-mode-map 'action-other 'elfeed-search-browse-url)
  (evil-collection-bind 'elfeed-search-mode-map 'refresh-all 'elfeed-search-fetch)
  (evil-collection-bind 'elfeed-search-mode-map     'refresh 'revert-buffer)

  ;; Refresh fallback for elfeed before commit 518e5bd3, where
  ;; `revert-buffer-function' is not wired up.
  (unless (fboundp 'elfeed-search--update-force)
    (evil-collection-bind 'elfeed-search-mode-map 'refresh 'elfeed-search-update--force))

  ;; Quit fallback for elfeed before commit 2ef14c92, where
  ;; `quit-window-hook' does not run `elfeed-db-save'.
  (when (and (fboundp 'elfeed-search-quit-window)
             (not (eq (indirect-function 'elfeed-search-quit-window)
                      (symbol-function 'quit-window))))
    (evil-collection-bind 'elfeed-search-mode-map        'quit 'elfeed-search-quit-window)
    (evil-collection-bind 'elfeed-search-mode-map   'quit-save 'elfeed-search-quit-window)
    (evil-collection-bind 'elfeed-search-mode-map 'quit-cancel 'elfeed-search-quit-window))

  (evil-collection-define-key '(normal visual) 'elfeed-search-mode-map
    "+" 'elfeed-search-tag-all
    "-" 'elfeed-search-untag-all
    "U" 'elfeed-search-tag-all-unread
    "u" 'elfeed-search-untag-all-unread)

  (evil-collection-set-readonly-bindings 'elfeed-show-mode-map)
  (evil-set-initial-state 'elfeed-show-mode 'normal)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    (kbd "<tab>") 'elfeed-show-next-link

    ;; filter
    "s" 'elfeed-show-new-live-search

    "+" 'elfeed-show-tag
    "-" 'elfeed-show-untag

    "A" 'elfeed-show-add-enclosure-to-playlist
    "P" 'elfeed-show-play-enclosure
    "d" 'elfeed-show-save-enclosure)
  (evil-collection-bind 'elfeed-show-mode-map  'scroll-down 'scroll-up-command)
  (evil-collection-bind 'elfeed-show-mode-map    'scroll-up 'scroll-down-command)
  (evil-collection-bind 'elfeed-show-mode-map 'action-other 'elfeed-show-visit)
  (evil-collection-bind 'elfeed-show-mode-map    'next-item 'elfeed-show-next)
  (evil-collection-bind 'elfeed-show-mode-map    'prev-item 'elfeed-show-prev)
  (evil-collection-bind 'elfeed-show-mode-map 'next-section 'elfeed-show-next)
  (evil-collection-bind 'elfeed-show-mode-map 'prev-section 'elfeed-show-prev)
  (evil-collection-bind 'elfeed-show-mode-map         'quit 'elfeed-kill-buffer)
  (evil-collection-bind 'elfeed-show-mode-map    'quit-save 'elfeed-kill-buffer)
  (evil-collection-bind 'elfeed-show-mode-map  'quit-cancel 'elfeed-kill-buffer)
  (evil-collection-bind 'elfeed-show-mode-map      'refresh 'elfeed-show-refresh)

  ;; yu, like `eww'
  (evil-collection-define-operator-key 'yank 'elfeed-show-mode-map
    "u" 'elfeed-show-yank)

  (evil-collection-set-readonly-bindings 'elfeed-tree-mode-map)
  (evil-set-initial-state 'elfeed-tree-mode 'normal)
  (evil-collection-bind 'elfeed-tree-mode-map 'action 'elfeed-tree-search))

(provide 'evil-collection-elfeed)
;;; evil-collection-elfeed.el ends here
