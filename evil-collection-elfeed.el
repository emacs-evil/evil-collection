;;; evil-collection-elfeed.el --- Evil bindings for elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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

(defconst evil-collection-elfeed-maps '(elfeed-search-mode-map
                                        elfeed-show-mode-map))

(defun evil-collection-elfeed-setup ()
  "Set up `evil' bindings for `elfeed'."

  (evil-collection-inhibit-insert-state 'elfeed-search-mode-map)
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    ;; open
    (kbd "<return>") 'elfeed-search-show-entry
    (kbd "S-<return>") 'elfeed-search-browse-url
    "go" 'elfeed-search-browse-url

    "y" 'elfeed-search-yank

    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command

    ;; filter
    "s" 'elfeed-search-live-filter
    "S" 'elfeed-search-set-filter

    ;; refresh
    "gR" 'elfeed-search-fetch ; TODO: Which update function is more useful?
    "gr" 'elfeed-search-update--force

    ;; quit
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'quit-window)

  (evil-collection-define-key '(normal visual) 'elfeed-search-mode-map
    "+" 'elfeed-search-tag-all
    "-" 'elfeed-search-untag-all
    "U" 'elfeed-search-tag-all-unread
    "u" 'elfeed-search-untag-all-unread)

  (evil-collection-inhibit-insert-state 'elfeed-show-mode-map)
  (evil-set-initial-state 'elfeed-show-mode 'normal)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    (kbd "S-<return>") 'elfeed-show-visit
    "go" 'elfeed-show-visit

    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command

    ;; filter
    "s" 'elfeed-show-new-live-search

    "+" 'elfeed-show-tag
    "-" 'elfeed-show-untag

    "A" 'elfeed-show-add-enclosure-to-playlist
    "P" 'elfeed-show-play-enclosure
    "d" 'elfeed-show-save-enclosure

    "]" 'elfeed-show-next
    "[" 'elfeed-show-prev
    "gj" 'elfeed-show-next
    "gk" 'elfeed-show-prev
    (kbd "C-j") 'elfeed-show-next
    (kbd "C-k") 'elfeed-show-prev

    ;; refresh
    "gr" 'elfeed-show-refresh

    ;; quit
    "q" 'elfeed-kill-buffer
    "ZQ" 'elfeed-kill-buffer
    "ZZ" 'elfeed-kill-buffer)

  (evil-collection-define-key 'operator 'elfeed-show-mode-map
    ;; Like `eww'.
    "u" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (memq evil-this-operator
                                evil-collection-yank-operators)
                      (setq evil-inhibit-operator t)
                      #'elfeed-show-yank)))))

(provide 'evil-collection-elfeed)
;;; evil-collection-elfeed.el ends here
