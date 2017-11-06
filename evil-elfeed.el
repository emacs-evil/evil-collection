;;; evil-elfeed.el --- Evil bindings for elfeed  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, compile, evil
;; HomePage: https://github.com/jojojames/evil-collection

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

;;; Code:

(require 'evil)
(require 'elfeed)

(defun evil-elfeed-set-keys ()
  (evil-set-initial-state 'elfeed-search-mode 'motion)

  (evil-define-key 'motion elfeed-search-mode-map
    (kbd "<return>") 'elfeed-search-show-entry
    "o" 'elfeed-search-browse-url
    "y" 'elfeed-search-yank

    ;; filter
    "s" 'elfeed-search-live-filter
    "S" 'elfeed-search-set-filter

    ;; update
    "R" 'elfeed-search-fetch ; TODO: Which update function is more useful?
    "r" 'elfeed-search-update--force

    ;; quit
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'quit-window)

  (evil-define-key '(motion visual) elfeed-search-mode-map
    "+" 'elfeed-search-tag-all
    "-" 'elfeed-search-untag-all
    "U" 'elfeed-search-tag-all-unread
    "u" 'elfeed-search-untag-all-unread)

  (evil-set-initial-state 'elfeed-show-mode 'motion)
  (evil-define-key 'motion elfeed-show-mode-map
    "o" 'elfeed-show-visit

    ;; filter
    "s" 'elfeed-show-new-live-search

    "y" 'elfeed-show-yank

    "+" 'elfeed-show-tag
    "-" 'elfeed-show-untag

    "A" 'elfeed-show-add-enclosure-to-playlist
    "P" 'elfeed-show-play-enclosure
    "d" 'elfeed-show-save-enclosure

    "]" 'elfeed-show-next
    "[" 'elfeed-show-prev
    (kbd "C-j") 'elfeed-show-next
    (kbd "C-k") 'elfeed-show-prev

    ;; update
    "r" 'elfeed-show-refresh

    ;; quit
    "q" 'elfeed-kill-buffer
    "ZQ" 'elfeed-kill-buffer
    "ZZ" 'elfeed-kill-buffer))

(provide 'evil-elfeed)
;;; evil-elfeed.el ends here
