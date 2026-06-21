;;; evil-collection-eww.el --- Evil bindings for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, eww, tools

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
;; Evil bindings for EWW.

;;; Code:
(require 'eww)
(require 'evil-collection)

(defvar evil-collection-eww-maps '(eww-mode-map
                                   eww-history-mode-map
                                   eww-buffers-mode-map
                                   eww-bookmark-mode-map))

;;;###autoload
(defun evil-collection-eww-setup ()
  "Set up `evil' bindings for `eww'."

  (evil-set-initial-state 'eww-mode 'normal)
  (evil-collection-define-key 'normal 'eww-mode-map
    "^" 'eww-up-url
    "u" 'eww-up-url
    "U" 'eww-top-url
    (kbd "DEL") 'eww-back-url
    "H" 'eww-back-url
    "L" 'eww-forward-url

    "&" 'eww-browse-with-external-browser
    "gc" 'url-cookie-list
    "zd" 'eww-toggle-paragraph-direction
    "ze" 'eww-set-character-encoding
    "zf" 'eww-toggle-fonts
    "d" 'eww-download
    "m" 'eww-add-bookmark
    "R" 'eww-readable                   ; Default binding.
    "r" 'eww-readable

    "o" 'eww                            ; Like qutebrowser.

    ;; bookmarks
    "gb" 'eww-list-bookmarks

    "gh" 'eww-list-histories
    "gt" 'eww-list-buffers)             ; Like dwb, qutebrowser.
  (evil-collection-bind 'eww-mode-map  'scroll-down 'scroll-up-command)
  (evil-collection-bind 'eww-mode-map    'scroll-up 'scroll-down-command)
  (evil-collection-bind 'eww-mode-map    'next-item 'eww-next-url)
  (evil-collection-bind 'eww-mode-map    'prev-item 'eww-previous-url)
  (evil-collection-bind 'eww-mode-map 'next-section 'eww-next-url)
  (evil-collection-bind 'eww-mode-map 'prev-section 'eww-previous-url)
  (evil-collection-bind 'eww-mode-map 'action-other 'eww-browse-with-external-browser)
  (evil-collection-bind 'eww-mode-map        'quit 'quit-window)
  (evil-collection-bind 'eww-mode-map   'quit-save 'quit-window)
  (evil-collection-bind 'eww-mode-map 'quit-cancel 'quit-window)
  (evil-collection-bind 'eww-mode-map     'refresh 'eww-reload)

  (evil-collection-define-operator-key 'yank 'eww-mode-map
    "u" 'eww-copy-page-url)

  (evil-collection-bind 'eww-mode-map     'cycle-next 'shr-next-link)
  (evil-collection-bind 'eww-mode-map 'cycle-previous 'shr-previous-link)

  (evil-collection-bind 'eww-mode-map 'find-file 'eww-view-source)

  (evil-collection-set-readonly-bindings 'eww-history-mode-map)
  (evil-set-initial-state 'eww-history-mode 'normal)
  (evil-collection-bind 'eww-history-mode-map 'action 'eww-history-browse)
  (evil-collection-bind 'eww-history-mode-map 'refresh 'revert-buffer)

  (evil-collection-set-readonly-bindings 'eww-buffers-mode-map)
  (evil-set-initial-state 'eww-buffers-mode 'normal)
  (evil-collection-bind 'eww-buffers-mode-map       'action 'eww-buffer-select)
  (evil-collection-bind 'eww-buffers-mode-map    'next-item 'eww-buffer-show-next)
  (evil-collection-bind 'eww-buffers-mode-map    'prev-item 'eww-buffer-show-previous)
  (evil-collection-bind 'eww-buffers-mode-map 'next-section 'eww-buffer-show-next)
  (evil-collection-bind 'eww-buffers-mode-map 'prev-section 'eww-buffer-show-previous)
  (evil-collection-bind 'eww-buffers-mode-map 'refresh 'revert-buffer)
  (evil-collection-bind 'eww-buffers-mode-map  'delete 'eww-buffer-kill)

  (evil-collection-set-readonly-bindings 'eww-bookmark-mode-map)
  (evil-set-initial-state 'eww-bookmark-mode 'normal)
  (evil-collection-define-key 'normal 'eww-bookmark-mode-map
    "P" 'eww-bookmark-yank)
  (evil-collection-bind 'eww-bookmark-mode-map 'action 'eww-bookmark-browse)
  (evil-collection-bind 'eww-bookmark-mode-map 'refresh 'revert-buffer)
  (evil-collection-bind 'eww-bookmark-mode-map  'delete 'eww-bookmark-kill)

  (evil-collection-define-operator-key 'yank 'eww-bookmark-mode-map
    "u" 'eww-copy-page-url))

(provide 'evil-collection-eww)
;;; evil-collection-eww.el ends here
