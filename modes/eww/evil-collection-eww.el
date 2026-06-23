;;; evil-collection-eww.el --- Evil bindings for EWW -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
  (evil-collection-bind 'eww-mode-map
                        'scroll-down 'scroll-up-command
                        'scroll-up 'scroll-down-command
                        'next-item 'eww-next-url
                        'prev-item 'eww-previous-url
                        'next-section 'eww-next-url
                        'prev-section 'eww-previous-url
                        'action-other 'eww-browse-with-external-browser
                        'browse-url 'eww-browse-with-external-browser
                        'quit 'quit-window
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window
                        'refresh 'eww-reload)

  (evil-collection-define-operator-key 'yank 'eww-mode-map
    "u" 'eww-copy-page-url)

  (evil-collection-bind 'eww-mode-map
                        'cycle-next 'shr-next-link
                        'cycle-previous 'shr-previous-link
                        'find-file 'eww-view-source)

  (evil-collection-set-readonly-bindings 'eww-history-mode-map)
  (evil-set-initial-state 'eww-history-mode 'normal)
  (evil-collection-bind 'eww-history-mode-map
                        'action 'eww-history-browse
                        'refresh 'revert-buffer)

  (evil-collection-set-readonly-bindings 'eww-buffers-mode-map)
  (evil-set-initial-state 'eww-buffers-mode 'normal)
  (evil-collection-bind 'eww-buffers-mode-map
                        'action 'eww-buffer-select
                        'next-item 'eww-buffer-show-next
                        'prev-item 'eww-buffer-show-previous
                        'next-section 'eww-buffer-show-next
                        'prev-section 'eww-buffer-show-previous
                        'refresh 'revert-buffer
                        'delete 'eww-buffer-kill)

  (evil-collection-set-readonly-bindings 'eww-bookmark-mode-map)
  (evil-set-initial-state 'eww-bookmark-mode 'normal)
  (evil-collection-define-key 'normal 'eww-bookmark-mode-map
    "P" 'eww-bookmark-yank)
  (evil-collection-bind 'eww-bookmark-mode-map
                        'action 'eww-bookmark-browse
                        'refresh 'revert-buffer
                        'delete 'eww-bookmark-kill)

  (evil-collection-define-operator-key 'yank 'eww-bookmark-mode-map
    "u" 'eww-copy-page-url))

(provide 'evil-collection-eww)
;;; evil-collection-eww.el ends here
