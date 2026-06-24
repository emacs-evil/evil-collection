;;; evil-collection-telega.el --- Evil bindings for telega -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ruoyu Feng

;; Author: Ruoyu Feng <mail@vonfry.name>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.6
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools, telegram, telega

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
;; Evil bindings for telega.

;;; Code:
(require 'telega nil t)
(require 'evil-collection)

(defvar telega-prefix-map)
(defvar telega-filter-map)
(defvar telega-sort-map)
(defvar telega-chatbuf-fastnav-map)
(defvar telega-describe-map)
(defvar telega-folder-map)
(defvar telega-voip-map)
(defvar telega-root-view-map)
(defvar telega-root-fastnav-map)

(defconst evil-collection-telega-maps '(telega-root-mode-map
                                        telega-chat-mode-map
                                        telega-image-mode-map
                                        telega-webpage-mode-map
                                        telega-user-button-map
                                        telega-msg-button-map
                                        telega-chat-button-map
                                        telega-sticker-button-map))

(defconst evil-collection-telega-modes '(telega-root-mode
                                         telega-chat-mode
                                         telega-image-mode
                                         telega-webpage-mode))

;;;###autoload
(defun evil-collection-telega-setup ()
  "Set up `evil' bindings for `telega'."
  (evil-collection-set-readonly-bindings 'telega-root-mode-map)

  (dolist (mode evil-collection-telega-modes)
    (evil-set-initial-state mode 'normal))

  (evil-collection-define-key 'normal 'telega-root-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line

    "ga" telega-prefix-map

    "S" telega-sort-map
    "s" telega-filter-map
    "_" 'telega-filter-undo
    "-" 'telega-filter-redo

    "c" 'telega-chat-join-by-link
    "C" 'telega-chat-create
    "D" 'telega-chats-filtered-kill-chatbuf
    "R" 'telega-chats-filtered-toggle-read

    "Q" 'telega-kill

    "gO" telega-folder-map
    "gC" telega-voip-map
    "gV" telega-root-view-map
    "J" telega-root-fastnav-map

    "gs" 'telega-view-search)
  (evil-collection-bind 'telega-root-mode-map
                        'quit 'bury-buffer
                        'describe-mode telega-describe-map
                        'next-button 'telega-button-forward
                        'previous-button 'telega-button-backward)

  (evil-collection-define-key 'normal 'telega-chat-mode-map
    "ga" telega-prefix-map
    "gA" telega-chatbuf-fastnav-map

    "zz" 'telega-chatbuf-recenter-1

    "^" 'telega-chatbuf-beginning-of-thing

    "Za" 'telega-chatbuf-attach
    "Zf" 'telega-chatbuf-attach-media
    "Zv" 'telega-chatbuf-attach-clipboard

    "_" 'telega-chatbuf-filter-cancel
    "S" 'telega-chatbuf-filter-search)
  (evil-collection-bind 'telega-chat-mode-map
                        'quit 'quit-window
                        'describe-mode 'telega-describe-chat
                        'repl-submit 'telega-chatbuf-newline-or-input-send
                        'repl-newline 'newline
                        'repl-force-newline 'newline
                        'search-or-filter 'telega-chatbuf-filter
                        'next-button 'telega-chatbuf-complete-or-next-link
                        'previous-button 'telega-chatbuf-prev-link)

  (evil-collection-define-key 'normal 'telega-image-mode-map
    "ga" telega-prefix-map

    "}" 'telega-image-next
    "{" 'telega-image-prev)
  (evil-collection-bind 'telega-image-mode-map 'quit 'telega-image-quit)

  (evil-collection-define-key 'normal 'telega-webpage-mode-map
    "ga" telega-prefix-map

    "yy" 'telega-webpage-copy-url
    "(" 'telega-webpage-history-prev
    ")" 'telega-webpage-history-next)
  (evil-collection-bind 'telega-webpage-mode-map
                        'browse-url 'telega-webpage-browse-url
                        'next-button 'telega-button-forward
                        'previous-button 'telega-button-backward)

  (evil-collection-set-readonly-bindings 'telega-user-button-map)
  ; We have to set keybinds for emacs instead of normal state because normal
  ; states takes no effects on buttons and remove the default bindings.
  ; ref: emacs-evil/evil#1477
  (evil-collection-define-key nil 'telega-user-button-map
    "B" nil
    "K" nil
    (kbd "DEL") nil
    "i" nil

    "m" 'telega-user-chat-with
    "D" 'telega-user-block)
  (evil-collection-bind 'telega-user-button-map
                        'describe-mode 'telega-describe-user)

  (evil-collection-set-readonly-bindings 'telega-msg-button-map)
  (evil-collection-define-key nil 'telega-msg-button-map
    "c" nil
    "d" nil
    "e" nil
    "f" nil
    "k" nil
    "l" nil
    "m" nil
    "n" nil
    "p" nil
    "^" nil
    (kbd "DEL") nil
    "*" nil
    "B" nil
    "L" nil
    "T" nil

    "D" 'telega-msg-delete-dwim
    "dd" 'telega-msg-delete-dwim
    "i" 'telega-msg-edit
    "a" 'telega-msg-mark-toggle
    "R" 'telega-msg-forward-dwim
    "r" 'telega-msg-reply

    "Zy" 'telega-msg-copy-text
    "Zl" 'telega-msg-copy-link
    "ds" 'telega-msg-ban-sender
    "ZL" 'telega-msg-redisplay
    "P" (if (fboundp 'telega-transient-msg-pin-toggle)
            'telega-transient-msg-pin-toggle
          'telega-msg-pin-toggle)
    "ZR" 'telega-msg-resend
    "S" 'telega-msg-save
    "u" 'telega-msg-unmark
    "U" 'telega-chatbuf-msg-marks-toggle
    "=" 'telega-msg-diff-edits
    "s" 'telega-msg-favorite-toggle)
  (evil-collection-bind 'telega-msg-button-map
                        'refresh 'telega-msg-open-thread-or-topic
                        'describe-mode 'telega-describe-message
                        'next-button 'telega-button-forward
                        'previous-button 'telega-button-backward)

  (evil-collection-set-readonly-bindings 'telega-chat-button-map)
  (evil-collection-define-key nil 'telega-chat-button-map
    "i" nil
    "h" nil
    "d" nil
    "^" nil
    (kbd "DEL") nil

    "a" 'telega-chat-add-member
    "o" 'telega-chat-set-custom-order
    "r" 'telega-chat-toggle-read
    "P" 'telega-chat-toggle-pin
    "C" 'telega-chat-call
    "D" 'telega-chat-delete)
  (evil-collection-bind 'telega-chat-button-map
                        'describe-mode 'telega-describe-chat)

  (evil-collection-set-readonly-bindings 'telega-sticker-button-map)
  (evil-collection-define-key nil 'telega-sticker-button-map
    "f" nil
    "*" nil
    "i" nil
    "h" nil)
  (evil-collection-bind 'telega-sticker-button-map
                        'describe-mode 'telega-sticker-help
                        'toggle 'telega-sticker-toggle-favorite))

(provide 'evil-collection-telega)
;;; evil-collection-telega.el ends here
