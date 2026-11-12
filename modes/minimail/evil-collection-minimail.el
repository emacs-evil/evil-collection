;;; evil-collection-minimail.el --- Bindings for `minimail' -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ruoyu Feng

;; Author: Ruoyu Feng <dev@vonfry.name>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.12
;; Package-Requires: ((emacs "30.1"))
;; Keywords: evil, emacs, email

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
;;; Bindings for `minimail'.

;;; Code:
(require 'evil-collection)
(require 'minimail nil t)

(defvar minimail-overview-mode-map)
(defvar minimail-mailbox-mode-map)
(defvar minimail-message-mode-map)

(defconst evil-collection-minimail-maps '(minimail-overview-mode-map
                                          minimail-mailbox-mode-map
                                          minimail-message-mode-map))

;;;###autoload
(defun evil-collection-minimail-setup ()
  "Set up `evil' bindings for `minimail'."
  (evil-set-initial-state 'minimail-overview-mode 'normal)
  (evil-set-initial-state 'minimail-mailbox-mode 'normal)
  (evil-set-initial-state 'minimail-message-mode 'normal)

  (dolist (map evil-collection-minimail-maps)
    (evil-collection-inhibit-insert-state map)
    (evil-collection-define-key 'normal map
      "cc" 'compose-mail)
    (evil-collection-bind map
                          'search-or-filter 'minimail-search
                          'quit 'quit-window
                          'quit-save 'quit-window
                          'quit-cancel 'quit-window
                          'describe-mode 'describe-mode))

  (evil-collection-define-key 'normal 'minimail-overview-mode-map
    ;; Bind RET instead of ''action' due to tranisent.
    (kbd "RET") 'widget-button-press
    "%" 'minimail-execute-server-command)
  (evil-collection-bind 'minimail-overview-mode-map
                        'cycle-next 'widget-forward
                        'cycle-previous 'widget-backward
                        'refresh 'revert-buffer)

  (dolist (map '(minimail-mailbox-mode-map minimail-message-mode-map))
    (evil-collection-define-key 'normal map
      "zj" 'minimail-message-scroll-up
      "zk" 'minimail-message-scroll-down
      "cr" 'minimail-reply
      "cR" 'minimail-reply-all
      "cf" 'minimail-forward
      "y" 'minimail-copy-to-mailbox
      "r" 'minimail-move-to-mailbox
      "A" 'minimail-move-to-archive
      "D" 'minimail-move-to-trash
      "x" 'minimail-move-to-junk
      "+" 'minimail-toggle-message-flags
      "~" 'minimail-toggle-message-flagged
      "u" 'minimail-toggle-message-seen)
    (evil-collection-bind map
                          'next-item 'minimail-next-message-unseen
                          'prev-item 'minimail-previous-message-unseen
                          'next-section-2 'minimail-next-message
                          'prev-section-2 'minimail-previous-message))

  (evil-collection-define-key 'normal 'minimail-mailbox-mode-map
    (kbd "RET") 'minimail-show-message
    "z+" 'minimail-load-more-messages
    "zc" 'minimail-quit-message-window
    "zt" 'minimail-sort-by-thread)
  (evil-collection-bind 'minimail-mailbox-mode-map
                        'refresh 'revert-buffer)

  (evil-collection-define-key 'normal 'minimail-message-mode-map
    (kbd "RET") 'push-button)
  (evil-collection-bind 'minimail-message-mode-map
                        'next-button 'forward-button
                        'previous-button 'backward-button))

(provide 'evil-collection-minimail)
;;; evil-collection-minimail.el ends here
