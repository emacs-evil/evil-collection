;;; evil-collection-notmuch.el --- Bindings for `notmuch'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, tools, evil

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
;;; Bindings for `notmuch'.

;;; Code:
(require 'notmuch nil t)
(require 'evil-collection)

(declare-function notmuch-show-get-tags "notmuch-show")
(declare-function notmuch-show-tag "notmuch-show")
(declare-function notmuch-search-get-tags "notmuch")
(declare-function notmuch-search-tag "notmuch")
(declare-function notmuch-tree-tag "notmuch-tree")

(declare-function notmuch-tree-close-message-pane-and "notmuch-tree")

(defconst evil-collection-notmuch-maps '(notmuch-common-keymap
                                         notmuch-hello-mode-map
                                         notmuch-show-mode-map
                                         notmuch-show-part-map
                                         notmuch-tree-mode-map
                                         notmuch-search-mode-map
                                         notmuch-search-stash-map))

(defun evil-collection-notmuch-toggle-tag (tag mode &optional next-function)
  "Toggle TAG tag for message in MODE."
  (let ((get (intern (format "notmuch-%s-get-tags" mode)))
        (set (intern (format "notmuch-%s-tag" mode)))
        (next (or next-function (intern (format "notmuch-%s-next-message" mode)))))
    (funcall set (list (concat (if (member tag (funcall get))
                                   "-" "+")
                               tag)))
    (funcall next)))

(defun evil-collection-notmuch-show-toggle-delete ()
  "Toggle deleted tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "deleted" "show"))

(defun evil-collection-notmuch-tree-toggle-delete ()
  "Toggle deleted tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "deleted" "tree"))

(defun evil-collection-notmuch-search-toggle-delete ()
  "Toggle deleted tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "deleted" "search" 'notmuch-search-next-thread))

(defun evil-collection-notmuch-tree-toggle-unread ()
  "Toggle unread tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "unread" "tree"))

(defun evil-collection-notmuch-search-toggle-unread ()
  "Toggle unread tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "unread" "search" 'notmuch-search-next-thread))

(defun evil-collection-notmuch-show-toggle-flagged ()
  "Toggle flagged tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "flagged" "show"))

(defun evil-collection-notmuch-tree-toggle-flagged ()
  "Toggle flagged tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "flagged" "tree"))

(defun evil-collection-notmuch-search-toggle-flagged ()
  "Toggle flagged tag for message."
  (interactive)
  (evil-collection-notmuch-toggle-tag "flagged" "tree" 'notmuch-search-next-thread))

(defun evil-collection-notmuch-hello-ret ()
  (interactive)
  (evil-execute-in-emacs-state)
  (call-interactively (key-binding (kbd "RET"))))

;;;###autoload
(defun evil-collection-notmuch-setup ()
  "Set up `evil' bindings for `notmuch'."
  (evil-collection-inhibit-insert-state 'notmuch-show-mode-map)
  (evil-collection-inhibit-insert-state 'notmuch-search-mode-map)
  (evil-collection-inhibit-insert-state 'notmuch-tree-mode-map)

  (evil-set-initial-state 'notmuch-show-mode 'normal)
  (evil-set-initial-state 'notmuch-search-mode 'normal)
  (evil-set-initial-state 'notmuch-hello-mode 'normal)
  (evil-set-initial-state 'notmuch-tree-mode 'normal)

  (evil-collection-define-key 'normal 'notmuch-common-keymap
    "g?" 'notmuch-help
    "q" 'notmuch-bury-or-kill-this-buffer
    "s" 'notmuch-search
    "z" 'notmuch-tree
    "C" 'notmuch-mua-new-mail           ; like mu4e
    "cc" 'notmuch-mua-new-mail          ; like mu4e
    "gr" 'notmuch-refresh-this-buffer
    "gR" 'notmuch-refresh-all-buffers
    "Z" 'notmuch-poll-and-refresh-this-buffer
    "J" 'notmuch-jump-search)

  (evil-collection-define-key 'normal 'notmuch-hello-mode-map
    "g?" 'notmuch-hello-versions
    (kbd "TAB") 'widget-forward
    (kbd "RET") 'evil-collection-notmuch-hello-ret
    (kbd "S-TAB") 'widget-backward
    (kbd "<C-tab>") 'widget-backward)

  (evil-collection-define-key 'normal 'notmuch-show-mode-map
    "gd" 'goto-address-at-point
    "p" 'notmuch-show-save-attachments
    "A" 'notmuch-show-archive-thread-then-next
    "S" 'notmuch-show-filter-thread
    "K" 'notmuch-tag-jump
    "C" 'notmuch-mua-new-mail           ; like mu4e
    "cc" 'notmuch-mua-new-mail          ; like mu4e
    "cR" 'notmuch-show-reply
    "X" 'notmuch-show-archive-thread-then-exit
    "Z" 'notmuch-tree-from-show-current-query
    "a" 'notmuch-show-archive-message-then-next-or-next-thread
    "d" 'evil-collection-notmuch-show-toggle-delete
    "=" 'evil-collection-notmuch-show-toggle-flagged
    "H" 'notmuch-show-toggle-visibility-headers
    "gj" 'notmuch-show-next-open-message
    "gk" 'notmuch-show-previous-open-message
    "]]" 'notmuch-show-next-message
    "[[" 'notmuch-show-previous-message
    (kbd "C-j") 'notmuch-show-next-message
    (kbd "C-k") 'notmuch-show-previous-message
    (kbd "M-j") 'notmuch-show-next-thread-show
    (kbd "M-k") 'notmuch-show-previous-thread-show
    "cr" 'notmuch-show-reply-sender
    (kbd "x") 'notmuch-show-archive-message-then-next-or-exit
    "|" 'notmuch-show-pipe-message
    "*" 'notmuch-show-tag-all
    "-" 'notmuch-show-remove-tag
    "+" 'notmuch-show-add-tag
    (kbd "TAB") 'notmuch-show-toggle-message
    (kbd "RET") 'notmuch-show-toggle-message
    "." 'notmuch-show-part-map)

  (evil-collection-define-key 'normal 'notmuch-tree-mode-map
    "g?" (notmuch-tree-close-message-pane-and 'notmuch-help)
    "q" 'notmuch-tree-quit
    "s" 'notmuch-tree-to-search
    "C" (notmuch-tree-close-message-pane-and 'notmuch-mua-new-mail) ; like mu4e
    "cc" (notmuch-tree-close-message-pane-and 'notmuch-mua-new-mail) ; like mu4e
    "J" (notmuch-tree-close-message-pane-and 'notmuch-jump-search)
    "S" 'notmuch-search-from-tree-current-query
    "cr" (notmuch-tree-close-message-pane-and 'notmuch-show-reply-sender) ; like mu4e
    "cR" (notmuch-tree-close-message-pane-and 'notmuch-show-reply)
    "d" 'evil-collection-notmuch-tree-toggle-delete
    "!" 'evil-collection-notmuch-tree-toggle-unread
    "=" 'evil-collection-notmuch-tree-toggle-flagged
    "K" 'notmuch-tag-jump
    (kbd "RET") 'notmuch-tree-show-message
    [mouse-1] 'notmuch-tree-show-message
    "A" 'notmuch-tree-archive-thread
    "a" 'notmuch-tree-archive-message-then-next
    "z" 'notmuch-tree-to-tree
    "gj" 'notmuch-tree-next-matching-message
    "gk" 'notmuch-tree-prev-matching-message
    "]]" 'notmuch-tree-next-message
    "[[" 'notmuch-tree-prev-message
    (kbd "C-k") 'notmuch-tree-prev-thread
    (kbd "C-j") 'notmuch-tree-next-thread
    "-" 'notmuch-tree-remove-tag
    "+" 'notmuch-tree-add-tag
    "*" 'notmuch-tree-tag-thread
    "e" 'notmuch-tree-resume-message)

  (dolist (state '(normal visual))
    (evil-collection-define-key state 'notmuch-search-mode-map
      "cC" 'compose-mail-other-frame
      "J" 'notmuch-jump-search
      "S" 'notmuch-search-filter
      "K" 'notmuch-tag-jump
      "o" 'notmuch-search-toggle-order
      "Z" 'notmuch-tree-from-search-current-query
      "*" 'notmuch-search-tag-all
      "a" 'notmuch-search-archive-thread
      "cc" 'compose-mail                ; like mu4e
      "d" 'evil-collection-notmuch-search-toggle-delete
      "!" 'evil-collection-notmuch-search-toggle-unread
      "=" 'evil-collection-notmuch-search-toggle-flagged
      "q" 'notmuch-bury-or-kill-this-buffer
      "cr" 'notmuch-search-reply-to-thread-sender
      "cR" 'notmuch-search-reply-to-thread
      "t" 'notmuch-search-filter-by-tag
      "z" 'notmuch-search-stash-map
      [mouse-1] 'notmuch-search-show-thread
      "-" 'notmuch-search-remove-tag
      "+" 'notmuch-search-add-tag
      (kbd "RET") 'notmuch-search-show-thread))

  (evil-collection-define-key 'normal 'notmuch-search-stash-map
    "i" 'notmuch-search-stash-thread-id
    "q" 'notmuch-stash-query
    "g?" 'notmuch-subkeymap-help))

(provide 'evil-collection-notmuch)
;;; evil-collection-notmuch.el ends here
