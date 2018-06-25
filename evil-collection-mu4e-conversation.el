;;; evil-collection-mu4e-conversation.el --- Evil bindings for mu4e-conversation -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (evil "1.2.10"))
;; Keywords: evil, mu4e, tools

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
;; Evil bindings for mu4e-conversation.

;;; Code:

(require 'evil-collection)
(require 'mu4e-conversation nil t)

(defconst evil-collection-mu4e-conversation-maps '(mu4e-conversation-mode-map
                                                   mu4e-conversation-linear-map
                                                   mu4e-conversation-tree-map))

(defun evil-collection-mu4e-conversation-setup ()
  "Set up `evil' bindings for `mu4e-conversation'."
  (evil-define-key 'normal mu4e-conversation-linear-map
    " " 'evil-scroll-page-down
    (kbd "S-SPC") 'evil-scroll-page-up
    "zv" 'mu4e-conversation-toggle-view)
  (evil-define-key 'visual mu4e-conversation-linear-map
    (kbd "<return>") 'mu4e-conversation-cite)
  (evil-define-key '(normal visual) mu4e-conversation-linear-map
    (kbd "C-x C-s") 'mu4e-conversation-save
    (kbd "C-c C-c") 'mu4e-conversation-send
    (kbd "M-q") 'mu4e-conversation-fill-long-lines
    "p" 'mu4e-view-save-attachment-multi
    "o" 'mu4e-view-open-attachment
    "#" 'mu4e-conversation-toggle-hide-cited
    "[" 'mu4e-conversation-previous-message ; TODO: Don't override previous-unread?  There is still "gk".
    "]" 'mu4e-conversation-next-message
    "q" 'mu4e-conversation-quit)
  (evil-define-key 'normal mu4e-conversation-tree-map
    " " 'evil-scroll-page-down
    (kbd "S-SPC") 'evil-scroll-page-up
    "a" 'mu4e-view-action
    "zv" 'mu4e-conversation-toggle-view)
  (evil-define-key 'visual mu4e-conversation-tree-map
    (kbd "<return>") 'mu4e-conversation-cite)
  (evil-define-key '(normal visual) mu4e-conversation-tree-map
    (kbd "C-x C-s") 'mu4e-conversation-save
    (kbd "C-c C-c") 'mu4e-conversation-send
    (kbd "M-q") 'mu4e-conversation-fill-long-lines
    "C" 'mu4e-compose-new
    "R" 'mu4e-compose-reply
    "p" 'mu4e-view-save-attachment-multi
    "o" 'mu4e-view-open-attachment
    "cc" 'mu4e-compose-new
    "cr" 'mu4e-compose-reply
    "ce" 'mu4e-compose-edit
    "cf" 'mu4e-compose-forward
    "q" 'mu4e-conversation-quit))

(provide 'evil-collection-mu4e-conversation)
;;; evil-collection-mu4e-conversation.el ends here
