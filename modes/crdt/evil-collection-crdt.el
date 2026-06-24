;;; evil-collection-crdt.el --- Bindings for `crdt' -*- lexical-binding: t -*-

;; Copyright (C) 2022 Arte Ebrahimi and Jonathan Ming

;; Authors: Arte Ebrahimi <arteebrahimi@gmail.com> and Jonathan Ming <jming422@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, convenience, tools

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
;;; Bindings for crdt.

;;; Code:
(require 'evil-collection)
(require 'crdt nil t)

(defvar crdt-mode-map)
(defconst evil-collection-crdt-maps
  '(crdt-buffer-menu-mode-map crdt-session-menu-mode-map crdt-user-menu-mode-map crdt-read-settings-map))

(defun evil-collection-crdt-setup ()
  "Set up `evil' bindings for crdt."
  (evil-collection-bind 'crdt-buffer-menu-mode-map
                        'action 'crdt-switch-to-buffer-other-window
                        'delete 'crdt-stop-share-buffer
                        'delete-2 'crdt-stop-share-buffer)

  (evil-collection-bind 'crdt-session-menu-mode-map
                        'action 'crdt-list-buffers
                        'delete 'crdt--stop-session
                        'delete-2 'crdt--stop-session)

  (evil-collection-bind 'crdt-user-menu-mode-map
                        'action 'crdt-goto-user
                        'delete 'crdt-kill-user
                        'delete-2 'crdt-kill-user)

  (evil-collection-bind 'crdt-read-settings-map
                        'action 'exit-recursive-edit
                        'quit 'exit-recursive-edit
                        'quit-save 'exit-recursive-edit
                        'quit-cancel 'abort-recursive-edit)

  (evil-collection-define-key 'normal 'crdt-user-menu-mode-map
    "f" 'crdt-follow-user))

(provide 'evil-collection-crdt)
;;; evil-collection-crdt.el ends here
