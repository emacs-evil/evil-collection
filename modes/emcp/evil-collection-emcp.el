;;; evil-collection-emcp.el --- Bindings for emcp -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emcp, tools

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
;;; Bindings for `emcp'.

;;; Code:
(require 'evil-collection)

(defconst evil-collection-emcp-maps '(emcp-session-manager-mode-map
                                      emcp-confirm-mode-map))

(defun evil-collection-emcp-confirm-dispatch ()
  "Dispatch the pressed key through the current `emcp-confirm' local map."
  (interactive)
  (let* ((key (this-command-keys-vector))
         (binding (lookup-key (current-local-map) key)))
    (cond
     ((commandp binding) (call-interactively binding))
     ((null binding) (user-error "No EMCP confirmation action for `%s'"
                                 (key-description key)))
     (t (user-error "Unexpected EMCP confirmation binding for `%s': %S"
                    (key-description key) binding)))))

(defun evil-collection-emcp-setup-session-manager ()
  "Set up `evil' bindings for `emcp-session-manager'."
  (evil-set-initial-state 'emcp-session-manager-mode 'normal)

  (evil-collection-bind 'emcp-session-manager-mode-map
                        'refresh 'emcp-session-manager-refresh
                        'delete 'emcp-session-manager-kill
                        'delete-2 'emcp-session-manager-kill-server
                        'describe-mode 'emcp-session-manager-show-log
                        'quit 'quit-window
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window)

  (evil-collection-define-key 'normal 'emcp-session-manager-mode-map
    "D" 'emcp-session-manager-kill-session
    "E" 'emcp-session-manager-cycle-eval-mode
    "S" 'emcp-session-manager-cycle-send-keys-mode))

(defun evil-collection-emcp-setup-confirm ()
  "Set up `evil' bindings for `emcp-confirm'."
  (evil-set-initial-state 'emcp-confirm-mode 'normal)

  (evil-collection-bind 'emcp-confirm-mode-map
                        'quit 'kill-current-buffer
                        'quit-save 'kill-current-buffer
                        'quit-cancel 'kill-current-buffer)

  (evil-collection-define-key 'normal 'emcp-confirm-mode-map
    "a" 'evil-collection-emcp-confirm-dispatch
    "n" 'evil-collection-emcp-confirm-dispatch
    "r" 'evil-collection-emcp-confirm-dispatch
    "w" 'evil-collection-emcp-confirm-dispatch
    "y" 'evil-collection-emcp-confirm-dispatch))

;;;###autoload
(defun evil-collection-emcp-setup ()
  "Set up `evil' bindings for `emcp'."
  (with-eval-after-load 'emcp-session-manager
    (evil-collection-emcp-setup-session-manager))
  (with-eval-after-load 'emcp-confirm
    (evil-collection-emcp-setup-confirm)))

(provide 'evil-collection-emcp)
;;; evil-collection-emcp.el ends here
