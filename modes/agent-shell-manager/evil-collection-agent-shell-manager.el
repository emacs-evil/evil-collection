;;; evil-collection-agent-shell-manager.el --- Bindings for agent-shell-manager -*- lexical-binding: t -*-

;; Copyright (C) 2026 Julio Borja Barra

;; Author: Julio Borja Barra
;; Maintainer: Julio Borja Barra
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, agent-shell-manager, tools

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
;; Evil bindings for `agent-shell-manager' -- the tabulated-list buffer
;; manager for `agent-shell'.

;;; Code:
(require 'evil-collection)
(require 'agent-shell-manager nil t)

(defconst evil-collection-agent-shell-manager-maps '(agent-shell-manager-mode-map))

(defvar agent-shell-manager-mode-map)

;;;###autoload
(defun evil-collection-agent-shell-manager-setup ()
  "Set up `evil' bindings for `agent-shell-manager'."
  (evil-set-initial-state 'agent-shell-manager-mode 'normal)

  (evil-collection-define-key 'normal 'agent-shell-manager-mode-map
    (kbd "C-c C-c") 'agent-shell-manager-interrupt)

  (evil-collection-bind 'agent-shell-manager-mode-map
                        'action 'agent-shell-manager-goto
                        'refresh 'agent-shell-manager-refresh
                        'quit 'quit-window
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window)

  (if evil-collection-want-g-bindings
      (evil-collection-define-key 'normal 'agent-shell-manager-mode-map
        "gk" 'agent-shell-manager-kill
        "gc" 'agent-shell-manager-new
        "gR" 'agent-shell-manager-restart
        "gd" 'agent-shell-manager-delete-killed
        "gm" 'agent-shell-manager-set-mode
        "gv" 'agent-shell-manager-set-model
        "gx" 'agent-shell-manager-interrupt
        "gt" 'agent-shell-manager-view-traffic
        "gl" 'agent-shell-manager-toggle-logging)
    (evil-collection-define-key 'normal 'agent-shell-manager-mode-map
      "K" 'agent-shell-manager-kill
      "c" 'agent-shell-manager-new
      "R" 'agent-shell-manager-restart
      "d" 'agent-shell-manager-delete-killed
      "m" 'agent-shell-manager-set-mode
      "M" 'agent-shell-manager-set-model
      "x" 'agent-shell-manager-interrupt
      "t" 'agent-shell-manager-view-traffic
      "l" 'agent-shell-manager-toggle-logging)))

(provide 'evil-collection-agent-shell-manager)
;;; evil-collection-agent-shell-manager.el ends here
