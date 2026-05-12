;;; evil-collection-eca.el --- Bindings for eca -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, eca, tools

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
;;; Bindings for eca.

;;; Code:
(require 'evil-collection)
(require 'eca nil t)

(defconst evil-collection-eca-maps '(eca-chat-mode-map))

(defvar eca-chat-mode-map)

;;;###autoload
(defun evil-collection-eca-setup ()
  "Set up `evil' bindings for `eca'."
  (add-hook 'eca-chat-mode-hook 'evil-normalize-keymaps)

  (let* ((submit evil-collection-repl-submit-state)
         (newline-state (if (eq submit 'normal) 'insert 'normal)))
    (evil-collection-define-key submit 'eca-chat-mode-map
      (kbd "<return>") 'eca-chat--key-pressed-return
      (kbd "RET") 'eca-chat--key-pressed-return)
    (evil-collection-define-key newline-state 'eca-chat-mode-map
      (kbd "<return>") 'eca-chat--key-pressed-newline
      (kbd "RET") 'eca-chat--key-pressed-newline
      (kbd "C-<return>") 'eca-chat--key-pressed-return))

  (evil-collection-define-key 'insert 'eca-chat-mode-map
    (kbd "S-<return>") 'eca-chat--key-pressed-newline)
  (evil-collection-define-key 'normal 'eca-chat-mode-map
    (kbd "S-<return>") 'eca-chat--key-pressed-newline))

(provide 'evil-collection-eca)
;;; evil-collection-eca.el ends here
