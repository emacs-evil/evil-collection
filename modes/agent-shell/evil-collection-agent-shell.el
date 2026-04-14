;;; evil-collection-agent-shell.el --- Bindings for agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Joseph LaFreniere

;; Author: Joseph LaFreniere <git@lafreniere.xyz>
;; Maintainer: Joseph LaFreniere <git@lafreniere.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, agent-shell, tools

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
;;; Bindings for agent-shell.

;;; Code:
(require 'evil-collection)
(require 'agent-shell nil t)

(defconst evil-collection-agent-shell-maps '(agent-shell-mode-map))

;;;###autoload
(defun evil-collection-agent-shell-setup ()
  "Set up `evil' bindings for `agent-shell'."
  ;; `agent-shell-mode-map' binds \"n\" and \"p\" at the map level, which causes
  ;; them to intercept keystrokes even in insert state.  Remove those bindings
  ;; entirely.
  (define-key agent-shell-mode-map "n" nil)
  (define-key agent-shell-mode-map "p" nil))

(provide 'evil-collection-agent-shell)
;;; evil-collection-agent-shell.el ends here
