;;; evil-collection-message.el --- Bindings for `message' -*- lexical-binding: t -*-

;; Copyright (C) 2026 jessy836

;; Author: jessy836 <netocenusa@gmail.com>
;; Maintainer: jessy836 <netocenusa@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
;;; Bindings for message.

;;; Code:
(require 'evil-collection)
(require 'message nil t)

(defvar message-mode-map)
(defconst evil-collection-message-maps '(message-mode-map))

;;;###autoload
(defun evil-collection-message-setup ()
  "Set up `evil' bindings for `message-mode'."
  (evil-set-initial-state 'message-mode 'insert)
  (evil-collection-define-key 'normal 'message-mode-map
    "ZZ" 'message-send-and-exit
    "ZQ" 'message-kill-buffer
    "ZD" 'message-dont-send
    "ZF" 'mml-attach-file))

(provide 'evil-collection-message)
;;; evil-collection-message.el ends here
