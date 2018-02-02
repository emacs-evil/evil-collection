;;; evil-collection-epa.el --- Evil bindings for epa-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: Maximiliano Sandoval <msandova@protonmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, epa, tools

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
;; Evil bindings for `epa-mode'.

;;; Code:

(require 'evil)
(require 'epa nil t)

(defun evil-collection-epa-setup ()
  (evil-define-key 'normal epa-key-list-mode-map
    (kbd "<tab>") 'widget-forward
    "m" 'epa-mark-key
    "u" 'epa-unmark-key
    "gr" 'revert-buffer
    "q" 'epa-exit-buffer
    "E" 'epa-decrypt-file
    "d" 'epa-delete-keys
    "ZZ" 'quit-window
    "ZQ" 'evil-quit
    "V" 'epa-verify-file
    
    ;; Unchanged keybindings.
    "s" 'epa-sign-file
    "e" 'epa-encrypt-file
    "i" 'epa-import-keys
    "o" 'epa-export-keys)

  (evil-define-key 'normal epa-key-mode-map
    "q" 'epa-exit-buffer
    "ZZ" 'quit-window
    "ZQ" 'evil-quit)
  
  (evil-define-key 'normal epa-info-mode-map
    "q" 'delete-window
    "ZZ" 'quit-window
    "ZQ" 'evil-quit))

(provide 'evil-collection-epa)
;;; evil-collection-epa.el ends here
