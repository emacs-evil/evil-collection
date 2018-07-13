;;; evil-collection-pass.el --- Evil bindings for pass-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, pass, tools

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
;; Evil bindings for `pass-mode'.

;;; Code:
(require 'evil-collection)
(require 'pass nil t)

(defvar pass-mode-map)

(defconst evil-collection-pass-maps '(pass-mode-map))

(defun evil-collection-pass-setup ()
  "Set up `evil' bindings for `pass-mode'."
  (evil-collection-define-key 'normal 'pass-mode-map
    "gj" 'pass-next-entry
    "gk" 'pass-prev-entry
    (kbd "C-j") 'pass-next-entry
    (kbd "C-k") 'pass-prev-entry
    (kbd "]") 'pass-next-directory
    (kbd "[") 'pass-prev-directory
    "x" 'pass-kill
    "s" 'isearch-forward
    "g?" 'describe-mode
    "gr" 'pass-update-buffer
    "i" 'pass-insert
    "I" 'pass-insert-generated
    "Y" 'pass-copy
    "r" 'pass-rename
    "o" 'pass-otp-options
    (kbd "<return>") 'pass-view
    "q" 'pass-quit))

(provide 'evil-collection-pass)
;;; evil-collection-pass.el ends here
