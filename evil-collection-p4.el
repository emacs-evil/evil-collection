;;; evil-collection-p4.el --- Evil bindings for P4 -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, p4, tools

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
;; Evil bindings for P4.

;;; Code:
(require 'p4 nil t)
(require 'evil-collection)

(defvar p4-basic-mode-map)

(defconst evil-collection-p4-maps '(p4-basic-mode-map))

(defun evil-collection-p4-setup ()
  "Set up `evil' bindings for `p4'."
  (evil-set-initial-state 'p4-basic-mode 'normal)

  (evil-collection-define-key 'normal 'p4-basic-mode-map
    [mouse-1] 'p4-buffer-mouse-clicked
    "k" 'p4-scroll-down-1-line
    "j" 'p4-scroll-up-1-line
    (kbd "C-j") 'p4-forward-active-link
    (kbd "C-k") 'p4-backward-active-link
    (kbd "<return>") 'p4-buffer-commands
    "q" 'quit-window
    "gr" 'revert-buffer
    "]" 'p4-scroll-down-1-window
    "[" 'p4-scroll-up-1-window
    "gg" 'p4-top-of-buffer
    "G" 'p4-bottom-of-buffer
    "=" 'delete-other-windows))

(provide 'evil-collection-p4)
;;; evil-collection-p4.el ends here
