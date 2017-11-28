;;; evil-geiser.el --- Bindings for `geiser'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, geiser, tools

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
;;; Bindings for `geiser'.
(require 'evil)
(require 'geiser nil t)

(defvar geiser-debug-mode-map)
(defvar geiser-doc-mode-map)

;;; Code:
(defun evil-geiser-setup ()
  "Set up bindings for `geiser'."
  (evil-set-initial-state 'geiser-debug-mode 'normal)
  (evil-set-initial-state 'geiser-doc-mode 'normal)

  (evil-define-key 'normal geiser-debug-mode-map
    "q" 'quit-window)

  (evil-define-key 'normal geiser-doc-mode-map
    "gd" 'geiser-edit-symbol-at-point
    (kbd "C-t") 'geiser-pop-symbol-stack
    "gr" 'geiser-doc-refresh
    "q" 'View-quit
    "gz" 'geiser-doc-switch-to-repl
    ">" 'geiser-doc-next
    "<" 'geiser-doc-previous
    "gj" 'forward-button
    "gk" 'backward-button
    (kbd "C-j") 'forward-button
    (kbd "C-k") 'backward-button
    "]" 'geiser-doc-next-section
    "[" 'geiser-doc-previous-section
    "x" 'geiser-doc-kill-page
    "X" 'geiser-doc-clean-history)

  (evil-define-key 'insert geiser-repl-mode-map
    (kbd "S-<return>") 'geiser-repl--newline-and-indent)

  (evil-define-key 'normal geiser-repl-mode-map
    "gd" 'geiser-edit-symbol-at-point
    (kbd "C-t") 'geiser-pop-symbol-stack
    (kbd "K") 'geiser-doc-symbol-at-point)

  (evil-define-key 'normal geiser-mode-map
    "gd" 'geiser-edit-symbol-at-point
    (kbd "C-t") 'geiser-pop-symbol-stack
    (kbd "gZ") 'geiser-mode-switch-to-repl-and-enter
    (kbd "gz") 'geiser-mode-switch-to-repl
    (kbd "K") 'geiser-doc-symbol-at-point))

(provide 'evil-geiser)
;;; evil-geiser.el ends here
