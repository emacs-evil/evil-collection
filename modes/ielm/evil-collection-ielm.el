;;; evil-collection-ielm.el --- Bindings for `ielm' -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ian Pan <ianpan870102@gmail.com>

;; Author: Ian Pan <ianpan870102@gmail.com>
;; Maintainer: Ian Pan <ianpan870102@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, ielm, tools

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
;;; Bindings for `ielm' (Inferior Emacs Lisp Mode).

;;; Code:
(require 'evil-collection)
(require 'ielm nil t)

(defvar inferior-emacs-lisp-mode-map)
(defconst evil-collection-ielm-maps '(inferior-emacs-lisp-mode-map))

(defcustom evil-collection-ielm-move-cursor-back nil
  "Whether the cursor is moved backwards when exiting insert state."
  :type 'boolean
  :group 'ielm)

(defun evil-collection-ielm-escape-stay ()
  "Go back to normal state but don't move cursor backwards.
Moving cursor backwards is the default vim behavior but
it is not intuitive for some cases like REPL buffers."
  (setq-local evil-move-cursor-back
              evil-collection-ielm-move-cursor-back))

;;;###autoload
(defun evil-collection-ielm-setup ()
  "Set up `evil' bindings for `ielm'."
  (evil-set-initial-state 'inferior-emacs-lisp-mode 'insert)

  (add-hook 'ielm-mode-hook #'evil-collection-ielm-escape-stay)

  (evil-collection-bind 'inferior-emacs-lisp-mode-map 'repl-submit #'ielm-return))

(provide 'evil-collection-ielm)
;;; evil-collection-ielm.el ends here
