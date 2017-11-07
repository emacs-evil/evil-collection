;;; evil-eshell.el --- Evil bindings for Eshell -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, eshell, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for EMMS

;;; Code:
(require 'eshell)
(require 'evil)

(defun evil-eshell-next-prompt ()
  (when (get-text-property (point) 'read-only)
    ;; If at end of prompt, `eshell-next-prompt' will not move, so go backward.
    (beginning-of-line)
    (eshell-next-prompt 1)))

(defun evil-eshell-next-prompt-on-insert ()
  (dolist (hook '(evil-replace-state-entry-hook evil-insert-state-entry-hook))
    (add-hook hook 'evil-eshell-next-prompt nil t)))

(defun evil-eshell-interrupt-process ()
  (interactive)
  (eshell-interrupt-process)
  (evil-insert 1))

;;; `eshell-mode-map' is reset when Eshell is initialized in `eshell-mode'. We
;;; need to add bindings to `eshell-first-time-mode-hook'.
(defun evil-eshell-setup-keys ()
  (evil-define-key 'normal eshell-mode-map
    ;; motion
    "[" 'eshell-previous-prompt
    "]" 'eshell-next-prompt
    (kbd "C-k") 'eshell-previous-prompt
    (kbd "C-j") 'eshell-next-prompt
    "0" 'eshell-bol
    "^" 'eshell-bol
    (kbd "M-h") 'eshell-backward-argument
    (kbd "M-l") 'eshell-forward-argument

    (kbd "<return>") 'eshell-send-input
    (kbd "C-c C-c") 'evil-eshell-interrupt-process)
  (evil-define-key 'insert eshell-mode-map
    ;; motion
    (kbd "M-h") 'eshell-backward-argument
    (kbd "M-l") 'eshell-forward-argument)
  (evil-define-key 'visual eshell-mode-map
    ;; motion
    ;; TODO: This does not work with `evil-visual-line'.
    "[" 'eshell-previous-prompt
    "]" 'eshell-next-prompt
    (kbd "C-k") 'eshell-previous-prompt
    (kbd "C-j") 'eshell-next-prompt
    "0" 'eshell-bol
    "^" 'eshell-bol))

;; TODO: Compare this setup procedure with evil-ediff.
(defun evil-eshell-setup ()
  (add-hook 'eshell-mode-hook 'evil-eshell-next-prompt-on-insert)
  (add-hook 'eshell-first-time-mode-hook 'evil-eshell-setup-keys))

(provide 'evil-eshell)
;;; evil-eshell.el ends here
