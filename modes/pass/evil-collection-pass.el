;;; evil-collection-pass.el --- Evil bindings for pass-mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

(declare-function pass--display-keybinding "pass")

(defconst evil-collection-pass-maps '(pass-mode-map))

(defvar evil-collection-pass-command-to-label
  '((pass-copy-field . "yf")
    (pass-copy-username . "yn")
    (pass-copy-url . "yu"))
  "Alist holding labels to be used in `pass' header.")

(defun evil-collection-pass-display-keybinding (f &rest args)
  "A version of `pass--display-keybinding' that handles displaying
keybindings listed in `evil-collection-pass-command-to-label'."
  (if (alist-get (car args) evil-collection-pass-command-to-label)
      (insert
       (format
        "%8s %-13s \t "
        (format "%s"
                (propertize
                 (format
                  "<%s>"
                  (alist-get (car args)
                             evil-collection-pass-command-to-label))
                 'face 'font-lock-constant-face))
        (cadr args)))
    (apply f args)))

;;;###autoload
(defun evil-collection-pass-setup ()
  "Set up `evil' bindings for `pass-mode'."

  (evil-collection-bind 'pass-mode-map 'rename 'pass-rename)

  ;; https://github.com/NicolasPetton/pass/pull/47
  (when (fboundp 'pass-edit)
    (evil-collection-bind 'pass-mode-map 'edit 'pass-edit))

  (advice-add 'pass--display-keybinding
              :around 'evil-collection-pass-display-keybinding)

  (evil-collection-define-operator-key 'yank 'pass-mode-map
    "f" 'pass-copy-field
    "n" 'pass-copy-username
    "u" 'pass-copy-url)

  (evil-collection-define-key 'normal 'pass-mode-map
    "i" 'pass-insert
    "I" 'pass-insert-generated
    "Y" 'pass-copy
    "o" 'pass-otp-options)
  (evil-collection-bind 'pass-mode-map
                        'next-item 'pass-next-entry
                        'prev-item 'pass-prev-entry
                        'next-section 'pass-next-directory
                        'prev-section 'pass-prev-directory
                        'next-section-2 'pass-next-entry
                        'prev-section-2 'pass-prev-entry
                        'jump 'pass-goto-entry
                        'action 'pass-view
                        'quit 'pass-quit
                        'describe-mode 'describe-mode
                        'refresh 'pass-update-buffer
                        'search-or-filter 'isearch-forward
                        'delete 'pass-kill
                        'delete-2 'pass-kill))

(provide 'evil-collection-pass)
;;; evil-collection-pass.el ends here
