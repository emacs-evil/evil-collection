;;; evil-collection-eat.el --- Bindings for `eat' -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Cherti

;; Maintainer: James Cherti <https://www.jamescherti.com/contact/>
;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Based on evil-collection-vterm.el by James Nguyen and Pierre Neidhardt
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, eat, tools

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
;; This package provides `evil-mode' keybindings for `eat'.

;;; Code:
(require 'evil-collection)
(require 'eat nil t)

(defconst evil-collection-eat-maps '(eat-mode-map))

(defvar-local evil-collection-eat-send-escape-to-eat-p nil
  "Track whether or not we send ESC to `eat' or `emacs'.")

(defun evil-collection-eat-toggle-send-escape ()
  "Toggle the destination of the ESC key between `eat' and `emacs'.
This adjustment is necessary for programs that utilize ESC, such as Vim or an
SSH-accessed Emacs that also uses `evil-mode'."
  (interactive)
  (if evil-collection-eat-send-escape-to-eat-p
      (evil-collection-define-key 'insert 'eat-mode-map (kbd "<escape>")
        (lookup-key evil-insert-state-map (kbd "<escape>")))
    (evil-collection-define-key 'insert 'eat-mode-map
      (kbd "<escape>") 'eat-self-input))
  (setq evil-collection-eat-send-escape-to-eat-p
        (not evil-collection-eat-send-escape-to-eat-p))
  (message (format "Sending ESC to %s."
                   (if evil-collection-eat-send-escape-to-eat-p
                       "eat"
                     "emacs"))))

;;;###autoload
(defun evil-collection-eat-setup ()
  "Set up `evil' bindings for `eat'."
  (evil-set-initial-state 'eat-mode 'insert)

  (evil-collection-define-key '(normal insert) 'eat-mode-map
    (kbd "C-c C-z") 'evil-collection-eat-toggle-send-escape)

  ;; `Evil' has some "C-" bindings in insert state that shadow the `eat' terminal
  ;; bindings. Do not send "C-c" (prefix key) nor "C-h" (help prefix) as raw input.
  (evil-collection-define-key 'insert 'eat-mode-map
    (kbd "C-a") 'eat-self-input
    (kbd "C-b") 'eat-self-input
    (kbd "C-d") 'eat-self-input
    (kbd "C-e") 'eat-self-input
    (kbd "C-f") 'eat-self-input
    (kbd "C-k") 'eat-self-input
    (kbd "C-l") 'eat-self-input
    (kbd "C-n") 'eat-self-input
    (kbd "C-o") 'eat-self-input
    (kbd "C-p") 'eat-self-input
    (kbd "C-q") 'eat-self-input
    (kbd "C-r") 'eat-self-input
    (kbd "C-s") 'eat-self-input
    (kbd "C-t") 'eat-self-input
    (kbd "C-u") 'eat-self-input
    (kbd "C-v") 'eat-self-input
    (kbd "C-w") 'eat-self-input
    (kbd "C-y") 'eat-self-input
    (kbd "C-z") 'eat-self-input
    (kbd "<delete>") 'eat-self-input))

(provide 'evil-collection-eat)
;;; evil-collection-eat.el ends here
