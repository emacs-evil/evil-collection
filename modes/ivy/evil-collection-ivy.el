;;; evil-collection-ivy.el --- Evil bindings for ivy -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, ivy, tools

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
;; Evil bindings for `ivy-mode'.

;;; Code:
(require 'evil-collection)
(require 'ivy nil t)

(defconst evil-collection-ivy-maps '(ivy-occur-mode-map
                                     ivy-occur-grep-mode-map
                                     ivy-minibuffer-map))

;;;###autoload
(defun evil-collection-ivy-setup ()
  "Set up `evil' bindings for `ivy-mode'."
  (evil-collection-define-key nil 'ivy-minibuffer-map
    (kbd "<escape>") 'minibuffer-keyboard-quit)
  (evil-collection-define-key 'normal 'ivy-occur-mode-map
    [mouse-1] 'ivy-occur-click
    "j" 'ivy-occur-next-line
    "k" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "g" nil
    "gg" 'evil-goto-first-line
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "gc" 'ivy-occur-toggle-calling)
  (evil-collection-bind 'ivy-occur-mode-map
                        'quit 'quit-window
                        'find-file 'ivy-occur-press
                        'refresh 'ivy-occur-revert-buffer
                        'action 'ivy-occur-press-and-switch)

  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'ivy-occur-grep-mode-map
      "D" 'ivy-occur-delete-candidate
      (kbd "C-d") 'evil-scroll-down))

  (evil-collection-define-key 'visual 'ivy-occur-grep-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line)

  (evil-collection-define-key 'normal 'ivy-occur-grep-mode-map
    "d" 'ivy-occur-delete-candidate
    (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode
    "i" 'ivy-wgrep-change-to-wgrep-mode
    "gd" 'ivy-occur-delete-candidate
    [mouse-1] 'ivy-occur-click
    "j" 'ivy-occur-next-line
    "k" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "g" nil
    "gg" 'evil-goto-first-line
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "gc" 'ivy-occur-toggle-calling

    "0" 'evil-digit-argument-or-evil-beginning-of-line)
  (evil-collection-bind 'ivy-occur-grep-mode-map
                        'quit 'quit-window
                        'find-file 'ivy-occur-press
                        'refresh 'ivy-occur-revert-buffer
                        'action 'ivy-occur-press-and-switch)

  (defvar evil-collection-setup-minibuffer)
  (when evil-collection-setup-minibuffer
    (evil-collection-define-key 'normal 'ivy-minibuffer-map
      (kbd "<escape>") 'abort-recursive-edit
      (kbd "C-m") 'ivy-done
      "j" 'ivy-next-line
      "k" 'ivy-previous-line)

    (evil-collection-define-key 'insert 'ivy-minibuffer-map
      (kbd "DEL") 'ivy-backward-delete-char
      (kbd "C-r") 'ivy-reverse-i-search)
    (evil-collection-bind 'ivy-minibuffer-map
                          'action 'exit-minibuffer
                          'completion-previous 'ivy-previous-line
                          'completion-next     'ivy-next-line)))

(provide 'evil-collection-ivy)
;;; evil-collection-ivy.el ends here
