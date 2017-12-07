;;; evil-collection-ivy.el --- Evil bindings for ivy -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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
(require 'evil)
(require 'ivy nil t)

(defun evil-collection-ivy-setup ()
  "Set up `evil' bindings for `ivy-mode'."
  (evil-define-key 'normal ivy-occur-mode-map
    [mouse-1] 'ivy-occur-click
    (kbd "<return>") 'ivy-occur-press-and-switch
    "j" 'ivy-occur-next-line
    "k" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "g" nil
    "gg" 'evil-goto-first-line
    "gf" 'ivy-occur-press
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "gc" 'ivy-occur-toggle-calling

    ;; refresh
    "gr" 'ivy-occur-revert-buffer

    ;; quit
    "q" 'quit-window)

  (when evil-want-C-d-scroll
    (evil-define-key 'normal ivy-occur-grep-mode-map
      "D" 'ivy-occur-delete-candidate
      (kbd "C-d") 'evil-scroll-down))

  (evil-define-key 'normal ivy-occur-grep-mode-map
    (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode
    "gd" 'ivy-occur-delete-candidate
    [mouse-1] 'ivy-occur-click
    (kbd "<return>") 'ivy-occur-press-and-switch
    "j" 'ivy-occur-next-line
    "k" 'ivy-occur-previous-line
    "h" 'evil-backward-char
    "l" 'evil-forward-char
    "g" nil
    "gg" 'evil-goto-first-line
    "gf" 'ivy-occur-press
    "gr" 'ivy-occur-revert-buffer
    "ga" 'ivy-occur-read-action
    "go" 'ivy-occur-dispatch
    "gc" 'ivy-occur-toggle-calling

    ;; quit
    "q" 'quit-window))

(provide 'evil-collection-ivy)
;;; evil-collection-ivy.el ends here
