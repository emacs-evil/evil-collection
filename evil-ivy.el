;;; evil-ivy.el --- Evil bindings for ivy -*- lexical-binding: t -*-

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
(require 'ivy)

(defun evil-ivy-setup ()
  (evil-define-key 'normal ivy-occur-mode-map
    [mouse-1] 'ivy-occur-click
    (kbd "RET") 'ivy-occur-press-and-switch
    (kbd "j") 'ivy-occur-next-line
    (kbd "k") 'ivy-occur-previous-line
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char
    (kbd "g") nil
    (kbd "gg") 'evil-goto-first-line
    (kbd "gf") 'ivy-occur-press
    (kbd "gr") 'ivy-occur-revert-buffer
    (kbd "ga") 'ivy-occur-read-action
    (kbd "go") 'ivy-occur-dispatch
    (kbd "gc") 'ivy-occur-toggle-calling
    (kbd "q") 'quit-window)

  (evil-define-key 'normal ivy-occur-grep-mode-map
    (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode
    "gd" 'ivy-occur-delete-candidate
    [mouse-1] 'ivy-occur-click
    (kbd "RET") 'ivy-occur-press-and-switch
    (kbd "j") 'ivy-occur-next-line
    (kbd "k") 'ivy-occur-previous-line
    (kbd "h") 'evil-backward-char
    (kbd "l") 'evil-forward-char
    (kbd "g") nil
    (kbd "gg") 'evil-goto-first-line
    (kbd "gf") 'ivy-occur-press
    (kbd "gr") 'ivy-occur-revert-buffer
    (kbd "ga") 'ivy-occur-read-action
    (kbd "go") 'ivy-occur-dispatch
    (kbd "gc") 'ivy-occur-toggle-calling
    (kbd "q") 'quit-window))

(provide 'evil-ivy)
;;; evil-ivy.el ends here
