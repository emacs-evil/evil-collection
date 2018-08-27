;;; evil-collection-occur.el --- Evil bindings for occur -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, occur, tools

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
;; Evil bindings for `occur'.

;;; Code:
(require 'evil-collection)

(when (> emacs-major-version 25)
  (require 'replace))

(defconst evil-collection-occur-maps '(occur-mode-map
                                       occur-edit-mode-map))

(defun evil-collection-occur-setup ()
  "Set up `evil' bindings for `occur'."
  (evil-set-initial-state 'occur-mode 'normal)

  (evil-collection-define-key 'normal 'occur-mode-map
    ;; Like `wdired-mode'.
    (kbd "C-x C-q") 'occur-edit-mode

    [mouse-2] 'occur-mode-mouse-goto
    (kbd "C-c C-c") 'occur-mode-goto-occurrence

    ;; open
    (kbd "<return>") 'occur-mode-goto-occurrence
    (kbd "S-<return>") 'occur-mode-goto-occurrence-other-window
    (kbd "M-<return>") 'occur-mode-display-occurrence
    "go" 'occur-mode-goto-occurrence-other-window

    "gj" 'occur-next
    "gk" 'occur-prev
    (kbd "C-j") 'occur-next
    (kbd "C-k") 'occur-prev
    "r" 'occur-rename-buffer
    "c" 'clone-buffer
    (kbd "C-c C-f") 'next-error-follow-minor-mode)

  (evil-collection-define-key 'normal 'occur-edit-mode-map
    ;; Like `wdired-mode'.
    (kbd "C-x C-q") 'occur-cease-edit

    [mouse-2] 'occur-mode-mouse-goto
    (kbd "C-c C-c") 'occur-cease-edit
    (kbd "C-o") 'occur-mode-display-occurrence
    (kbd "C-c C-f") 'next-error-follow-minor-mode))

(provide 'evil-collection-occur)
;;; evil-collection-occur.el ends here
