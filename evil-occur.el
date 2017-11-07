;;; evil-occur.el --- Evil integration for occur. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, occur
;; HomePage: https://github.com/jojojames/evil-collection

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
;; Evil integration for `occur'.

;;; Code:
(require 'evil-collection-util)

(when (> emacs-major-version 25)
  (require 'replace))

(defun evil-occur-setup ()
  (setq evil-emacs-state-modes
        (remove 'occur-mode evil-emacs-state-modes))

  (evil-define-key 'normal occur-mode-map
    ;; Like `wdired-mode'.
    "\C-x\C-q" 'occur-edit-mode

    [mouse-2] 'occur-mode-mouse-goto
    "\C-c\C-c" 'occur-mode-goto-occurrence
    "\C-m" 'occur-mode-goto-occurrence
    "o" 'occur-mode-goto-occurrence-other-window
    "\C-o" 'occur-mode-display-occurrence
    "\C-j" 'occur-next
    "\C-k" 'occur-prev
    "r" 'occur-rename-buffer
    "c" 'clone-buffer
    "\C-c\C-f" 'next-error-follow-minor-mode)

  (evil-define-key 'normal occur-edit-mode-map
    ;; Like `wdired-mode'.
    "\C-x\C-q" 'occur-cease-edit

    [mouse-2] 'occur-mode-mouse-goto
    "\C-c\C-c" 'occur-cease-edit
    "\C-o" 'occur-mode-display-occurrence
    "\C-c\C-f" 'next-error-follow-minor-mode))

(provide 'evil-occur)
;;; evil-occur.el ends here
