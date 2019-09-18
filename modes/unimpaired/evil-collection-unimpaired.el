;;; evil-collection-unimpaired.el --- Evil Collection port of Unimpaired -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, unimpaired, tools

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
;; `evil' port of unimpaired for `evil-collection'.
;; https://github.com/tpope/vim-unimpaired

;;; Code:
(require 'evil-collection)

(defconst evil-collection-unimpaired-maps '(evil-collection-unimpaired-mode-map))

(defvar evil-collection-unimpaired-mode-map (make-sparse-keymap)
  "Keymap for `evil-collection-unimpaired-mode'.")

(define-minor-mode evil-collection-unimpaired-mode
  ""
  nil
  nil
  evil-collection-unimpaired-mode-map)

;;;###autoload
(define-global-minor-mode global-evil-collection-unimpaired-mode
  evil-collection-unimpaired-mode evil-collection-unimpaired-mode-on)

(defun evil-collection-unimpaired-mode-on ()
  "Turn on `evil-collection-unimpaired-mode'."
  (evil-collection-unimpaired-mode 1))

(defun evil-collection-unimpaired-next-error ()
  "Go to next error."
  (interactive)
  (cond
   ((and (bound-and-true-p flycheck-mode)
         (fboundp 'flycheck-next-error))
    (flycheck-next-error))
   ((and (bound-and-true-p flymake-mode)
         (fboundp 'flymake-goto-next-error))
    (flymake-goto-next-error))
   (:default
    (message "No linting modes are on."))))

(defun evil-collection-unimpaired-previous-error ()
  "Go to previous error."
  (interactive)
  (cond
   ((and (bound-and-true-p flycheck-mode)
         (fboundp 'flycheck-previous-error))
    (flycheck-previous-error))
   ((and (bound-and-true-p flymake-mode)
         (fboundp 'flymake-goto-prev-error))
    (flymake-goto-prev-error))
   (:default
    (message "No linting modes are on."))))

;;;###autoload
(defun evil-collection-unimpaired-setup ()
  "Set up unimpaired-like bindings."
  (global-evil-collection-unimpaired-mode 1)
  (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
    "[b" 'previous-buffer
    "]b" 'next-buffer
    "]l" 'evil-collection-unimpaired-next-error
    "[l" 'evil-collection-unimpaired-previous-error))

(provide 'evil-collection-unimpaired)
;;; evil-collection-unimpaired.el ends here
