;;; evil-collection-markdown-ts-appear.el --- Evil integration for `markdown-ts-appear' -*- lexical-binding: t -*-

;; Copyright (C) 2026 Fredrik Bergroth

;; Author: Fredrik Bergroth <fbergroth@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, convenience, markdown

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
;; Evil integration for `markdown-ts-appear'.
;;
;; Markup at point is only revealed in the states listed in
;; `evil-collection-markdown-ts-appear-states', so navigating in
;; normal state keeps the rendered view stable.

;;; Code:
(require 'evil-collection)
(require 'markdown-ts-appear nil t)

(defvar markdown-ts-appear-mode)
(declare-function markdown-ts-appear-start "markdown-ts-appear")
(declare-function markdown-ts-appear-stop "markdown-ts-appear")

(defconst evil-collection-markdown-ts-appear-maps nil)

(defcustom evil-collection-markdown-ts-appear-states '(insert replace emacs)
  "Evil states in which `markdown-ts-appear-mode' reveals markup at point.
In all other states the markup stays rendered."
  :group 'evil-collection
  :type '(repeat symbol))

(defun evil-collection-markdown-ts-appear-sync ()
  "Reveal markup at point only in `evil-collection-markdown-ts-appear-states'."
  (if (or (not evil-local-mode)
          (memq evil-state evil-collection-markdown-ts-appear-states))
      (markdown-ts-appear-start)
    (markdown-ts-appear-stop)))

(defun evil-collection-markdown-ts-appear-setup-buffer ()
  "Add or remove the buffer-local hooks for `markdown-ts-appear-mode'."
  (dolist (hook `(evil-local-mode-hook
                  ,@(mapcar (lambda (state)
                              (evil-state-property (car state) :entry-hook))
                            evil-state-properties)))
    (if markdown-ts-appear-mode
        (add-hook hook #'evil-collection-markdown-ts-appear-sync nil t)
      (remove-hook hook #'evil-collection-markdown-ts-appear-sync t)))
  (when markdown-ts-appear-mode
    (evil-collection-markdown-ts-appear-sync)))

;;;###autoload
(defun evil-collection-markdown-ts-appear-setup ()
  "Set up `evil' integration for `markdown-ts-appear'."
  (add-hook 'markdown-ts-appear-mode-hook
            #'evil-collection-markdown-ts-appear-setup-buffer))

(provide 'evil-collection-markdown-ts-appear)
;;; evil-collection-markdown-ts-appear.el ends here
