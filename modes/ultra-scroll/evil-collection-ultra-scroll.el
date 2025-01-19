;;; evil-collection-ultra-scroll.el --- Evil integration for ultra-scroll -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Allen

;; Author: Steven Allen <steven@stebalien.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, ultra-scroll, tools

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
;; Evil integration for ultra-scroll.

;;; Code:

(require 'evil-collection)
(require 'ultra-scroll nil t)

(defvar evil-collection-ultra-scroll-hide-cursor-states
  '(evil-motion-state-cursor evil-normal-state-cursor)
  "States in which to hide the cursor when scrolling.")

(defun evil-collection-ultra-scroll--hide-cursor ()
  "A special Evil cursor type that simply hides the cursor."
  (setq cursor-type nil))

(defun evil-collection-ultra-scroll--set-cursor-visible (arg)
  "Make the cursor invisible when called with ARG < 1, or visible otherwise."
  (when (bound-and-true-p evil-local-mode)
    (if (and (numberp arg) (< arg 1))
        (dolist (var evil-collection-ultra-scroll-hide-cursor-states)
          (make-local-variable var)
          (set var #'evil-collection-ultra-scroll--hide-cursor))
      (mapc #'kill-local-variable evil-collection-ultra-scroll-hide-cursor-states)
      (evil-refresh-cursor))))

(defun evil-collection-ultra-scroll-setup ()
  "Set up `evil' integration for `ultra-scroll'."
  (add-hook 'ultra-scroll-hide-functions #'evil-collection-ultra-scroll--set-cursor-visible))

(provide 'evil-collection-ultra-scroll)

;;; evil-collection-ultra-scroll.el ends here
