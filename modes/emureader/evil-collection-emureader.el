;;; evil-collection-emureader.el --- Evil bindings for `emureader' -*- lexical-binding: t -*-

;; Copyright (C) 2025 Icy-Thought

;; Author: Icy-Thought
;; Maintainer: Icy-Thought <icy-thought@pm.me>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools, reader

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
;; Evil bindings for Emacs `emureader'.

;;; Code:
(require 'evil-collection)
(require 'emureader nil t)

(declare-function emureader-first-page "emureader")
(declare-function emureader-last-page "emureader")
(declare-function emureader-goto-page "emureader")
(declare-function emureader-close-doc "emureader")
(declare-function emureader-outline-show "emureader")
(declare-function emureader-presentation-mode "emureader")
(declare-function emureader-rotate-counter-clockwise "emureader")
(declare-function emureader-rotate-clockwise "emureader")
(declare-function emureader-fit-to-width "emureader")
(declare-function emureader-fit-to-height "emureader")
(declare-function emureader-mwheel-shrink-size "emureader")
(declare-function emureader-shrink-size "emureader")
(declare-function emureader-mwheel-enlarge-size "emureader")
(declare-function emureader-enlarge-size "emureader")
(declare-function emureader-scroll-left-most "emureader")
(declare-function emureader-scroll-right-most "emureader")
(declare-function emureader-scroll-left "emureader")
(declare-function emureader-scroll-right "emureader")
(declare-function emureader-scroll-up-or-prev-page "emureader")
(declare-function emureader-scroll-down-or-next-page "emureader")
(declare-function emureader-scroll-down-screen "emureader")
(declare-function emureader-scroll-up-screen "emureader")
(declare-function emureader-mwheel-scroll-right "emureader")
(declare-function emureader-mwheel-scroll-left "emureader")
(declare-function emureader-mwheel-scroll-down "emureader")
(declare-function emureader-mwheel-scroll-up "emureader")
(declare-function emureader-scroll-down-or-next-page "emureader")
(declare-function emureader-scroll-up-or-prev-page "emureader")
(declare-function emureader-previous-page "emureader")
(declare-function emureader-next-page "emureader")

(declare-function emureader-outline-visit-page "emureader")
(declare-function emureader-outline-select-doc-window "emureader")

(defvar emureader-mode-map)
(defvar emureader-outline-mode-map)

(defun evil-collection-emureader-goto-first-page (&optional page)
  "Navigates to `page' X or jump to the FIRST PAGE of the document."
  (interactive "P")
  (if page
      (emureader-goto-page page)
    (emureader-first-page)))

(defun evil-collection-emureader-goto-page (&optional page)
  "Navigates to `page' X or jump to the LAST PAGE of the document.
`evil' wrapper around `emureader-last-page'."
  (interactive "P")
  (if page
      (emureader-goto-page page)
    (emureader-last-page)))

(defconst evil-collection-emureader-maps
  '(emureader-mode-map emureader-outline-mode-map))

;;;###autoload
(defun evil-collection-emureader-setup ()
  "Set up `evil' bindings for `emureader'."
  (evil-collection-set-readonly-bindings 'emureader-mode-map)

  ;; Refresh emureader-mode bindings
  (add-hook 'emureader-mode-hook #'evil-normalize-keymaps)
  (add-hook 'emureader-outline-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key 'normal 'emureader-mode-map
    "n" #'emureader-next-page
    "p" #'emureader-previous-page
    "k" #'emureader-scroll-up-or-prev-page
    "j" #'emureader-scroll-down-or-next-page
    [remap next] #'emureader-scroll-down-or-next-page
    [remap previous] #'emureader-scroll-up-or-prev-page
    [remap evil-next-line] #'emureader-scroll-down-or-next-page
    [remap evil-previous-line] #'emureader-scroll-up-or-prev-page

    "<wheel-up>" #'emureader-mwheel-scroll-up
    "<wheel-down>" #'emureader-mwheel-scroll-down
    "S-<wheel-up>" #'emureader-mwheel-scroll-left
    "S-<wheel-down>" #'emureader-mwheel-scroll-right

    "C-b" #'emureader-scroll-up-screen
    "C-f" #'emureader-scroll-down-screen
    [remap scroll-down-command] #'emureader-scroll-up-screen
    [remap scroll-up-command] #'emureader-scroll-down-screen

    "DEL" #'emureader-scroll-up-or-prev-page

    "l" #'emureader-scroll-right
    "h" #'emureader-scroll-left
    [remap forward-char] #'emureader-scroll-right
    [remap backward-char] #'emureader-scroll-left

    "$" #'emureader-scroll-right-most
    "^" #'emureader-scroll-left-most
    [remap move-end-of-line] #'emureader-scroll-right-most
    [remap move-beginning-of-line] #'emureader-scroll-left-most

    "gg" #'evil-collection-emureader-goto-first-page
    "G" #'evil-collection-emureader-goto-page
    [remap beginning-of-buffer] #'emureader-first-page
    [remap end-of-buffer] #'emureader-last-page
    [remap goto-line] #'emureader-goto-page

    "C-<wheel-up>" #'emureader-mwheel-enlarge-size
    "C-<wheel-down>" #'emureader-mwheel-shrink-size

    "H" #'emureader-fit-to-height
    "W" #'emureader-fit-to-width

    "r" #'emureader-rotate-clockwise
    "R" #'emureader-rotate-counter-clockwise

    "<f5>" #'emureader-presentation-mode
    "o" #'emureader-outline-show
    "Q" #'emureader-close-doc)

  (evil-collection-bind 'emureader-mode-map
                        'scroll-down 'emureader-scroll-down-or-next-page
                        'scroll-up 'emureader-scroll-up-or-prev-page
                        'zoom-in  'emureader-enlarge-size
                        'zoom-out 'emureader-shrink-size)

  (evil-collection-define-key 'normal 'emureader-outline-mode-map
    "p" #'previous-line
    "n" #'next-line
    "o" #'emureader-outline-select-doc-window
    "q" #'quit-window
    "RET" #'emureader-outline-visit-page
    "M-RET" #'emureader-outline-visit-page))

(provide 'evil-collection-emureader)
;;; evil-collection-emureader.el ends here
