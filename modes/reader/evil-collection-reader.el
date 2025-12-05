;;; evil-collection-reader.el --- Evil bindings for `reader' -*- lexical-binding: t -*-

;; Copyright (C) 2025 Icy-Thought

;; Author: Icy-Thought
;; Maintainer: Icy-Thought <icy-thought@pm.me>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
;; Evil bindings for Emacs `reader'.

;;; Code:
(require 'evil-collection)
(require 'reader nil t)

(declare-function reader-first-page "reader")
(declare-function reader-last-page "reader")
(declare-function reader-goto-page "reader")
(declare-function reader-close-doc "reader")
(declare-function reader-outline-show "reader")
(declare-function reader-presentation-mode "reader")
(declare-function reader-rotate-counter-clockwise "reader")
(declare-function reader-rotate-clockwise "reader")
(declare-function reader-fit-to-width "reader")
(declare-function reader-fit-to-height "reader")
(declare-function reader-mwheel-shrink-size "reader")
(declare-function reader-shrink-size "reader")
(declare-function reader-mwheel-enlarge-size "reader")
(declare-function reader-enlarge-size "reader")
(declare-function reader-scroll-left-most "reader")
(declare-function reader-scroll-right-most "reader")
(declare-function reader-scroll-left "reader")
(declare-function reader-scroll-right "reader")
(declare-function reader-scroll-up-or-prev-page "reader")
(declare-function reader-scroll-down-or-next-page "reader")
(declare-function reader-scroll-down-screen "reader")
(declare-function reader-scroll-up-screen "reader")
(declare-function reader-mwheel-scroll-right "reader")
(declare-function reader-mwheel-scroll-left "reader")
(declare-function reader-mwheel-scroll-down "reader")
(declare-function reader-mwheel-scroll-up "reader")
(declare-function reader-scroll-down-or-next-page "reader")
(declare-function reader-scroll-up-or-prev-page "reader")
(declare-function reader-previous-page "reader")
(declare-function reader-next-page "reader")

(declare-function reader-outline-visit-page "reader")
(declare-function reader-outline-select-doc-window "reader")

(defvar reader-mode-map)
(defvar reader-outline-mode-map)

(defun evil-collection-reader-goto-first-page (&optional page)
  "Navigates to `page' X or jump to the FIRST PAGE of the document."
  (interactive "P")
  (if page
      (reader-goto-page page)
    (reader-first-page)))

(defun evil-collection-reader-goto-page (&optional page)
  "Navigates to `page' X or jump to the LAST PAGE of the document.
`evil' wrapper around `reader-last-page'."
  (interactive "P")
  (if page
      (reader-goto-page page)
    (reader-last-page)))

(defconst evil-collection-reader-maps
  '(reader-mode-map reader-outline-mode-map))

;;;###autoload
(defun evil-collection-reader-setup ()
  "Set up `evil' bindings for `reader'."
  (evil-collection-set-readonly-bindings 'reader-mode-map)

  ;; Refresh reader-mode bindings
  (add-hook 'reader-mode-hook #'evil-normalize-keymaps)
  (add-hook 'reader-outline-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key 'normal 'reader-mode-map
    "n" #'reader-next-page
    "p" #'reader-previous-page
    "k" #'reader-scroll-up-or-prev-page
    "j" #'reader-scroll-down-or-next-page
    [remap next] #'reader-scroll-down-or-next-page
    [remap previous] #'reader-scroll-up-or-prev-page
    [remap evil-next-line] #'reader-scroll-down-or-next-page
    [remap evil-previous-line] #'reader-scroll-up-or-prev-page

    "<wheel-up>" #'reader-mwheel-scroll-up
    "<wheel-down>" #'reader-mwheel-scroll-down
    "S-<wheel-up>" #'reader-mwheel-scroll-left
    "S-<wheel-down>" #'reader-mwheel-scroll-right

    "C-b" #'reader-scroll-up-screen
    "C-f" #'reader-scroll-down-screen
    [remap scroll-down-command] #'reader-scroll-up-screen
    [remap scroll-up-command] #'reader-scroll-down-screen

    "SPC" #'reader-scroll-down-or-next-page
    "DEL" #'reader-scroll-up-or-prev-page
    "S-SPC" #'reader-scroll-up-or-prev-page

    "l" #'reader-scroll-right
    "h" #'reader-scroll-left
    [remap forward-char] #'reader-scroll-right
    [remap backward-char] #'reader-scroll-left

    "$" #'reader-scroll-right-most
    "^" #'reader-scroll-left-most
    [remap move-end-of-line] #'reader-scroll-right-most
    [remap move-beginning-of-line] #'reader-scroll-left-most

    "gg" #'evil-collection-reader-goto-first-page
    "G" #'evil-collection-reader-goto-page
    [remap beginning-of-buffer] #'reader-first-page
    [remap end-of-buffer] #'reader-last-page
    [remap goto-line] #'reader-goto-page

    ;; "0" #'evil-collections-reader-reset-zoom
    "=" #'reader-enlarge-size
    "+" #'reader-enlarge-size
    "C-<wheel-up>" #'reader-mwheel-enlarge-size
    "-" #'reader-shrink-size
    "C-<wheel-down>" #'reader-mwheel-shrink-size

    "H" #'reader-fit-to-height
    "W" #'reader-fit-to-width

    "r" #'reader-rotate-clockwise
    "R" #'reader-rotate-counter-clockwise

    "<f5>" #'reader-presentation-mode
    "o" #'reader-outline-show
    "Q" #'reader-close-doc)

  (evil-collection-define-key 'normal 'reader-outline-mode-map
    "p" #'previous-line
    "n" #'next-line
    "o" #'reader-outline-select-doc-window
    "q" #'quit-window
    "RET" #'reader-outline-visit-page
    "M-RET" #'reader-outline-visit-page))

(provide 'evil-collection-reader)
;;; evil-collection-reader.el ends here
