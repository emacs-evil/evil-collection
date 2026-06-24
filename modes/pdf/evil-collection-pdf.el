;;; evil-collection-pdf.el --- Evil bindings for pdf-tools  -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2024 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, pdf, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for pdf-tools.

;;; Code:
(require 'evil-collection)
(require 'pdf-tools nil t)
(require 'pdf-view nil t)
(require 'pdf-history nil t)

(defconst evil-collection-pdf-maps '(pdf-view-mode-map
                                     pdf-history-minor-mode-map
                                     pdf-outline-buffer-mode-map
                                     pdf-occur-buffer-mode-map))

(declare-function pdf-view-last-page "pdf-view")
(declare-function pdf-view-first-page "pdf-view")
(declare-function pdf-view-goto-page "pdf-view")
(declare-function pdf-view-previous-line-or-previous-page "pdf-view")
(declare-function pdf-view-next-line-or-next-page "pdf-view")
(declare-function pdf-view-assert-active-region "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")
(declare-function pdf-view-deactivate-region "pdf-view")
(declare-function pdf-history-forward "pdf-history")
(declare-function pdf-history-backward "pdf-history")

(defvar pdf-view-mode-map)
(defvar pdf-history-minor-mode-map)
(defvar pdf-outline-buffer-mode-map)
(defvar pdf-occur-buffer-mode-map)

(defun evil-collection-pdf-view-goto-page (&optional page)
  "`evil' wrapper around `pdf-view-last-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (let ((hscroll (window-hscroll)))
      (pdf-view-last-page)
      (image-eob)
      (image-set-window-hscroll hscroll))))

(defun evil-collection-pdf-view-goto-first-page (&optional page)
  "`evil' wrapper around `pdf-view-first-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (let ((hscroll (window-hscroll)))
      (pdf-view-first-page)
      (image-bob)
      (image-set-window-hscroll hscroll))))

(defun evil-collection-pdf-jump-forward (&optional count)
  "Wrap `pdf-history-forward' with `evil'.

Consider COUNT."
  (interactive "P")
  (pdf-history-forward (or count 1)))

(defun evil-collection-pdf-jump-backward (&optional count)
  "Wrap `pdf-history-backward' with `evil'.

Consider COUNT."
  (interactive "P")
  (pdf-history-backward (or count 1)))

(defun evil-collection-pdf-yank ()
  "Save the text of the active region into the currently selected register."
  (interactive)
  (pdf-view-assert-active-region)
  (let ((txt (pdf-view-active-region-text))
        (reg evil-this-register))
    (pdf-view-deactivate-region)
    (evil-set-register
     (or reg ?\")
     (mapconcat #'identity txt nil))))

(defun evil-collection-pdf-disable-visual-mode ()
  "Don't enter visual-mode when the mark is activated.
Rationale: pdf-view allows using the region to select text within the pdf,
but evil-mode does not know how to do this. It selects entire pdf images
instead, which is useless and counterintuitive."
  (add-hook 'evil-local-mode-hook
            (lambda () (remove-hook
                        'activate-mark-hook
                        'evil-visual-activate-hook
                        t))
            nil t))

;;;###autoload
(defun evil-collection-pdf-setup ()
  "Set up `evil' bindings for `pdf-view'."
  (evil-collection-inhibit-insert-state 'pdf-view-mode-map)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-collection-define-key 'normal 'pdf-view-mode-map
    ;; motion
    (kbd "RET") 'image-next-line
    "j" 'pdf-view-next-line-or-next-page
    "k" 'pdf-view-previous-line-or-previous-page
    (kbd "<delete>") 'pdf-view-scroll-down-or-previous-page
    (kbd "C-f") 'pdf-view-scroll-up-or-next-page
    (kbd "C-b") 'pdf-view-scroll-down-or-previous-page
    (kbd "<next>") 'forward-page
    (kbd "<prior>") 'backward-page
    (kbd "<down>") 'pdf-view-next-line-or-next-page
    (kbd "<up>") 'pdf-view-previous-line-or-previous-page
    "gg" 'evil-collection-pdf-view-goto-first-page
    "G" 'evil-collection-pdf-view-goto-page

    ;; mark
    "'" 'pdf-view-jump-to-register
    "m" 'pdf-view-position-to-register

    ;; vim-fold-style synonyms kept inline.
    "zi" 'pdf-view-enlarge
    "zo" 'pdf-view-shrink
    "z0" 'pdf-view-scale-reset

    "f" 'pdf-links-isearch-link
    "F" 'pdf-links-action-perform
    "h" 'image-backward-hscroll
    "^" 'image-bol
    "$" 'image-eol
    "l" 'image-forward-hscroll

    "H" 'pdf-view-fit-height-to-window ; evil-image has "H"
    "P" 'pdf-view-fit-page-to-window
    "W" 'pdf-view-fit-width-to-window ; evil-image has "W"

    (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
    (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
    (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region

    (kbd "C-c C-c") 'doc-view-mode
    (kbd "C-c <tab>") 'pdf-view-extract-region-image

    "sb" 'pdf-view-set-slice-from-bounding-box
    "sm" 'pdf-view-set-slice-using-mouse
    "sr" 'pdf-view-reset-slice

    ;; goto
    "gl" 'pdf-view-goto-label

    ;; search
    (kbd "M-s o") 'pdf-occur ; TODO: More Evil bindings?

    "/" 'isearch-forward
    "?" 'isearch-backward
    "n" 'isearch-repeat-forward
    "N" 'isearch-repeat-backward

    "zd" 'pdf-view-dark-minor-mode
    "zm" 'pdf-view-midnight-minor-mode
    "zp" 'pdf-view-printer-minor-mode

    "o" 'pdf-outline

    ;; quit
    "Q" 'kill-current-buffer)
  (evil-collection-bind 'pdf-view-mode-map
                        'scroll-down 'pdf-view-scroll-up-or-next-page
                        'scroll-up 'pdf-view-scroll-down-or-previous-page
                        'next-item 'pdf-view-next-page-command
                        'prev-item 'pdf-view-previous-page-command
                        'next-section 'pdf-view-next-page-command
                        'prev-section 'pdf-view-previous-page-command
                        'quit 'quit-window
                        'quit-save 'quit-window
                        'quit-cancel 'kill-current-buffer
                        'refresh 'revert-buffer
                        'zoom-in    'pdf-view-enlarge
                        'zoom-out   'pdf-view-shrink
                        'zoom-reset 'pdf-view-scale-reset)


  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'pdf-view-mode-map
      (kbd "C-d") 'pdf-view-scroll-up-or-next-page))
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'pdf-view-mode-map
      (kbd "C-u") 'pdf-view-scroll-down-or-previous-page))

  (add-hook 'pdf-view-mode-hook #'evil-collection-pdf-disable-visual-mode)

  (evil-collection-define-key 'normal 'pdf-view-mode-map
    "y" 'evil-collection-pdf-yank)

  (evil-collection-inhibit-insert-state 'pdf-history-minor-mode-map)
  (evil-set-initial-state 'pdf-history-minor-mode 'normal)
  (evil-collection-define-key 'normal 'pdf-history-minor-mode-map
    ;; history forward / backward
    (kbd "C-i") 'evil-collection-pdf-jump-forward
    (kbd "C-o") 'evil-collection-pdf-jump-backward)

  (evil-collection-inhibit-insert-state 'pdf-outline-buffer-mode-map)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
  (evil-collection-define-key 'normal 'pdf-outline-buffer-mode-map
    "." 'pdf-outline-move-to-current-page
    (kbd "SPC") 'pdf-outline-select-pdf-window

    "G" 'pdf-outline-end-of-buffer
    "^" 'pdf-outline-up-heading
    "<" 'pdf-outline-up-heading ; TODO: Don't set this by default?

    "zf" 'pdf-outline-follow-mode ; Helm has "C-c C-f" in Emacs state.

    ;; quit
    (kbd "C-w q") 'pdf-outline-quit-and-kill) ; TODO: Do we need to set this? I think not.
  (evil-collection-bind 'pdf-outline-buffer-mode-map
                        'section-toggle 'outline-toggle-children
                        'section-toggle-all 'pdf-outline-toggle-subtree
                        'action 'pdf-outline-follow-link-and-quit
                        'action-other 'pdf-outline-follow-link
                        'action-stay 'pdf-outline-display-link
                        'quit 'quit-window
                        'quit-save 'pdf-outline-quit-and-kill
                        'quit-cancel 'quit-window)

  (evil-collection-inhibit-insert-state 'pdf-occur-buffer-mode-map)
  (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)
  (evil-collection-define-key 'normal 'pdf-occur-buffer-mode-map
    (kbd "SPC") 'pdf-occur-view-occurrence
    "gD" 'pdf-occur-view-occurrence

    "A" 'pdf-occur-tablist-gather-documents
    "D" 'tablist-do-kill-lines

    ;; sort
    "o" 'tabulated-list-sort
    "O" 'tablist-sort ; TODO: Do we need this?

    ;; refresh
    "G" 'tablist-revert

    "K" 'pdf-occur-abort-search

    ;; mark
    "*m" 'tablist-mark-forward
    "~" 'tablist-toggle-marks
    "*!" 'tablist-unmark-all-marks
    "*c" 'tablist-change-marks
    "*n" 'tablist-mark-items-numeric
    "*r" 'tablist-mark-items-regexp
    "%"  'tablist-mark-items-regexp

    "a" 'tablist-flag-forward

    ;; "f" 'tablist-find-entry ; TODO: Equivalent to 'pdf-occur-goto-occurrence?
    "r" 'pdf-occur-revert-buffer-with-args
    (kbd "<delete>") 'tablist-unmark-backward
    (kbd "<backtab>") 'tablist-backward-column
    (kbd "C-c C-e") 'tablist-export-csv

    [remap evil-first-non-blank] 'tablist-move-to-major-column
    [remap evil-next-line] 'tablist-next-line
    [remap evil-previous-line] 'tablist-previous-line

    ;; filter
    ;; TODO: See if overriding "/" is a good idea.
    "/!" 'tablist-negate-filter
    "//" 'tablist-display-filter
    "/=" 'tablist-push-equal-filter
    "/C" 'tablist-clear-filter
    "/D" 'tablist-delete-named-filter
    "/a" 'tablist-push-named-filter
    "/d" 'tablist-deconstruct-named-filter
    "/e" 'tablist-edit-filter
    "/n" 'tablist-push-numeric-filter
    "/p" 'tablist-pop-filter
    "/r" 'tablist-push-regexp-filter
    "/s" 'tablist-name-current-filter
    "/t" 'tablist-toggle-first-filter-logic
    "/z" 'tablist-suspend-filter)
  (evil-collection-bind 'pdf-occur-buffer-mode-map
                        'mark 'tablist-mark-forward
                        'unmark 'tablist-unmark-forward
                        'unmark-all 'tablist-unmark-all-marks
                        'execute-marks 'pdf-occur-tablist-do-flagged-delete
                        'scroll-up 'scroll-down-command
                        'action 'pdf-occur-goto-occurrence
                        'action-other 'pdf-occur-view-occurrence
                        'quit 'tablist-quit
                        'quit-save 'tablist-quit
                        'quit-cancel 'tablist-quit
                        'delete 'pdf-occur-tablist-do-delete
                        'find-definition 'pdf-occur-goto-occurrence))

(provide 'evil-collection-pdf)
;;; evil-collection-pdf.el ends here
