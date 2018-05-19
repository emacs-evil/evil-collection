;;; evil-collection-pdf.el --- Evil bindings for pdf-tools  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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
(require 'evil-collection-util)
(require 'pdf-tools nil t)
(require 'pdf-view nil t)

(defconst evil-collection-pdf-maps '(pdf-view-mode-map
                                     pdf-outline-buffer-mode-map
                                     pdf-occur-buffer-mode-map))

(declare-function pdf-view-last-page "pdf-view")
(declare-function pdf-view-first-page "pdf-view")
(declare-function pdf-view-goto-page "pdf-view")
(defvar pdf-view-mode-map)
(defvar pdf-outline-buffer-mode-map)
(defvar pdf-occur-buffer-mode-map)

(defvar pdf-view-mode-map)
(defvar pdf-outline-buffer-mode-map)
(defvar pdf-occur-buffer-mode-map)

(defun evil-collection-pdf-view-goto-page (&optional page)
  "`evil' wrapper around `pdf-view-last-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-last-page)
    (image-eob)))

(defun evil-collection-pdf-view-goto-first-page (&optional page)
  "`evil' wrapper around `pdf-view-first-page'."
  (interactive "P")
  (if page
      (pdf-view-goto-page page)
    (pdf-view-first-page)
    (image-bob)))

(defun evil-collection-pdf-setup ()
  "Set up `evil' bindings for `pdf-view'."
  (evil-collection-util-inhibit-insert-state pdf-view-mode-map)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-define-key 'normal pdf-view-mode-map
    ;; motion
    (kbd "<return>") 'image-next-line
    "j" 'pdf-view-next-line-or-next-page
    "k" 'pdf-view-previous-line-or-previous-page
    (kbd "SPC") 'pdf-view-scroll-up-or-next-page
    (kbd "S-SPC") 'pdf-view-scroll-down-or-previous-page
    (kbd "<delete>") 'pdf-view-scroll-down-or-previous-page
    (kbd "C-f") 'pdf-view-scroll-up-or-next-page
    (kbd "C-b") 'pdf-view-scroll-down-or-previous-page
    "]" 'pdf-view-next-page-command
    "[" 'pdf-view-previous-page-command
    (kbd "C-j") 'pdf-view-next-page-command
    (kbd "C-k") 'pdf-view-previous-page-command
    "gj" 'pdf-view-next-page-command
    "gk" 'pdf-view-previous-page-command
    (kbd "<next>") 'forward-page
    (kbd "<prior>") 'backward-page
    (kbd "<down>") 'pdf-view-next-line-or-next-page
    (kbd "<up>") 'pdf-view-previous-line-or-previous-page
    "gg" 'evil-collection-pdf-view-goto-first-page
    "G" 'evil-collection-pdf-view-goto-page

    ;; mark
    "'" 'pdf-view-jump-to-register
    "m" 'pdf-view-position-to-register

    ;; zoom
    "+" 'pdf-view-enlarge
    "-" 'pdf-view-shrink
    "0" 'pdf-view-scale-reset
    "=" 'pdf-view-enlarge

    ;; TODO: Why are those image-* bindings in pdf-tools?
    "a+" 'image-increase-speed
    "a-" 'image-decrease-speed
    "a0" 'image-reset-speed
    "ar" 'image-reverse-speed
    "F" 'image-goto-frame
    "b" 'image-previous-frame
    "f" 'image-next-frame
    "h" 'image-backward-hscroll
    "^" 'image-bol
    "$" 'image-eol
    "l" 'image-forward-hscroll

    "H" 'pdf-view-fit-height-to-window ; evil-image has "H"
    "P" 'pdf-view-fit-page-to-window
    "W" 'pdf-view-fit-width-to-window ; evil-image has "W"

    ;; refresh
    "gr" 'revert-buffer

    (kbd "<C-down-mouse-1>") 'pdf-view-mouse-extend-region
    (kbd "<M-down-mouse-1>") 'pdf-view-mouse-set-region-rectangle
    (kbd "<down-mouse-1>")  'pdf-view-mouse-set-region

    (kbd "C-c C-c") 'docview-mode
    (kbd "C-c <tab>") 'pdf-view-extract-region-image

    "sb" 'pdf-view-set-slice-from-bounding-box
    "sm" 'pdf-view-set-slice-using-mouse
    "sr" 'pdf-view-reset-slice

    ;; goto
    "gl" 'pdf-view-goto-label

    "y" 'pdf-view-kill-ring-save

    ;; search
    (kbd "M-s o") 'pdf-occur ; TODO: More Evil bindings?

    "/" #'isearch-forward
    "?" #'isearch-backward
    "n" #'isearch-repeat-forward
    "N" #'isearch-repeat-backward

    "zd" 'pdf-view-dark-minor-mode
    "zm" 'pdf-view-midnight-minor-mode
    "zp" 'pdf-view-printer-minor-mode

    ;; quit
    "q" 'quit-window
    "Q" 'kill-this-buffer
    "ZQ" 'kill-this-buffer
    "ZZ" 'quit-window)

  (evil-collection-util-inhibit-insert-state pdf-outline-buffer-mode-map)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
  (evil-define-key 'normal pdf-outline-buffer-mode-map
    ;; open
    (kbd "<return>") 'pdf-outline-follow-link-and-quit
    (kbd "S-<return>") 'pdf-outline-follow-link
    (kbd "M-<return>") 'pdf-outline-display-link
    "go" 'pdf-outline-follow-link
    "." 'pdf-outline-move-to-current-page
    (kbd "SPC") 'pdf-outline-select-pdf-window

    "G" 'pdf-outline-end-of-buffer
    "^" 'pdf-outline-up-heading
    "<" 'pdf-outline-up-heading ; TODO: Don't set this by default?

    "zf" 'pdf-outline-follow-mode ; Helm has "C-c C-f" in Emacs state.

    ;; quit
    (kbd "C-w q") 'pdf-outline-quit-and-kill ; TODO: Do we need to set this? I think not.
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'pdf-outline-quit-and-kill)

  (evil-collection-util-inhibit-insert-state pdf-occur-buffer-mode-map)
  (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)
  (evil-define-key 'normal pdf-occur-buffer-mode-map
    ;; open
    (kbd "<return>") 'pdf-occur-goto-occurrence
    (kbd "S-<return>") 'pdf-occur-view-occurrence
    (kbd "SPC") 'pdf-occur-view-occurrence
    "gd" 'pdf-occur-goto-occurrence
    "gD" 'pdf-occur-view-occurrence

    "A" 'pdf-occur-tablist-gather-documents
    "D" 'pdf-occur-tablist-do-delete

    ;; sort
    "o" 'tabulated-list-sort
    "O" 'tablist-sort ; TODO: Do we need this?

    ;; refresh
    "G" 'tablist-revert

    "K" 'pdf-occur-abort-search

    ;; mark
    "*m" 'tablist-mark-forward
    "m" 'tablist-mark-forward
    "~" 'tablist-toggle-marks
    "u" 'tablist-unmark-forward
    "U" 'tablist-unmark-all-marks
    "*!" 'tablist-unmark-all-marks
    "*c" 'tablist-change-marks
    "*n" 'tablist-mark-items-numeric
    "*r" 'tablist-mark-items-regexp
    "%"  'tablist-mark-items-regexp

    "a" 'tablist-flag-forward

    ;; "f" 'tablist-find-entry ; TODO: Equivalent to 'pdf-occur-goto-occurrence?
    "r" 'pdf-occur-revert-buffer-with-args
    "d" 'tablist-do-kill-lines
    "x" 'pdf-occur-tablist-do-flagged-delete
    (kbd "<delete>") 'tablist-unmark-backward
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<backtab>") 'tablist-backward-column
    (kbd "C-c C-e") 'tablist-export-csv

    [remap evil-first-non-blank] 'tablist-move-to-major-columnj
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
    "/z" 'tablist-suspend-filter

    ;; quit
    "q" 'tablist-quit
    "ZQ" 'tablist-quit
    "ZZ" 'tablist-quit))

(provide 'evil-collection-pdf)
;;; evil-collection-pdf.el ends here
