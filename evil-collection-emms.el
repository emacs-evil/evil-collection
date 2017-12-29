;;; evil-collection-emms.el --- Evil bindings for EMMS -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emms, tools

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
;; Evil bindings for EMMS.

;;; Code:
(require 'emms nil t)
(require 'evil)

(declare-function emms-with-inhibit-read-only-t "emms")
(declare-function emms-playlist-mode-correct-previous-yank "emms-playlist-mode")

(defvar emms-browser-mode-map)
(defvar emms-playlist-mode-map)

;;; TODO: Make all playback bindings consistent across modes:
;;; - stop
;;; - pause
;;; - volume
;;; - next/previous
;;; - fast-forward/backward

;;; TODO: Add bindings to emms-browser-search-mode-map and emms-metaplaylist-mode-map.

(defun evil-collection-emms-playlist-mode-insert-newline-above ()
  "Insert a newline above point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (evil-insert-newline-above)))

(defun evil-collection-emms-playlist-mode-insert-newline-below ()
  "Insert a newline below point."
  (interactive)
  (emms-with-inhibit-read-only-t
   (evil-insert-newline-below)))

(defun evil-collection-emms-playlist-mode-paste-before ()
  "Pastes the latest yanked playlist items before the cursor position.
The return value is the yanked text."
  (interactive)
  (emms-with-inhibit-read-only-t
   (goto-char (point-at-bol))
   (yank)
   (emms-playlist-mode-correct-previous-yank)
   (evil-previous-line)
   (evil-beginning-of-line)))

(defun evil-collection-emms-playlist-mode-paste-after ()
  "Pastes the latest yanked playlist items behind point.
The return value is the yanked text."
  (interactive)
  (evil-next-line)
  (evil-collection-emms-playlist-mode-paste-before))

(defun evil-collection-emms-browser-setup ()
  ;; TODO: Why doesn't evil-set-initial-state work with emms-browser-mode?
  (add-hook 'emms-browser-mode-hook 'evil-motion-state)

  (evil-define-key 'motion emms-browser-mode-map
    ;; playback controls
    "x" 'emms-pause
    "X" 'emms-stop
    "r" 'emms-random
    "<" 'emms-seek-backward
    ">" 'emms-seek-forward
    (kbd "<return>") 'emms-browser-add-tracks
    (kbd "C-<return>") 'emms-browser-add-tracks-and-play

    ;; volume controls
    "+" 'emms-volume-raise
    "=" 'emms-volume-raise
    "-" 'emms-volume-lower

    "u" 'emms-playlist-mode-undo

    ;; motion
    "[" 'emms-browser-prev-non-track
    "]" 'emms-browser-next-non-track
    "gj" 'emms-browser-prev-non-track
    "gk" 'emms-browser-next-non-track

    (kbd "<tab>") 'emms-browser-toggle-subitems
    (kbd "SPC") 'emms-browser-toggle-subitems
    ;; TODO: Use S-<tab>?
    "g1" 'emms-browser-collapse-all
    "g2" 'emms-browser-expand-to-level-2
    "g3" 'emms-browser-expand-to-level-3
    "g4" 'emms-browser-expand-to-level-4
    "g0" 'emms-browser-expand-all
    "ga" 'emms-browse-by-artist
    "gA" 'emms-browse-by-album
    "gb" 'emms-browse-by-genre
    "gy" 'emms-browse-by-year
    "gc" 'emms-browse-by-composer
    "gp" 'emms-browse-by-performer

    "/" 'emms-isearch-buffer ; This shows hidden items during search.

    ;; filter
    ;; "" 'emms-browser-previous-filter ; TODO: What does this do?
    ;; "" 'emms-browser-next-filter

    "s" (lookup-key emms-browser-mode-map (kbd "s"))
    "g" (lookup-key emms-browser-mode-map (kbd "W")) ;; TODO: This overrides other "g-" prefixed keys.

    "C" 'emms-browser-clear-playlist
    "D" 'emms-browser-delete-files
    "d" 'emms-browser-view-in-dired
    "gd" 'emms-playlist-mode-goto-dired-at-point)) ; "d" does the same, keep "gd" for consistency.

(defun evil-collection-emms-setup ()
  "Set up `evil' bindings for `emms'."
  (with-eval-after-load 'emms-browser
    (evil-collection-emms-browser-setup))
  (evil-set-initial-state 'emms-playlist-mode 'motion)

  (evil-define-key 'motion emms-playlist-mode-map
    ;; playback controls
    "x" 'emms-pause
    "X" 'emms-stop
    "r" 'emms-random
    "<" 'emms-seek-backward
    ">" 'emms-seek-forward
    (kbd "C-j") 'emms-next
    (kbd "C-k") 'emms-previous
    "gj" 'emms-next
    "gk" 'emms-previous
    (kbd "<return>") 'emms-playlist-mode-play-smart

    ;; volume controls
    "+" 'emms-volume-raise
    "=" 'emms-volume-raise
    "-" 'emms-volume-lower

    "u" 'emms-playlist-mode-undo

    ;; motion
    "gg" 'emms-playlist-mode-first
    "G" 'emms-playlist-mode-last
    "]" 'emms-playlist-mode-next
    "[" 'emms-playlist-mode-previous

    "D" 'emms-playlist-mode-kill-track ; emms-browser uses "D"
    "C" 'emms-playlist-mode-clear
    "O" 'evil-collection-emms-playlist-mode-insert-newline-above
    "o" 'evil-collection-emms-playlist-mode-insert-newline-below
    "P" 'evil-collection-emms-playlist-mode-paste-before
    "p" 'evil-collection-emms-playlist-mode-paste-after

    "u" 'emms-playlist-mode-undo

    "ze" 'emms-tag-editor-edit
    "R" 'emms-tag-editor-rename

    "." 'emms-playlist-mode-center-current
    "gd" 'emms-playlist-mode-goto-dired-at-point ; "d" does the same, keep "gd" for consistency.

    "zs" 'emms-show
    "a" 'emms-playlist-mode-add-contents
    "zp" 'emms-playlist-set-playlist-buffer

    ;; filter
    "S" (lookup-key emms-playlist-mode-map (kbd "S"))
    "s" (lookup-key emms-playlist-mode-map (kbd "/"))
    ;; "" 'emms-playlist-limit-to-all ; TODO: Test.

    (kbd "M-y") 'emms-playlist-mode-yank-pop)

  (evil-define-key 'visual emms-playlist-mode-map
    ;; "d" 'emms-playlist-mode-kill
    "D" 'emms-playlist-mode-kill))

(provide 'evil-collection-emms)
;;; evil-collection-emms.el ends here
