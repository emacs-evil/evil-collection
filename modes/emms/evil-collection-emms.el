;;; evil-collection-emms.el --- Evil bindings for EMMS -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2024 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'evil-collection)

(declare-function emms-with-inhibit-read-only-t "emms")
(declare-function emms-playlist-mode-correct-previous-yank "emms-playlist-mode")
(declare-function emms-playlist-mode-yank "emms-playlist-mode")

(defvar emms-browser-mode-map)
(defvar emms-playlist-mode-map)

(defconst evil-collection-emms-maps '(emms-browser-mode-map
                                      emms-playlist-mode-map
                                      emms-browser-search-mode-map
                                      emms-metaplaylist-mode-map
                                      emms-stream-mode-map))

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
   (goto-char (line-beginning-position))
   (emms-playlist-mode-yank)
   (emms-playlist-mode-correct-previous-yank)
   (evil-previous-line)
   (evil-beginning-of-line)))

(defun evil-collection-emms-playlist-mode-paste-after ()
  "Pastes the latest yanked playlist items behind point.
The return value is the yanked text."
  (interactive)
  (unless (eobp) (evil-next-line))
  (evil-collection-emms-playlist-mode-paste-before))

;;;###autoload
(defun evil-collection-emms-browser-setup ()
  "Set up `evil' bindings for `emms-browser'."
  ;; TODO: Why doesn't evil-set-initial-state work with emms-browser-mode?

  (evil-collection-inhibit-insert-state 'emms-browser-mode-map)
  (add-hook 'emms-browser-mode-hook 'evil-normal-state)
  (evil-collection-define-key 'normal 'emms-browser-mode-map
    ;; The following overrides other "g-" and "s-" prefixed keys so we set it first.
    "g" nil
    "g" (lookup-key emms-browser-mode-map (kbd "W"))
    "s" nil
    "s" (lookup-key emms-browser-mode-map (kbd "s"))

    ;; playback controls
    "x" 'emms-pause
    "X" 'emms-stop
    "r" 'emms-random
    "<" 'emms-seek-backward
    ">" 'emms-seek-forward
    (kbd "C-<return>") 'emms-browser-add-tracks-and-play

    ;; volume controls
    "+" 'emms-volume-raise
    "=" 'emms-volume-raise
    "-" 'emms-volume-lower

    "u" 'emms-playlist-mode-undo

    ;; motion
    "^" 'emms-browser-move-up-level
    (kbd "SPC") 'emms-browser-toggle-subitems
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
    "zm" 'emms-browser-collapse-all
    "zr" 'emms-browser-expand-all
    "zo" 'emms-browser-expand-one-level
    ;; TODO find a real replacement for zc
    "zc" 'emms-browser-collapse-all

    ;; TODO find a way to integrate this with evil-collection-evil-search
    "/" 'emms-isearch-buffer ; This shows hidden items during search.
    "n" 'isearch-repeat-forward
    "N" 'isearch-repeat-backward

    ;; filter
    ;; "" 'emms-browser-previous-filter ; TODO: What does this do?
    ;; "" 'emms-browser-next-filter


    "C" 'emms-browser-clear-playlist
    "D" 'emms-browser-remove-tracks
    "d" 'emms-browser-view-in-dired
    ;; "d" does the same, keep "gd" for consistency.
    "gd" 'emms-browser-view-in-dired)
  (evil-collection-bind 'emms-browser-mode-map
                        'action 'emms-browser-add-tracks
                        'next-item 'emms-browser-next-non-track
                        'prev-item 'emms-browser-prev-non-track
                        'next-section 'emms-browser-next-non-track
                        'prev-section 'emms-browser-prev-non-track
                        'section-toggle 'emms-browser-toggle-subitems-recursively
                        'section-toggle-all 'emms-browser-toggle-subitems-recursively)

  (unless evil-collection-always-run-setup-hook-after-load
    (run-hook-with-args 'evil-collection-setup-hook
                        'emms '(emms-browser-mode-map))))

;;;###autoload
(defun evil-collection-emms-playlist-setup ()
  "Set up `evil' bindings for `emms-playlist'."
  (evil-set-initial-state 'emms-playlist-mode 'normal)
  (evil-collection-bind 'emms-playlist-mode-map 'rename 'emms-tag-editor-rename)
  (evil-collection-define-key 'normal 'emms-playlist-mode-map
    ;; playback controls
    "x" 'emms-pause
    "X" 'emms-stop
    "r" 'emms-random
    "<" 'emms-seek-backward
    ">" 'emms-seek-forward

    ;; volume controls
    "+" 'emms-volume-raise
    "=" 'emms-volume-raise
    "-" 'emms-volume-lower

    "u" 'emms-playlist-mode-undo

    ;; motion
    "gg" 'emms-playlist-mode-first
    "G" 'emms-playlist-mode-last

    "D" 'emms-playlist-mode-kill-track  ; emms-browser uses "D"
    "C" 'emms-playlist-clear
    "O" 'evil-collection-emms-playlist-mode-insert-newline-above
    "o" 'evil-collection-emms-playlist-mode-insert-newline-below
    "P" 'evil-collection-emms-playlist-mode-paste-before
    "p" 'evil-collection-emms-playlist-mode-paste-after

    "u" 'emms-playlist-mode-undo

    "ze" 'emms-tag-editor-edit

    "." 'emms-playlist-mode-center-current
    "d" 'emms-playlist-mode-goto-dired-at-point
    "gd" 'emms-playlist-mode-goto-dired-at-point ; "d" does the same, keep "gd" for consistency.

    "zs" 'emms-show
    "a" 'emms-playlist-mode-add-contents
    "zp" 'emms-playlist-set-playlist-buffer

    ;; filter
    "S" (lookup-key emms-playlist-mode-map (kbd "S"))
    "s" (lookup-key emms-playlist-mode-map (kbd "/"))
    ;; "" 'emms-playlist-limit-to-all ; TODO: Test.

    (kbd "M-y") 'emms-playlist-mode-yank-pop)
  (evil-collection-bind 'emms-playlist-mode-map
                        'action 'emms-playlist-mode-play-smart
                        'next-item 'emms-next
                        'prev-item 'emms-previous
                        'next-section 'emms-playlist-mode-next
                        'prev-section 'emms-playlist-mode-previous
                        'next-section-2 'emms-next
                        'prev-section-2 'emms-previous)

  (evil-collection-define-key 'visual 'emms-playlist-mode-map
    ;; "d" 'emms-playlist-mode-kill
    "D" 'emms-playlist-mode-kill)

  (unless evil-collection-always-run-setup-hook-after-load
    (run-hook-with-args 'evil-collection-setup-hook
                        'emms '(emms-playlist-mode-map))))

;;;###autoload
(defun evil-collection-emms-setup ()
  "Set up `evil' bindings for `emms'."
  ;; emms-browser and emms-playlist-mode must be set up after they are loaded
  ;; because we need the mode map to be defined when we `(lookup-key
  ;; ...-mode-map ...)'.
  (with-eval-after-load 'emms-browser
    (evil-collection-emms-browser-setup))
  (with-eval-after-load 'emms-playlist-mode
    (evil-collection-emms-playlist-setup))

  (evil-collection-bind 'emms-browser-search-mode-map 'quit 'emms-browser-kill-search)

  (evil-set-initial-state 'emms-metaplaylist-mode 'normal)
  (evil-collection-define-key 'normal 'emms-metaplaylist-mode-map
    (kbd "<space>") 'emms-metaplaylist-mode-set-active
    "C" 'emms-metaplaylist-mode-new-buffer
    "." 'emms-metaplaylist-mode-center-current
    "D" 'emms-metaplaylist-mode-kill-buffer)
  (evil-collection-bind 'emms-metaplaylist-mode-map
                        'action 'emms-metaplaylist-mode-goto-current
                        'quit 'kill-current-buffer
                        'refresh 'emms-metaplaylist-mode-update)

  (evil-set-initial-state 'emms-stream-mode 'normal)
  (evil-collection-define-key 'normal 'emms-stream-mode-map
    "j" 'emms-stream-next-line
    "k" 'emms-stream-previous-line
    "y" 'emms-stream-yank-bookmark
    "d" 'emms-stream-kill-bookmark
    "c" 'emms-stream-edit-bookmark
    "r" 'emms-stream-edit-bookmark
    "i" 'emms-stream-info-bookmark
    "s" 'emms-stream-save-bookmarks-file
    "x" 'emms-stream-toggle-default-action)
  (evil-collection-bind 'emms-stream-mode-map
                        'action 'emms-stream-play
                        'quit 'emms-stream-quit)

  (unless evil-collection-always-run-setup-hook-after-load
    (run-hook-with-args
     'evil-collection-setup-hook 'emms
     '(emms-browser-search-mode-map
       emms-metaplaylist-mode-map
       emms-stream-mode-map))))

(provide 'evil-collection-emms)
;;; evil-collection-emms.el ends here
