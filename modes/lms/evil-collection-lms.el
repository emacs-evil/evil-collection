;;; evil-collection-lms.el --- Evil bindings for lms.el -*- lexical-binding: t -*-
;; Copyright (C) 2021 Edgar Vincent

;; Author: Edgar Vincent <e-v@posteo.net>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, multimedia, music, logitechmediaserver, squeezebox

;; This file is not part of GNU Emacs.
;;
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
;; Evil bindings for lms.el.
;;
;;; Code:
(require 'lms nil t)
(require 'evil-collection)

(defvar lms-ui-playing-now-mode-map)
(defvar lms-ui-players-mode-map)
(defvar lms-ui-playlist-mode-map)
(defvar lms-ui-track-info-mode-map)
(defvar lms-ui-tracks-list-mode-map)

(defconst evil-collection-lms-maps '(lms-ui-players-mode-map
                                     lms-ui-playing-now-mode-map
                                     lms-ui-playlist-mode-map
                                     lms-ui-track-info-mode-map
                                     lms-ui-tracks-list-mode-map
                                     lms-ui-year-album-artist-list-mode-map))

(defconst evil-collection-lms-modes '(lms-players-mode
                                      lms-ui-playing-now-mode
                                      lms-ui-track-info-mode
                                      lms-ui-players-mode
                                      lms-ui-playlist-mode
                                      lms-ui-tracks-list-mode
                                      lms-ui-year-album-artist-list-mode))

(defun evil-collection-lms-ui-track-info-mode-quit ()
  "Exit lms-ui-track-info-mode."
  (interactive)
  (kill-buffer "*LMS: Track Information*")
  (lms-ui-playing-now-refresh))

(defun evil-collection-lms-ui-players-mode-quit ()
  "Exit lms-ui-players-mode."
  (interactive)
  (kill-buffer (format "*LMS: Players*"))
  (lms-ui-playing-now-refresh))

(defun evil-collection-lms-ui-playlist-mode-quit ()
  "Exit lms-ui-playlist-mode."
  (interactive)
  (kill-buffer (format "*LMS: Playlist [%d tracks]*" (length lms--ui-pl-tracks)))
  (lms-ui-playing-now-refresh))

(defun evil-collection-lms-ui-tracks-list-mode-quit ()
  "Exit lms-ui-tracks-list-mode."
  (interactive)
  (kill-buffer)
  (lms-ui-playing-now-refresh))

(defalias 'evil-collection-lms-ui-year-album-artist-list-mode-quit
  'evil-collection-lms-ui-tracks-list-mode-quit
  "Exit lms-ui-tracks-list-mode.")

(defun evil-collection-lms-goto-bottom ()
  "Move the point to the last penultimate line of the current buffer."
  (goto-char (max-char))
  (forward-line -1))

(defun evil-collection-lms-next-line ()
  "Move the point to the next line, except if it is at the end of the buffer."
  (interactive)
  (forward-line 1)
  (when (eobp) (forward-line -1)))


;;;###autoload
(defun evil-collection-lms-setup ()
  "Set up `evil' bindings for `lms'."

  (dolist (map evil-collection-lms-maps)
    (evil-collection-inhibit-insert-state map))

  (dolist (mode evil-collection-lms-modes)
    (evil-set-initial-state mode 'normal))

  (evil-collection-define-key 'normal 'lms-ui-playing-now-mode-map
    "t"              'lms-ui-playing-now-change-player-power-state
    (kbd "C-p")      'lms-ui-playing-now-players-list
    "R"              'lms-ui-playing-now-change-rating
    (kbd  "C-=")     'lms-ui-playing-now-set-volume
    "x"              'lms-ui-playing-now-play-pause
    (kbd "RET")      'lms-ui-playing-now-play-pause
    "p"              'lms-ui-playing-now-play
    "s"              'lms-ui-playing-now-stop
    "J"              'lms-ui-playing-now-next
    (kbd  "<right>") 'lms-ui-playing-now-next
    "K"              'lms-ui-playing-now-prev
    (kbd  "<left>")  'lms-ui-playing-now-prev
    "+"              'lms-ui-playing-now-volume-up
    "="              'lms-ui-playing-now-volume-up
    "-"              'lms-ui-playing-now-volume-down
    "m"              'lms-ui-playing-now-volume-mute
    "r"              'lms-ui-playing-now-cycle-repeat
    "S"              'lms-ui-playing-now-cycle-shuffle
    "u"              'lms-ui-playing-now-refresh
    "i"              'lms-ui-playing-now-show-track-info
    "P"              'lms-ui-playing-now-show-playlist
    "T"              'lms-ui-playing-now-album-tracks-list
    "A"              'lms-ui-playing-now-artist-albums-list
    "Y"              'lms-ui-playing-now-year-albums-list
    "M"              'lms-ui-playing-now-browse-music-library
    "g?"             'lms-ui-playing-now-help
    "gh"             'lms-ui-playing-now-help
    "q"              'lms-ui-playing-now-quit)

  (evil-collection-define-key 'normal 'lms-ui-track-info-mode-map
    "R"             'lms-ui-track-info-change-rating
    "K"             'lms-ui-track-info-prev
    (kbd "<left>")  'lms-ui-track-info-prev
    "J"             'lms-ui-track-info-next
    (kbd "<right>") 'lms-ui-track-info-next
    "g?"            'lms-ui-playing-now-help
    "gh"            'lms-ui-playing-now-help
    "q"             'evil-collection-lms-ui-track-info-mode-quit)

  (evil-collection-define-key 'normal 'lms-ui-players-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-bottom
    [remap end-of-defun] 'evil-collection-lms-goto-bottom
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-bottom
    [remap evil-next-line] 'evil-collection-lms-next-line
    (kbd "RET") 'lms-ui-players-select
    "x" 'lms-ui-players-playpause
    "t" 'lms-ui-players-toggle-power
    "g?" 'lms-ui-playing-now-help
    "gh" 'lms-ui-playing-now-help
    "q" 'evil-collection-lms-ui-players-mode-quit)

    (evil-collection-define-key 'normal 'lms-ui-playlist-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-bottom
    [remap end-of-defun] 'evil-collection-lms-goto-bottom
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-bottom
    [remap evil-next-line] 'evil-collection-lms-next-line
      (kbd "RET")      'lms-ui-playlist-play
      "i"              'lms-ui-playlist-track-info
      "C"              'lms-ui-playlist-jump-to-current
      "d"              'lms-ui-playlist-delete-track
      (kbd "<delete>") 'lms-ui-playlist-delete-track
      "cc"             'lms-ui-playlist-clear
      "ct"             'lms-ui-playlist-clear-until-track
      "cf"             'lms-ui-playlist-clear-from-track
      "u"              'lms-ui-playlist
      "A"              'lms-ui-playlist-artist-albums-list
      "T"              'lms-ui-playlist-album-tracks-list
      "Y"              'lms-ui-playlist-year-albums-list
      "g?"             'lms-ui-playing-now-help
      "gh"             'lms-ui-playing-now-help
      "q"              'evil-collection-lms-ui-playlist-mode-quit)

    (evil-collection-define-key 'normal 'lms-ui-tracks-list-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-bottom
    [remap end-of-defun] 'evil-collection-lms-goto-bottom
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-bottom
    [remap evil-next-line] 'evil-collection-lms-next-line
      "i"         'lms-ui-tl-track-info
      (kbd "RET") 'lms-ui-tl-track-info
      "p"         'lms-ui-tl-to-playlist
      "P"         'lms-ui-tl-all-to-playlist
      "Y"         'lms-ui-tl-by-year
      "A"         'lms-ui-tl-by-artist
      "g?"        'lms-ui-playing-now-help
      "gh"        'lms-ui-playing-now-help
      "q"         'evil-collection-lms-ui-tracks-list-mode-quit)


    (evil-collection-define-key 'normal 'lms-ui-year-album-artist-list-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-bottom
    [remap end-of-defun] 'evil-collection-lms-goto-bottom
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-bottom
    [remap evil-next-line] 'evil-collection-lms-next-line
      "Y" 'lms-ui-yaal-by-year
      "A" 'lms-ui-yaal-by-artist
      "a" 'lms-ui-yaal-by-album
      "p" 'lms-ui-yaal-to-playlist
      "g?" 'lms-ui-playing-now-help
      "gh" 'lms-ui-playing-now-help
      "q" 'evil-collection-lms-ui-year-album-artist-list-mode-quit))

(provide 'evil-collection-lms)
;;; evil-collection-lms.el ends here
