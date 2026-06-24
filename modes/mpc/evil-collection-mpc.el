;;; evil-collection-mpc.el --- Bindings for `mpc-mode' -*- lexical-binding: t -*-
;; Copyright (C) 2021, 2022 pspiagicw

;; Author: pspiagicw <pspiagicw@gmail.com>
;; Maintainer: pspiagicw <pspiagicw@gmail.com>
;; pspiagicw <pspiagicw@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, mpc, processes , mpd

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
;;; Bindings for `mpc-mode'.

;;; Code:
(require 'mpc)
(require 'evil-collection)

(defconst evil-collection-mpc-mode-maps '(mpc-mode-map))

(defun evil-collection-mpc-move-down ()
  "Move the cursor down along with selecting the element."
  (interactive)
  (evil-next-visual-line)
  (mpc-select))

(defun evil-collection-mpc-move-up ()
  "Move the cursor up along with selecting the element."
  (interactive)
  (evil-previous-visual-line)
  (mpc-select))

;;;###autoload
(defun evil-collection-mpc-setup ()
  "Setup up `evil' bindings for `mpc-mode'."
  (evil-collection-bind 'mpc-mode-map
                        'next-item 'evil-collection-mpc-move-down
                        'prev-item 'evil-collection-mpc-move-up
                        'next-section 'evil-collection-mpc-move-down
                        'prev-section 'evil-collection-mpc-move-up)
  (evil-collection-define-key 'normal 'mpc-mode-map
    "T"                'mpc-stop
    "r"                'mpc-toggle-repeat
    "s"                'mpc-toggle-shuffle
    "c"                'mpc-toggle-consume
    "gp"               'mpc-playlist
    "a"                'mpc-playlist-add
    "J"                'mpc-next
    "K"                'mpc-prev
    "x"                'mpc-play-at-point
    "X"                'mpc-play
    "gd"               'mpc-describe-song
    "p"                'mpc-pause
    "P"                'mpc-resume
    "gs"               'mpc-seek-current
    "gb"               'mpc-tagbrowser
    "g/"               'mpc-songs-search
    "g_"               'mpc-songs-kill-search
    "gd"               'mpc-songs-jump-to
    "D"                'mpc-playlist-delete)
  ;; Bind action keys before restoring `go' to `mpc-goto-playing-song'.
  (evil-collection-bind 'mpc-mode-map
                        'action 'mpc-select
                        'action-other 'mpc-select-toggle
                        'action-stay 'mpc-select-extend
                        'toggle 'mpc-toggle-play)
  (evil-collection-define-key 'normal 'mpc-mode-map
    (kbd "C-<return>") 'mpc-select-extend
    "go" 'mpc-goto-playing-song)
  (evil-collection-bind 'mpc-mode-map
                        'quit 'mpc-quit
                        'refresh-all 'mpc-update))

(provide 'evil-collection-mpc)
;;; evil-collection-mpc.el ends here
