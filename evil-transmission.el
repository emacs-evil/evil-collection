;;; evil-transmission.el --- Evil bindings for transmission.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, transmission, tools

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
;; Evil bindings for `transmission'.

;;; Code:
(require 'evil)
(require 'transmission nil t)

(defun evil-transmission-setup ()
  "Set up `evil' bindings for `transmission'."
  (evil-set-initial-state 'transmission-mode 'motion)
  (evil-define-key 'motion transmission-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<delete>") 'scroll-down-command

    ;; sort
    "s" 'tabulated-list-sort

    (kbd "<return>") 'transmission-files
    "p" 'transmission-peers
    "i" 'transmission-info

    "a" 'transmission-add
    ;; "D" 'transmission-delete ; Useless with `transmission-remove'?
    "p" 'transmission-move
    "d" 'transmission-remove
    "x" 'transmission-toggle ; EMMS has "x" for pause.
    "t" 'transmission-trackers-add
    "c" 'transmission-verify ; "c" for "[c]heck".
    "D" 'transmission-set-download
    "U" 'transmission-set-upload
    "S" 'transmission-set-ratio ; "S" for "[S]eed"
    "P" 'transmission-set-bandwidth-priority

    ;; mark
    "m" 'transmission-toggle-mark
    "M" 'transmission-unmark-all
    "~" 'transmission-invert-marks

    ;; update
    "gr" 'revert-buffer

    ;; quit
    "q" 'transmission-quit
    "ZQ" 'evil-quit
    "ZZ" 'transmission-quit)

  (evil-set-initial-state 'transmission-files-mode 'motion)
  (evil-define-key 'motion transmission-files-mode-map
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<delete>") 'scroll-down-command

    ;; sort
    "s" 'tabulated-list-sort

    "p" 'transmission-peers
    "i" 'transmission-info

    "R" 'transmission-move
    "u" 'transmission-files-unwant
    "U" 'transmission-files-want
    "P" 'transmission-files-priority
    (kbd "<return>") 'transmission-find-file
    (kbd "S-<return>") 'transmission-display-file
    "o" 'transmission-find-file-other-window
    "O" 'transmission-view-file
    "!" 'transmission-files-command
    ;; "X" 'transmission-files-command
    "t" 'transmission-trackers-add
    "T" 'transmission-trackers-remove

    ;; goto URL
    "gx" 'transmission-browse-url-of-file ; See mu4e.

    ;; quit
    "q" 'transmission-quit
    "ZQ" 'evil-quit
    "ZZ" 'transmission-quit)

  (evil-set-initial-state 'transmission-info-mode 'motion)
  (evil-define-key 'motion transmission-info-mode-map
    "p" 'transmission-peers

    "t" 'transmission-trackers-add
    "T" 'transmission-trackers-remove
    "D" 'transmission-set-torrent-download
    "U" 'transmission-set-torrent-upload
    "S" 'transmission-set-torrent-ratio ; "S" for "[S]eed"
    "P" 'transmission-set-bandwidth-priority
    "gy" 'transmission-copy-magnet ; TODO: Use "ym"?
    "R" 'transmission-move

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window)

  (evil-set-initial-state 'transmission-peers-mode 'motion)
  (evil-define-key 'motion transmission-peers-mode-map
    ;; sort
    "s" 'tabulated-list-sort

    "i" 'transmission-info

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(provide 'evil-transmission)
;;; evil-transmission.el ends here
