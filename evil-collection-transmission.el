;;; evil-collection-transmission.el --- Evil bindings for transmission.el -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
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
(require 'evil-collection)
(require 'transmission nil t)

(defvar transmission-mode-map)
(defvar transmission-files-mode-map)
(defvar transmission-info-mode-map)
(defvar transmission-peers-mode-map)

(defconst evil-collection-transmission-maps '(transmission-mode-map
                                              transmission-files-mode-map
                                              transmission-info-mode-map
                                              transmission-peers-mode-map))

(defun evil-collection-transmission-setup ()
  "Set up `evil' bindings for `transmission'."

  (evil-collection-inhibit-insert-state 'transmission-mode-map)
  (evil-set-initial-state 'transmission-mode 'normal)
  (evil-collection-define-key 'normal 'transmission-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<delete>") 'scroll-down-command

    ;; sort
    "o" 'tabulated-list-sort

    (kbd "<return>") 'transmission-files
    "p" 'transmission-peers
    "i" 'transmission-info

    "a" 'transmission-add
    ;; "D" 'transmission-delete ; Useless with `transmission-remove'?
    "r" 'transmission-move
    "D" 'transmission-remove
    "x" 'transmission-toggle ; EMMS has "x" for pause.
    "t" 'transmission-trackers-add
    "c" 'transmission-verify ; "c" for "[c]heck".
    "d" 'transmission-set-download
    "u" 'transmission-set-upload
    "S" 'transmission-set-ratio ; "S" for "[S]eed"
    "P" 'transmission-set-bandwidth-priority

    ;; mark
    "m" 'transmission-toggle-mark
    "U" 'transmission-unmark-all
    "~" 'transmission-invert-marks

    ;; refresh
    "gr" 'revert-buffer

    ;; quit
    "q" 'transmission-quit
    "ZQ" 'evil-quit
    "ZZ" 'transmission-quit)

  (evil-collection-inhibit-insert-state 'transmission-files-mode-map)
  (evil-set-initial-state 'transmission-files-mode 'normal)
  (evil-collection-define-key 'normal 'transmission-files-mode-map
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<delete>") 'scroll-down-command

    ;; sort
    "o" 'tabulated-list-sort

    "p" 'transmission-peers
    "i" 'transmission-info

    "r" 'transmission-move
    "P" 'transmission-files-priority

    ;; mark
    "u" 'transmission-files-unwant
    "m" 'transmission-files-want

    ;; open
    (kbd "<return>") 'transmission-find-file
    (kbd "S-<return>") 'transmission-find-file-other-window
    (kbd "M-<return>") 'transmission-display-file
    "go" 'transmission-find-file-other-window

    "v" 'transmission-view-file

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

  (evil-collection-inhibit-insert-state 'transmission-info-mode-map)
  (evil-set-initial-state 'transmission-info-mode 'normal)
  (evil-collection-define-key 'normal 'transmission-info-mode-map
    "p" 'transmission-peers

    "t" 'transmission-trackers-add
    "T" 'transmission-trackers-remove
    "D" 'transmission-set-torrent-download
    "U" 'transmission-set-torrent-upload
    "S" 'transmission-set-torrent-ratio ; "S" for "[S]eed"
    "P" 'transmission-set-bandwidth-priority
    "r" 'transmission-move

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window)

  (evil-collection-define-key 'operator 'transmission-info-mode-map
    ;; Like `eww'.
    "u" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (memq evil-this-operator
                                evil-collection-yank-operators)
                      (setq evil-inhibit-operator t)
                      #'transmission-copy-magnet))))


  (evil-collection-inhibit-insert-state 'transmission-peers-mode-map)
  (evil-set-initial-state 'transmission-peers-mode 'normal)
  (evil-collection-define-key 'normal 'transmission-peers-mode-map
    ;; sort
    "o" 'tabulated-list-sort

    "i" 'transmission-info

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(provide 'evil-collection-transmission)
;;; evil-collection-transmission.el ends here
