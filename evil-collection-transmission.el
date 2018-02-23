;;; evil-collection-transmission.el --- Evil bindings for transmission.el -*- lexical-binding: t -*-

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

(defun evil-collection-transmission-setup ()
  "Set up `evil' bindings for `transmission'."

  (evil-collection-inhibit-insert-state transmission-mode-map)
  (evil-set-initial-state 'transmission-mode 'normal)
  (evil-define-key 'normal transmission-mode-map
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

    ;; refresh
    "gr" 'revert-buffer

    ;; quit
    "q" 'transmission-quit
    "ZQ" 'evil-quit
    "ZZ" 'transmission-quit)

  (evil-collection-inhibit-insert-state transmission-files-mode-map)
  (evil-set-initial-state 'transmission-files-mode 'normal)
  (evil-define-key 'normal transmission-files-mode-map
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<delete>") 'scroll-down-command

    ;; sort
    "o" 'tabulated-list-sort

    "p" 'transmission-peers
    "i" 'transmission-info

    "r" 'transmission-move
    "u" 'transmission-files-unwant
    "U" 'transmission-files-want
    "P" 'transmission-files-priority

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

  (evil-collection-inhibit-insert-state transmission-info-mode-map)
  (evil-set-initial-state 'transmission-info-mode 'normal)
  (evil-define-key 'normal transmission-info-mode-map
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

  (evil-define-key 'operator transmission-info-mode-map
    ;; Like `eww'.
    "u" '(menu-item
          ""
          nil
          :filter (lambda (&optional _)
                    (when (memq evil-this-operator
                                evil-collection-yank-operators)
                      #'transmission-copy-magnet))))


  (evil-collection-inhibit-insert-state transmission-peers-mode-map)
  (evil-set-initial-state 'transmission-peers-mode 'normal)
  (evil-define-key 'normal transmission-peers-mode-map
    ;; sort
    "o" 'tabulated-list-sort

    "i" 'transmission-info

    ;; quit
    "q" 'quit-window
    "ZQ" 'evil-quit
    "ZZ" 'quit-window))

(provide 'evil-collection-transmission)
;;; evil-collection-transmission.el ends here
