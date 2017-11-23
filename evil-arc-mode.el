;;; evil-arc-mode.el --- Evil bindings for arc-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, arc-mode, archive, bindings, files

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
;;; Evil bindings for arc-mode.

;;; Code:
(require 'arc-mode)
(require 'evil)

(defun evil-arc-mode-setup ()
  "Set up `evil' bindings for `arc-mode'."
  (evil-set-initial-state 'arc-mode 'motion)
  (evil-set-initial-state 'archive-mode 'motion)
  (evil-define-key 'motion archive-mode-map
    "j" 'archive-next-line
    "k" 'archive-previous-line
    (kbd "C-j") 'archive-next-line
    (kbd "C-k") 'archive-previous-line
    "gj" 'archive-next-line
    "gk" 'archive-previous-line

    "gg" 'beginning-of-buffer
    "G" 'end-of-buffer

    ;; open
    (kbd "<return>") 'archive-extract
    (kbd "S-<return>") 'archive-extract-other-window
    (kbd "M-<return>") 'archive-view
    "gd" 'archive-extract
    "gD" 'archive-extract-other-window

    "a" 'archive-alternate-display
    "d" 'archive-flag-deleted
    "r" 'archive-rename-entry
    "x" 'archive-expunge
    "M" 'archive-chmod-entry
    "P" 'archive-chgrp-entry
    "C" 'archive-chown-entry

    ;; refresh
    "gr" 'revert-buffer

    ;; mark
    "m" 'archive-mark
    "u" 'archive-unflag
    "U" 'archive-unmark-all-files

    ;; quit
    "q" 'quit-window))

(provide 'evil-arc-mode)
;;; evil-arc-mode.el ends here
