;;; evil-arc-mode.el --- Evil bindings for arc-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, arc-mode, archive, bindings

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
  (evil-set-initial-state 'arc-mode 'motion)
  (evil-set-initial-state 'archive-mode 'motion)
  (evil-define-key 'motion archive-mode-map
    (kbd "q") 'quit-window
    (kbd "gg") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer
    (kbd "gr") 'revert-buffer
    (kbd "j") 'archive-next-line
    (kbd "a") 'archive-alternate-display
    (kbd "d") 'archive-flag-deleted
    (kbd "m") 'archive-mark
    (kbd "C-j") 'archive-next-line
    (kbd "gj") 'archive-next-line
    (kbd "o") 'archive-extract
    (kbd "O") 'archive-extract-other-window
    (kbd "k") 'archive-previous-line
    (kbd "C-k") 'archive-previous-line
    (kbd "gk") 'archive-previous-line
    (kbd "r") 'archive-rename-entry
    (kbd "u") 'archive-unflag
    (kbd "U") 'archive-unmark-all-files
    (kbd "RET") 'archive-view
    (kbd "x") 'archive-expunge
    (kbd "M") 'archive-chmod-entry
    (kbd "P") 'archive-chgrp-entry
    (kbd "C") 'archive-chown-entry))

(provide 'evil-arc-mode)
;;; evil-arc-mode.el ends here
