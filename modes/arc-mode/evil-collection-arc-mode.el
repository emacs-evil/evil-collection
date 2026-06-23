;;; evil-collection-arc-mode.el --- Evil bindings for arc-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'evil-collection)

(defconst evil-collection-arc-mode-maps '(archive-mode-map))

;;;###autoload
(defun evil-collection-arc-mode-setup ()
  "Set up `evil' bindings for `arc-mode'."
  (evil-set-initial-state 'arc-mode 'normal)
  (evil-set-initial-state 'archive-mode 'normal)
  (evil-collection-bind 'archive-mode-map 'rename 'archive-rename-entry)
  (evil-collection-define-key 'normal 'archive-mode-map
    "j" 'archive-next-line
    "k" 'archive-previous-line

    "gg" 'beginning-of-buffer
    "G" 'end-of-buffer

    "a" 'archive-alternate-display
    "C" 'archive-copy-file
    "M" 'archive-chmod-entry
    "O" 'archive-chown-entry
    "P" 'archive-chgrp-entry)
  (evil-collection-bind 'archive-mode-map
                        'mark 'archive-mark
                        'unmark 'archive-unflag
                        'unmark-all 'archive-unmark-all-files
                        'mark-delete 'archive-flag-deleted
                        'execute-marks 'archive-expunge
                        'action 'archive-extract
                        'action-other 'archive-extract-other-window
                        'action-stay 'archive-view
                        'next-item 'archive-next-line
                        'prev-item 'archive-previous-line
                        'next-section 'archive-next-line
                        'prev-section 'archive-previous-line
                        'quit 'quit-window
                        'refresh 'revert-buffer))

(provide 'evil-collection-arc-mode)
;;; evil-collection-arc-mode.el ends here
