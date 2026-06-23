;;; evil-collection-tar-mode.el --- Evil bindings for tar-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Michael Arndt

;; Author: 0x28
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, tar-mode, archive, bindings, files

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
;;; Evil bindings for `tar-mode'.

;;; Code:
(require 'tar-mode)
(require 'evil-collection)

(defconst evil-collection-tar-mode-maps '(tar-mode-map))

;;;###autoload
(defun evil-collection-tar-mode-setup ()
  "Set up `evil' bindings for `tar-mode'."
  (evil-set-initial-state 'tar-mode 'normal)
  (evil-collection-bind 'tar-mode-map 'rename 'tar-rename-entry)
  (evil-collection-define-key 'normal 'tar-mode-map
    "j" 'tar-next-line
    "k" 'tar-previous-line

    "gg" 'beginning-of-buffer
    "G" 'end-of-buffer

    "M" 'tar-chmod-entry
    "P" 'tar-chgrp-entry
    "O" 'tar-chown-entry

    ;; create
    "I" 'tar-new-entry

    ;; write
    "C" 'tar-copy)
  (evil-collection-bind 'tar-mode-map
                        'mark-delete 'tar-flag-deleted
                        'execute-marks 'tar-expunge
                        'unmark 'tar-unflag
                        'unmark-all 'tar-clear-modification-flags
                        'action 'tar-extract
                        'action-other 'tar-extract-other-window
                        'action-stay 'tar-view
                        'next-item 'tar-next-line
                        'prev-item 'tar-previous-line
                        'next-section 'tar-next-line
                        'prev-section 'tar-previous-line
                        'quit 'quit-window
                        'refresh 'revert-buffer))

(provide 'evil-collection-tar-mode)
;;; evil-collection-tar-mode.el ends here
