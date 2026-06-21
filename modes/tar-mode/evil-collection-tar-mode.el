;;; evil-collection-tar-mode.el --- Evil bindings for tar-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Michael Arndt

;; Author: 0x28
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
  (evil-collection-define-key 'normal 'tar-mode-map
    "j" 'tar-next-line
    "k" 'tar-previous-line

    "gg" 'beginning-of-buffer
    "G" 'end-of-buffer

    "d" 'tar-flag-deleted
    "r" 'tar-rename-entry
    "x" 'tar-expunge
    "M" 'tar-chmod-entry
    "P" 'tar-chgrp-entry
    "O" 'tar-chown-entry

    ;; create
    "I" 'tar-new-entry

    ;; write
    "C" 'tar-copy

    ;; mark
    "u" 'tar-unflag
    "U" 'tar-clear-modification-flags)
  (evil-collection-bind 'action       'tar-mode-map 'tar-extract)
  (evil-collection-bind 'action-other 'tar-mode-map 'tar-extract-other-window)
  (evil-collection-bind 'action-stay  'tar-mode-map 'tar-view)
  (evil-collection-bind 'next-item    'tar-mode-map 'tar-next-line)
  (evil-collection-bind 'prev-item    'tar-mode-map 'tar-previous-line)
  (evil-collection-bind 'next-section 'tar-mode-map 'tar-next-line)
  (evil-collection-bind 'prev-section 'tar-mode-map 'tar-previous-line)
  (evil-collection-bind 'quit    'tar-mode-map 'quit-window)
  (evil-collection-bind 'refresh 'tar-mode-map 'revert-buffer))

(provide 'evil-collection-tar-mode)
;;; evil-collection-tar-mode.el ends here
