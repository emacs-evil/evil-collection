;;; evil-dired.el --- Evil bindings for Dired -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, dired, tools
;; HomePage: https://github.com/jojojames/evil-collection

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
;; This package provides a sane set of defaults for `dired-mode' when using
;; `evil-mode'.

;;; Code:
(require 'dired)
(require 'evil-collection-util)

(defun evil-dired-set-keys ()
  (evil-define-key 'normal dired-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "l" 'evil-forward-char
    "h" 'evil-backward-char
    ":" 'evil-ex
    [mouse-2] 'dired-mouse-find-file-other-window
    [follow-link] 'mouse-face
    ;; Commands to mark or flag certain categories of files
    "#" 'dired-flag-auto-save-files
    "." 'dired-clean-directory
    "~" 'dired-flag-backup-files
    ;; Upper case keys (except !) for operating on the marked files
    "A" 'dired-do-find-regexp
    "C" 'dired-do-copy
    "B" 'dired-do-byte-compile
    "D" 'dired-do-delete
    "G" 'dired-do-chgrp
    "H" 'dired-do-hardlink
    "L" 'dired-do-load
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "Q" 'dired-do-find-regexp-and-replace
    "R" 'dired-do-rename
    "S" 'dired-do-symlink
    "T" 'dired-do-touch
    "X" 'dired-do-shell-command
    "Z" 'dired-do-compress
    "c" 'dired-do-compress-to
    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command
    ;; Comparison commands
    "=" 'dired-diff
    ;; Tree Dired commands
    "\M-\C-?" 'dired-unmark-all-files
    "\M-\C-d" 'dired-tree-down
    "\M-\C-u" 'dired-tree-up
    "\M-\C-n" 'dired-next-subdir
    "\M-\C-p" 'dired-prev-subdir
    ;; move to marked files
    "\M-{" 'dired-prev-marked-file
    "\M-}" 'dired-next-marked-file
    ;; Make all regexp commands share a `%' prefix:
    ;; We used to get to the submap via a symbol dired-regexp-prefix,
    ;; but that seems to serve little purpose, and copy-keymap
    ;; does a better job without it.
    "%" nil
    "%u" 'dired-upcase
    "%l" 'dired-downcase
    "%d" 'dired-flag-files-regexp
    "%g" 'dired-mark-files-containing-regexp
    "%m" 'dired-mark-files-regexp
    "%r" 'dired-do-rename-regexp
    "%C" 'dired-do-copy-regexp
    "%H" 'dired-do-hardlink-regexp
    "%R" 'dired-do-rename-regexp
    "%S" 'dired-do-symlink-regexp
    "%&" 'dired-flag-garbage-files
    ;; Commands for marking and unmarking.
    "*" nil
    "**" 'dired-mark-executables
    "*/" 'dired-mark-directories
    "*@" 'dired-mark-symlinks
    "*%" 'dired-mark-files-regexp
    "*c" 'dired-change-marks
    "*s" 'dired-mark-subdir-files
    "*m" 'dired-mark
    "*u" 'dired-unmark
    "*?" 'dired-unmark-all-files
    "*!" 'dired-unmark-all-marks
    "U" 'dired-unmark-all-marks
    "*\177" 'dired-unmark-backward
    "*\C-n" 'dired-next-marked-file
    "*\C-p" 'dired-prev-marked-file
    "*t" 'dired-toggle-marks
    ;; Lower keys for commands not operating on all the marked files
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "e" 'dired-find-file
    "f" 'dired-find-file
    "\C-m" 'dired-find-file
    "g" nil
    "gr" 'revert-buffer
    "i" 'dired-maybe-insert-subdir
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "r" 'dired-do-redisplay
    "m" 'dired-mark
    "n" 'dired-next-line
    "o" 'dired-find-file-other-window
    "\C-o" 'dired-display-file
    "p" 'dired-previous-line
    "s" 'dired-sort-toggle-or-edit
    "t" 'dired-toggle-marks
    "u" 'dired-unmark
    "v" 'dired-view-file
    "w" 'dired-copy-filename-as-kill
    "W" 'browse-url-of-dired-file
    "x" 'dired-do-flagged-delete
    "y" 'dired-show-file-type
    "+" 'dired-create-directory
    ;; moving
    "<" 'dired-prev-dirline
    ">" 'dired-next-dirline
    "^" 'dired-up-directory
    " " 'dired-next-line
    [?\S-\ ] 'dired-previous-line
    [remap next-line] 'dired-next-line
    [remap previous-line] 'dired-previous-line
    ;; hiding
    "$" 'dired-hide-subdir
    "\M-$" 'dired-hide-all
    "(" 'dired-hide-details-mode
    ;; isearch
    (kbd "M-s a C-s")   'dired-do-isearch
    (kbd "M-s a M-C-s") 'dired-do-isearch-regexp
    (kbd "M-s f C-s")   'dired-isearch-filenames
    (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp
    ;; misc
    [remap read-only-mode] 'dired-toggle-read-only
    ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
    [remap toggle-read-only] 'dired-toggle-read-only
    "?" 'dired-summary
    "\177" 'dired-unmark-backward
    [remap undo] 'dired-undo
    [remap advertised-undo] 'dired-undo
    ;; thumbnail manipulation (image-dired)
    "\C-td" 'image-dired-display-thumbs
    "\C-tt" 'image-dired-tag-files
    "\C-tr" 'image-dired-delete-tag
    "\C-tj" 'image-dired-jump-thumbnail-buffer
    "\C-ti" 'image-dired-dired-display-image
    "\C-tx" 'image-dired-dired-display-external
    "\C-ta" 'image-dired-display-thumbs-append
    "\C-t." 'image-dired-display-thumb
    "\C-tc" 'image-dired-dired-comment-files
    "\C-tf" 'image-dired-mark-tagged-files
    "\C-t\C-t" 'image-dired-dired-toggle-marked-thumbs
    "\C-te" 'image-dired-dired-edit-comment-and-tags
    ;; encryption and decryption (epa-dired)
    ";d" 'epa-dired-do-decrypt
    ";v" 'epa-dired-do-verify
    ";s" 'epa-dired-do-sign
    ";e" 'epa-dired-do-encrypt))

(provide 'evil-dired)
;;; evil-dired.el ends here
