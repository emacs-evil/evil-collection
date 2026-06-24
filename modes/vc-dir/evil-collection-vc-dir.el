;;; evil-collection-vc-dir.el --- Evil bindings for Vc-Dir -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, vc-dir, tools

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
;; Evil bindings for Vc-Dir.

;;; Code:
(require 'vc-dir)
(require 'evil-collection)

(defconst evil-collection-vc-dir-maps '(vc-dir-mode-map))

;;;###autoload
(defun evil-collection-vc-dir-setup ()
  "Set up `evil' bindings for `vc-dir'."
  (evil-set-initial-state 'vc-dir-mode 'normal)
  (evil-collection-set-readonly-bindings 'vc-dir-mode-map)
  (evil-collection-define-key 'normal 'vc-dir-mode-map
    ;; VC commands
    "c" 'vc-next-action

    "d" 'vc-diff
    "D" 'vc-root-diff
    "\t" 'vc-diff
    [backtab] 'vc-root-diff

    "R" 'vc-register
    "s" 'vc-register ;; Like `magit-stage'?
    "gu" 'vc-update

    "F" 'vc-update ;; This is the same as `vc-update' bound to "gu".
    "p" 'vc-push
    "P" 'vc-push

    "Lf" 'vc-print-log ;; Log File.
    "Ll" 'vc-print-log
    "Lr" 'vc-print-root-log
    "LL" 'vc-print-root-log
    "Li" 'vc-log-incoming
    "Lo" 'vc-log-outgoing

    "x" 'vc-revert
    "b" 'vc-annotate ;; Like `magit-blame'

    "\C-c\C-c" 'vc-dir-kill-dir-status-process
    [down-mouse-3] 'vc-dir-menu
    [mouse-2] 'vc-dir-find-file-other-window
    [follow-link] 'mouse-face

    "(" 'vc-dir-hide-up-to-date
    "o" 'vc-dir-hide-up-to-date

    "X" 'vc-dir-kill-line
    "S" 'vc-dir-search
    "Q" 'vc-dir-query-replace-regexp
    (kbd "M-s a C-s")   'vc-dir-isearch
    (kbd "M-s a M-C-s") 'vc-dir-isearch-regexp
    "i" 'vc-dir-ignore

    ;; Branching
    "Bc" 'vc-create-tag
    "Bl" 'vc-print-branch-log
    "Bs" 'vc-retrieve-tag)

  (evil-collection-bind 'vc-dir-mode-map
                        'mark 'vc-dir-mark
                        'mark-all 'vc-dir-mark-all-files
                        'toggle 'vc-dir-toggle-mark
                        'unmark 'vc-dir-unmark
                        'unmark-all 'vc-dir-unmark-all-files
                        'next-item 'vc-dir-next-directory
                        'prev-item 'vc-dir-previous-directory
                        'next-section 'vc-dir-next-directory
                        'prev-section 'vc-dir-previous-directory
                        'action 'vc-dir-find-file
                        'action-other 'vc-dir-find-file-other-window
                        'action-stay 'vc-dir-display-file
                        'find-file 'vc-dir-find-file
                        'refresh 'revert-buffer))

(provide 'evil-collection-vc-dir)
;;; evil-collection-vc-dir.el ends here
