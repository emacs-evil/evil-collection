;;; evil-collection-fzfa.el --- Evil bindings for fzfa -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, fzfa, tools

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
;; Evil bindings for `fzfa'.
;;
;; The fzfa commands are autoloaded so no (require 'fzfa nil t) is needed.

;;; Code:
(require 'evil-collection)

(defvar evil-collection-fzfa-jump-commands
  '(;; Buffer-local jumps.
    fzfa-swiper
    fzfa-imenu
    fzfa-imenu-all-but-current
    fzfa-mark
    fzfa-outline
    fzfa-evil-marks
    ;; Cross-buffer / file jumps.
    fzfa-swiper-all
    fzfa-imenu-all
    fzfa-global-mark
    fzfa-recent-file
    fzfa-buffer
    fzfa-bookmark
    fzfa-tramp
    fzfa-evil-jumps
    fzfa-find
    fzfa-fd
    fzfa-locate
    fzfa-grep
    fzfa-grep-current-file
    fzfa-ugrep
    fzfa-rg
    fzfa-rg-files
    fzfa-ag
    fzfa-ag-files
    fzfa-git-grep
    fzfa-git-ls-files
    fzfa-git-modified-locally
    fzfa-git-added-files
    fzfa-git-staged-for-commit
    fzfa-git-modified-in-head
    fzfa-git-log-grep
    fzfa-hg-files
    fzfa-hg-modified-locally
    fzfa-hg-added-files
    fzfa-hg-modified-in-head
    fzfa-project-find-file
    fzfa-project-find-dir
    fzfa-project-buffer
    fzfa-project-recentf
    fzfa-project-switch-project
    fzfa-compile-error
    fzfa-flymake
    fzfa-flymake-project
    fzfa-org-heading
    fzfa-org-heading-all
    fzfa-org-agenda
    fzfa-org-todo
    fzfa-org-tags-view
    fzfa-info
    fzfa-info-emacs
    fzfa-info-elisp
    fzfa-info-org
    fzfa-info-cl
    fzfa-info-eieio
    fzfa-info-magit
    fzfa-info-at-point
    fzfa-hungry-swiper
    fzfa-hungry-find
    fzfa-find-any
    fzfa-find-some
    fzfa-evil-any)
  "List of `fzfa' commands that should be treated as evil jumps.")

(defun evil-collection-fzfa-set-bindings ()
  "Set the bindings."
  (dolist (cmd evil-collection-fzfa-jump-commands)
    (evil-declare-not-repeat cmd)
    (evil-set-command-property cmd :jump t)))

;;;###autoload
(defun evil-collection-fzfa-setup ()
  "Set up `evil' bindings for `fzfa'."
  (evil-collection-fzfa-set-bindings))

(provide 'evil-collection-fzfa)
;;; evil-collection-fzfa.el ends here
