;;; evil-collection-wgrep.el --- Bindings for `wgrep' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, tools

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
;; Bindings for `wgrep'.

;;; Code:
(require 'evil)
(require 'wgrep nil t)

(defvar wgrep-mode-map)

(defconst evil-collection-wgrep-maps '(wgrep-mode-map))

(evil-define-operator evil-collection-wgrep-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((eq type 'line)
    (if (and (= end (point-max))
             (or (= beg end)
                 (/= (char-before end) ?\n))
             (/= beg (point-min))
             (=  (char-before beg) ?\n))
        (delete-region (1- beg) end)
      (dotimes (_ (count-lines beg end))
        (wgrep-mark-deletion)
        (evil-next-line))))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

(defun evil-collection-wgrep-setup ()
  "Set up `evil' bindings for `wgrep'."
  (evil-define-key nil wgrep-mode-map
    [remap evil-write] 'wgrep-finish-edit)

  (evil-define-key 'normal wgrep-mode-map
    "d" 'evil-collection-wgrep-delete
    "ZQ" 'wgrep-abort-changes
    "ZZ" 'wgrep-finish-edit
    (kbd "<escape>") 'wgrep-exit))

(provide 'evil-collection-wgrep)
;;; evil-collection-wgrep.el ends here
