;;; evil-ag.el --- Evil Bindings for Ag -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, ag, tools

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
;; Evil bindings for `ag-mode' from the `wgrep-ag' package.

;;; Code:
(require 'ag nil t)
(require 'evil)

(defun evil-ag-setup ()
  "Set up `evil' bindings for `ag'."
  (evil-add-hjkl-bindings ag-mode-map 'normal
    "gg" 'evil-goto-first-line
    "gr" 'recompile
    "gj" 'compilation-next-error
    "gk" 'compilation-previous-error
    (kbd "C-j") 'compilation-next-error
    (kbd "C-k") 'compilation-previous-error
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    "n" 'evil-search-next
    "N" 'evil-search-previous)

  ;; `ag' is best set in 'normal state because its buffer can be edited.
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  (evil-set-initial-state 'ag-mode 'normal))

(provide 'evil-ag)
;;; evil-ag.el ends here
