;;; evil-ag.el --- Evil Bindings for Ag -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, ag, tools
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
;; This package provides a sane set of defaults for `ag-mode' when using
;; `evil-mode'.

;;; Code:
(require 'ag)
(require 'evil)

(defun evil-ag-set-keys ()
  (evil-add-hjkl-bindings ag-mode-map 'normal
    "gg" #'evil-goto-first-line
    "gr" #'recompile
    "gj" #'compilation-next-error
    "gk" #'compilation-previous-error
    "\C-j" #'compilation-next-error
    "\C-k" #'compilation-previous-error
    "0" #'evil-digit-argument-or-evil-beginning-of-line
    "n" #'evil-search-next
    "N" #'evil-search-previous)

  (setq evil-motion-state-modes
        (delete 'ag-mode evil-motion-state-modes)))

(provide 'evil-ag)
;;; evil-ag.el ends here
