;;; evil-p4.el --- Evil bindings for P4 -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, p4, tools

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
;; Evil bindings for P4.

;;; Code:
(require 'evil-collection-util)
(require 'p4)

(defun evil-p4-setup ()
  (+evilify-map
   p4-basic-mode-map
   :mode p4-basic-mode
   :bindings
   [mouse-1] 'p4-buffer-mouse-clicked
   "\C-j" 'p4-forward-active-link
   "\C-k" 'p4-backward-active-link
   "\C-m" 'p4-buffer-commands
   "gr" 'revert-buffer
   "gj" 'p4-forward-active-link
   "gk" 'p4-backward-active-link
   "q" 'quit-window
   "k" 'p4-scroll-down-1-line
   "j" 'p4-scroll-up-1-line))

;;; Code:
(provide 'evil-p4)
;;; evil-p4.el ends here
