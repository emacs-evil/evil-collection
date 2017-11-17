;;; evil-ibuffer.el --- Evil bindings for IBuffer -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, ibuffer, tools

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
;; Evil bindings for IBuffer.

;;; Code:
(require 'evil-collection-util)
(require 'ibuffer)

(defun evil-ibuffer-setup ()
  (evil-collection-util-evilify-map
   ibuffer-mode-map
   :mode ibuffer-mode
   :bindings
   "gb" 'ibuffer-bury-buffer
   "gB" 'ibuffer-copy-buffername-as-kill
   "gw" 'ibuffer-copy-filename-as-kill
   "gW" 'ibuffer-do-view-and-eval
   "gr" 'ibuffer-update
   "gx" 'ibuffer-kill-line
   "\C-j" 'ibuffer-forward-filter-group
   "\C-k" 'ibuffer-backward-filter-group ; Originally `ibuffer-kill-line'.
   "gj" 'ibuffer-forward-filter-group
   "gk" 'ibuffer-backward-filter-group))

(provide 'evil-ibuffer)
;;; evil-ibuffer.el ends here
