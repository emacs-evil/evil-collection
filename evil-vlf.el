;;; evil-vlf.el --- Evil bindings for vlf -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, vlf, tools

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
;; Evil bindings for `vlf'.

;;; Code:
(require 'evil-collection-util)
(require 'vlf)

;;; Code:
(defun evil-vlf-setup ()
  (+evilify-map
   vlf-mode-map
   :mode vlf-mode
   :bindings
   "C-j" 'vlf-next-batch
   "C-k" 'vlf-prev-batch
   "f" 'evil-find-char
   "F" 'vlf-toggle-follow
   "gr" vlf-revert
   "e" vlf-ediff-buffers))

(provide 'evil-vlf)
;;; evil-vlf.el ends here
