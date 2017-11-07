;;; evil-prodigy.el --- Evil bindings for prodigy -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, prodigy, tools

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
;; Evil bindings for `prodigy'.

;;; Code:
(require 'evil-collection-util)
(require 'prodigy)

(defun evil-prodigy-setup ()
  (+evilify-map
   prodigy-mode-map
   :mode prodigy-mode
   :bindings
   "j" 'prodigy-next
   "n" 'prodigy-prev
   "gr" 'prodigy-restart
   "`" 'prodigy-display-process
   "o" 'prodigy-browse
   "f t" 'prodigy-add-tag-filter
   "f n" 'prodigy-add-name-filter
   "F" 'prodigy-clear-filters
   "J m" 'prodigy-jump-magit
   "J d" 'prodigy-jump-dired
   "\C-j" 'prodigy-next-with-status
   "\C-k" 'prodigy-prev-with-status
   "y" 'prodigy-copy-cmd))

(provide 'evil-prodigy)
;;; evil-prodigy.el ends here
