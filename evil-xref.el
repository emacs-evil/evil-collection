;;; evil-xref.el --- Evil integration for xref. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, xref
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
;; Evil integration for `xref'.

;;; Code:

(require 'evil-collection-util)
(require 'xref)

(defun evil-xref-setup ()
  (+evilify-map
   xref--xref-buffer-mode-map
   :mode xref--xref-buffer-mode
   :bindings
   "\C-j" #'xref-next-line
   "\C-k" #'xref-prev-line
   "gj" #'xref-next-line
   "gk" #'xref-prev-line))

(provide 'evil-xref)
;;; evil-xref.el ends here
