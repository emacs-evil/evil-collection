;;; evil-compile.el --- Evil integration for `compile'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: compile, evil
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
;; Evil integration for `compile'

;;; Code:
(require 'evil-collection-util)
(require 'compile)

(defun evil-compile-setup ()
  (+evilify-map
   compilation-mode-map
   :mode compilation-mode
   :bindings
   "gr" 'recompile
   "gj" 'compilation-next-error
   "gk" 'compilation-previous-error
   "\C-j" 'compilation-next-error
   "\C-k" 'compilation-previous-error
   "\M-j" 'compilation-next-file
   "\M-k" 'compilation-previous-file))

(provide 'evil-compile)
;;; evil-compile.el ends here
