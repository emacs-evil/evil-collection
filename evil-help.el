;;; evil-help.el --- Evil integration for `help-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, help
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
;; Evil integration for `help-mode'.

;;; Code:
(require 'evil-collection-util)
(require 'help-mode)

(defun evil-help-set-keys ()
  (+evilify-map
   help-mode-map
   :mode help-mode
   :bindings
   "Y" 'evil-yank-line
   "y" 'evil-yank
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end
   ">" 'help-go-forward
   "<" 'help-go-back
   "gj" 'help-go-forward
   "gk" 'help-go-back
   "\C-j" 'help-go-forward
   "\C-k" 'help-go-back))

(provide 'evil-help)
;;; evil-help.el ends here
