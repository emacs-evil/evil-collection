;;; evil-flycheck.el --- Evil integration for `flycheck'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, flycheck, emacs
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
;; Evil integration for `flycheck-mode'.

;;; Code:
(require 'evil-collection-util)
(require 'flycheck)

(defun evil-flycheck-set-keys ()
  (+evilify-map
   flycheck-error-list-mode-map
   :mode flycheck-error-list-mode
   :bindings
   "gr" #'flycheck-error-list-check-source
   "j" 'flycheck-error-list-next-error
   "k" 'flycheck-error-list-previous-error))

(provide 'evil-flycheck)
;;; evil-flycheck.el ends here
