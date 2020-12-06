;;; evil-collection-consult.el --- Evil bindings for consult -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, consult, tools

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
;; Evil bindings for `consult'.

;;; Code:
(require 'evil-collection)
(require 'consult nil t)

(defun evil-collection-consult-set-bindings ()
  "Set the bindings."
  (evil-set-command-property 'consult-outline :jump t)
  (evil-set-command-property 'consult-mark :jump t)
  (evil-set-command-property 'consult-line :jump t)
  (evil-set-command-property 'consult-line-symbol-at-point :jump t)
  (evil-set-command-property 'consult-line-from-isearch :jump t))

;;;###autoload
(defun evil-collection-consult-setup ()
  "Set up `evil' bindings for `consult'."
  (evil-collection-consult-set-bindings))

(provide 'evil-collection-consult)
;;; evil-collection-consult.el ends here
