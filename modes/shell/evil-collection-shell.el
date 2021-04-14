;;; evil-collection-shell.el --- Evil bindings for `shell' -*- lexical-binding: t -*-

;; Copyright (C) 2021 Kira Bruneau

;; Author: Kira Bruneau <kira.bruneau@pm.me>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, shell, tools

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
;; Evil bindings for `shell'.

;;; Code:
(require 'evil-collection)
(require 'shell)

;;;###autoload
(defun evil-collection-shell-setup ()
  "Set up `evil' bindings for `shell'."
  (evil-set-initial-state 'shell-mode 'normal))

(provide 'evil-collection-shell)
;;; evil-collection-shell.el ends here
