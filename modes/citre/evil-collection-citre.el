;;; evil-collection-citre.el --- Bindings for `citre' -*- lexical-binding: t -*-

;; Copyright (C) 2024 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, citre, tools

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
;;; Bindings for `citre'

;;; Code:
(require 'evil-collection)

(defun evil-collection-citre-set-bindings ()
  "Set jump point for citre commands."
  (dolist (cmd '(citre-jump
                 citre-query-jump
                 citre-jump-to-reference
                 citre-query-jump-to-reference))
    (evil-declare-not-repeat cmd)
    (evil-set-command-property cmd :jump t)))

;;;###autoload
(defun evil-collection-citre-setup ()
  "Set up `evil' bindings for `citre'."
  (evil-collection-citre-set-bindings))

(provide 'evil-collection-citre)
;;; evil-collection-citre.el ends here
