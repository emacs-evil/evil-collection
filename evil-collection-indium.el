;;; evil-collection-indium.el --- Bindings for `indium'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, indium, javascript, tools

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
;;; Bindings for `indium'.

;;; Code:
(require 'evil)
(require 'indium nil t)

(defun evil-collection-indium-setup ()
  "Set up `evil' bindings for `indium'."
  (evil-define-key 'normal indium-interaction-mode-map
    "gr" 'indium-update-script-source
    "gz" 'indium-switch-to-repl-buffer)

  (evil-define-key 'normal indium-repl-mode-map
    (kbd "C-j") 'indium-repl-next-input
    (kbd "C-k") 'indium-repl-previous-input))

(provide 'evil-collection-indium)
;;; evil-collection-indium.el ends here
