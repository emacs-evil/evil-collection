;;; evil-collection-evil-ghostel.el --- Bindings for `evil-ghostel' -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, ghostel, tools

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
;; Bindings for `evil-ghostel'.
;;
;; `evil-ghostel' ships its own evil bindings in `evil-ghostel-mode-map'.
;; This module layers `evil-collection's customizable theme bindings on top.

;;; Code:
(require 'evil-collection)
(require 'evil-ghostel nil t)

(defvar evil-ghostel-mode-map)

(defconst evil-collection-evil-ghostel-maps '(evil-ghostel-mode-map))

;;;###autoload
(defun evil-collection-evil-ghostel-setup ()
  "Set up `evil' bindings for `evil-ghostel'."
  (evil-collection-bind 'evil-ghostel-mode-map
                              'term-toggle-escape
                              'evil-ghostel-toggle-send-escape))

(provide 'evil-collection-evil-ghostel)
;;; evil-collection-evil-ghostel.el ends here
