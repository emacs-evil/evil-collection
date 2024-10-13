;;; evil-collection-with-editor.el --- Bindings for `with-editor' -*- lexical-binding: t -*-

;; Copyright (C) 2024 Tianshu Wang

;; Author: Tianshu Wang <dev@wang.tianshu.me>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools

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
;; Bindings for `with-editor'.

;;; Code:
(require 'evil-collection)
(require 'with-editor nil t)

(defvar with-editor-mode-map)

(defconst evil-collection-with-editor-maps '(with-editor-mode-map))

;;;###autoload
(defun evil-collection-with-editor-setup ()
  "Set up `evil' bindings for `with-editor'."
  (evil-collection-define-key 'normal 'with-editor-mode-map
    "ZQ" 'with-editor-cancel
    "ZZ" 'with-editor-finish))

(provide 'evil-collection-with-editor)
;;; evil-collection-with-editor.el ends here
