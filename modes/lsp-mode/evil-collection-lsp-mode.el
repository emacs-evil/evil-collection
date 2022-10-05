;;; evil-collection-lsp-mode.el --- Bindings for `lsp-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: jgart <jgart@dismail.de>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, lsp-mode, tools

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
;;; Bindings for `lsp-mode'.

;;; Code:
(require 'lsp-mode nil t)
(require 'evil-collection)

(defconst evil-collection-lsp-mode-maps '(lsp-mode-map))

;;;###autoload
(defun evil-collection-lsp-mode-setup ()
  "Set up `evil' bindings for `lsp-mode'."
  (evil-collection-define-key 'normal 'lsp-mode-map
    "gd" 'xref-find-definitions
    (kbd "C-t") 'xref-pop-marker-stack)

  (when evil-collection-want-find-usages-bindings
    (evil-collection-define-key 'normal 'lsp-mode-map
      "gr" 'xref-find-references)))

(provide 'evil-collection-lsp-mode)
;;; evil-collection-lsp-mode.el ends here
