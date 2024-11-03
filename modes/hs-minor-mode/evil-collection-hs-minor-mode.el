;;; evil-collection-hs-minor-mode --- Bindings for `hs-minor-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Julian Hoch

;; Author: Julian Hoch <julianhoch@web.de>
;; Maintainer: Julian Hoch <julianhoch@web.de>
;; Created: 2024-11-03
;; Keywords: evil, emacs, tools
;; URL: https://github.com/emacs-evil/evil-collection

;; This file is not part of GNU Emacs.

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
;;; Bindings for `hs-minor-mode'.

;;; Code:
(require 'evil-collection)
(require 'hs-minor-mode nil t)

(declare-function hs-hide-level "hideshow")

;;;###autoload
(defun evil-collection-hs-minor-mode-setup ()
  "Set up `evil' bindings for `hs-minor-mode'."
  (evil-collection-define-key 'normal 'hs-minor-mode-map
    "zL" 'hs-hide-level))

(provide 'evil-collection-hs-minor-mode)
;;; evil-collection-hs-minor-mode.el ends here
