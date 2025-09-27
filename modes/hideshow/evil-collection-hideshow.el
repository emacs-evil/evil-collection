;;; evil-collection-hideshow --- Bindings for `hideshow'  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Julian Hoch

;; Author: Julian Hoch <julianhoch@web.de>
;; Maintainer: Julian Hoch <julianhoch@web.de>
;; Created: 2025-09-27
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
;;; Bindings for `hideshow'.

;;; Code:
(require 'evil-collection)
(require 'hideshow nil t)

(declare-function hs-hide-level "hideshow")
(defvar hs-minor-mode-map)
(defconst evil-collection-hideshow-maps '(hs-minor-mode-map))

;;;###autoload
(defun evil-collection-hideshow-setup ()
  "Set up `evil' bindings for `hideshow'."
  (evil-set-initial-state 'hs-minor-mode 'normal)
  (evil-collection-define-key 'normal 'hs-minor-mode-map
    "zL" 'hs-hide-level))

(provide 'evil-collection-hideshow)
;;; evil-collection-hideshow.el ends here
