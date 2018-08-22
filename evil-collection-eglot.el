;;; evil-collection-eglot.el --- Bindings for `eglot'. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, eglot, tools

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
;;; Bindings for `eglot'.

;;; Code:
(require 'eglot nil t)
(require 'evil-collection)

(defconst evil-collection-eglot-maps '(eglot-mode-map))

(defun evil-collection-eglot-setup ()
  "Set up `evil' bindings for `eglot'."
  (evil-collection-define-key 'normal 'eglot-mode-map
    "gd" 'xref-find-definitions
    (kbd "C-t") 'xref-pop-marker-stack
    "K" 'eglot-help-at-point))

(provide 'evil-collection-eglot)
;;; evil-collection-eglot.el ends here
