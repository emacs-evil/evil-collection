;;; evil-collection-xref.el --- Evil bindings for xref -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, xref, tools

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
;; Evil bindings for `xref'.

;;; Code:
(require 'evil-collection)
(require 'xref)

(defconst evil-collection-xref-maps '(xref--xref-buffer-mode-map))

(defun evil-collection-xref-setup ()
  "Set up `evil' bindings for `xref'."
  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map
    "q" 'quit-window
    "gj" 'xref-next-line
    "gk" 'xref-prev-line
    (kbd "C-j") 'xref-next-line
    (kbd "C-k") 'xref-prev-line
    "]" 'xref-next-line
    "[" 'xref-prev-line
    "r" 'xref-query-replace-in-results

    ;; open
    (kbd "<return>") 'xref-goto-xref
    (kbd "S-<return>") 'xref-show-location-at-point
    "o" 'xref-show-location-at-point
    "go" 'xref-show-location-at-point))

(provide 'evil-collection-xref)
;;; evil-collection-xref.el ends here
