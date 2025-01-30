;;; evil-collection-minesweeper.el --- Bindings for `minesweeper' -*- lexical-binding: t -*-

;; Copyright (C) 2025 StrawberryTea

;; Author: StrawberryTea
;; Maintainer: StrawberryTea <look@strawberrytea.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, games

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
;; Bindings for `minesweeper'.

;;; Code:

(require 'evil-collection)
(require 'minesweeper nil t)

(defconst evil-collection-minesweeper-maps '(minesweeper-mode-map))

;;;###autoload
(defun evil-collection-minesweeper-setup ()
  "Set up `evil' bindings for `minesweeper'."
  (evil-set-initial-state 'minesweeper-mode 'normal)
  (evil-collection-define-key 'normal 'minesweeper-mode-map
    "RET" 'minesweeper-choose
    "m" 'minesweeper-toggle-mark
    "c" 'minesweeper-choose-around
    "s" 'minesweeper-toggle-show-neighbors
    "j" 'minesweeper-next-line
    "h" 'minesweeper-backward-char
    "l" 'minesweeper-forward-char
    "$" 'minesweeper-move-end-of-field
    "e" 'minesweeper-move-end-of-field))

(provide 'evil-collection-minesweeper)

;;; evil-collection-minesweeper.el ends here
