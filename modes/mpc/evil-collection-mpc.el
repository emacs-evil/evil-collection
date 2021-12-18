;;; evil-collection-mpc.el --- Bindings for `mpc-mode' -*- lexical-binding: t -*-
;; Copyright (C) 2021 pspiagicw

;; Author: pspiagicw <pspiagicw@gmail.com>
;; Maintainer: pspiagicw <pspiagicw@gmail.com>
;; pspiagicw <pspiagicw@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, mpc, processes , mpd

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
;;; Bindings for `mpc-mode'.

;;; Code:
(require 'mpc)
(require 'evil-collection)

(defun evil-collection-move-mpc-down ()
  "Move the cursor down along with selecting the element"
  (evil-next-visual-line)
  (mpc-select))
(defun evil-collection-move-mpc-up ()
  "Move the cursor up along with selecting the element"
  (evil-previous-visual-line)
  (mpc-select))
(defconst evil-collection-mpc-mode-maps '(mpc-mode-map))

;;;###autoload
(defun evil-collection-mpc-mode-setup ()
  "Setup up 'evil' bindings for 'mpc-mode'"
  (evil-collection-define-key 'normal 'mpc-mode-map
    "j" 'evil-collection-move-mpc-down
    "k" 'evil-collection-move-mpc-up
    "t" 'mpc-toggle-play
    "r" 'mpc-toggle-repeat
    "s" 'mpc-toggle-shuffle
    "c" 'mpc-toggle-consume
    "a" 'mpc-playlist-add
    ">" 'mpc-next
    "<" 'mpc-prev
    "x" 'mpc-play-at-point
    "RET" 'mpc-select))

(provide 'evil-collection-mpc-mode)
;;; evil-collection-mpc.el ends here
