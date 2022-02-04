;;; evil-collection-mpc.el --- Bindings for `simple-mpc-mode' -*- lexical-binding: t -*-
;; Copyright (C) 2022 Joseph Turner

;; Author: Joseph Turner <joseph@breatheoutbreathe.in>
;; Maintainer: Joseph Turner <joseph@breatheoutbreathe.in>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2"))
;; Keywords: evil, simple-mpc, processes, mpd

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
;;; Bindings for `simple-mpc-mode'.

;;; Code:
(require 'simple-mpc nil t)
(require 'evil-collection)

(defconst evil-collection-simple-mpc-mode-maps '(simple-mpc-mode-map))

;;;###autoload
(defun evil-collection-simple-mpc-setup ()
  "Setup up 'evil' bindings for 'simple-mpc-mode', 'simple-mpc-query-mode', and 'simple-mpc-current-playlist-mode'."
  (evil-collection-define-key 'normal 'simple-mpc-mode-map
    "p" 'simple-mpc-toggle
    ">" 'simple-mpc-next
    "<" 'simple-mpc-prev
    "f" 'simple-mpc-seek-forward
    "b" 'simple-mpc-seek-backward
    "s" 'simple-mpc-query
    "c" 'simple-mpc-view-current-playlist
    "l" 'simple-mpc-load-playlist
    "S" 'simple-mpc-shuffle-current-playlist
    "C" 'simple-mpc-clear-current-playlist
    "+" 'simple-mpc-increase-volume
    "-" 'simple-mpc-decrease-volume

    "q" 'simple-mpc-quit
    "ZQ" 'simple-mpc-quit
    "ZZ" 'simple-mpc-quit)

  (evil-collection-define-key 'normal 'simple-mpc-current-playlist-mode-map
    "x" 'simple-mpc-delete
    "d" 'simple-mpc-delete

    "q" 'simple-mpc-current-playlist-quit
    "ZQ" 'simple-mpc-current-playlist-quit
    "ZZ" 'simple-mpc-current-playlist-quit

    "<return>" 'simple-mpc-play-current-line
    (kbd "RET") 'simple-mpc-play-current-line)

  (evil-collection-define-key 'normal 'simple-mpc-query-mode-map
    "o" 'simple-mpc-query-sort

    "q" 'simple-mpc-query-quit
    "ZQ" 'simple-mpc-query-quit
    "ZZ" 'simple-mpc-query-quit

    "<S-return>" 'simple-mpc-query-add
    (kbd "M-RET") 'simple-mpc-query-add

    "<return>" 'simple-mpc-query-add-and-play
    (kbd "RET") 'simple-mpc-query-add-and-play))

(provide 'evil-collection-simple-mpc)
;;; evil-collection-simple-mpc.el ends here
