;;; evil-collection-mpdired.el --- Evil bindings for mpdired -*- lexical-binding: t -*-

;; Copyright (C) 2024 Ruoyu Feng

;; Author: Ruoyu Feng <mail@vonfry.name>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, tools, mpd

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for mpdired.

;;; Code:

(require 'mpdired nil t)
(require 'evil-collection)

(defvar mpdired-core-map)

(defconst evil-collection-mpdired-maps '(mpdired-mode-map))

(defconst evil-collection-mpdired-modes '(mpdired-mode-map))

;;;###autoload
(defun evil-collection-mpdired-setup ()
  "Set up `evil' bindings for `mpdired'."

  (evil-collection-set-readonly-bindings 'mpdired-mode-map)
  (dolist (mode evil-collection-mpdired-modes)
    (evil-set-initial-state mode 'normal))

  (evil-collection-define-key 'normal 'mpdired-mode-map
    "j"         'mpdired-next-line
    "k"         'mpdired-previous-line
    (kbd "RET") 'mpdired-enter
    "^"         'mpdired-goto-parent
    "."         'mpdired-goto-current-song
    "o"         'mpdired-toggle-view
    "gr"        'mpdired-update
    "gR"        'mpdired-db-update
    "J"         'mpdired-next-internal
    "K"         'mpdired-previous-internal
    "a"         'mpdired-add
    "d"         'mpdired-flag-at-point
    "x"         'mpdired-execute
    "D"         'mpdired-delete
    "p"         'mpdired-pause-internal
    (kbd "SPC") 'mpdired-pause-internal
    "gv"        'mpdired-set-volume-internal
    "ss"        'mpdired-stop
    "sR"        'mpdired-toggle-repeat
    "sr"        'mpdired-toggle-random
    "sS"        'mpdired-toggle-single
    "sc"        'mpdired-toggle-consume
    "gc"        'mpdired-playlist-create
    "ga"        'mpdired-playlist-append
    "m"         'mpdired-mark-at-point
    "M"         'mpdired-change-marks
    "u"         'mpdired-unmark-at-point
    "U"         'mpdired-unmark-all-marks
    "t"         'mpdired-toggle-marks
    "T"         'mpdired-previous-unmark
    "%d"        'mpdired-flag-files-regexp
    "%m"        'mpdired-mark-files-regexp
    "%i"        'mpdired-put-order-at-point
    "%r"        'mpdired-reset-order-index))

(provide 'evil-collection-mpdired)
;;; evil-collection-mpdired.el ends here
