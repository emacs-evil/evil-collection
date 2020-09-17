;;; evil-collection-newsticker.el --- Evil bindings for newsticker -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, newsticker, tools

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
;; Evil bindings for newsticker.

;;; Code:
(require 'evil-collection)
(require 'newsticker)

(defconst evil-collection-newsticker-maps '(newsticker-mode-map))

;;;###autoload
(defun evil-collection-newsticker-setup ()
  "Set up `evil' bindings for `newsticker'."
  (evil-set-initial-state 'newsticker-mode 'normal)
  (evil-collection-define-key 'normal 'newsticker-mode-map
      ;; move
      "k" 'newsticker-previous-item
      "j" 'newsticker-next-item
      "gk" 'newsticker-previous-feed
      "gj" 'newsticker-next-feed

      ;; mark
      "r" 'newsticker-mark-item-at-point-as-read
      "i" 'newsticker-mark-item-at-point-as-immortal

      ;; show/hide
      "o" 'newsticker-show-old-items
      "O" 'newsticker-hide-old-items

      ;; refresh
      "gr" 'newsticker-buffer-force-update
      "gR" 'newsticker-get-all-news

      ;; quit
      "q" 'newsticker-close-buffer))

(provide 'evil-collection-newsticker)
;;; evil-collection-newsticker.el ends here
