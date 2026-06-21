;;; evil-collection-bm.el --- Evil bindings for bm -*- lexical-binding: t -*-

;; Copyright (C) 2020 Zhiwei Chen

;; Author: Zhiwei Chen <condy0919@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, bm, tools

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
;; Evil bindings for bm.

;;; Code:
(require 'evil-collection)
(require 'bm nil t)

(defconst evil-collection-bm-maps '(bm-show-mode-map))

;;;###autoload
(defun evil-collection-bm-setup ()
  "Set up `evil' bindings for `bm'."
  (evil-set-initial-state 'bm-show-mode 'normal)

  (evil-collection-define-key 'normal 'bm-show-mode-map
    "j" 'bm-show-next
    "k" 'bm-show-prev)
  (evil-collection-bind 'action        'bm-show-mode-map 'bm-show-goto-bookmark)
  (evil-collection-bind 'action-other  'bm-show-mode-map 'bm-show-bookmark)
  (evil-collection-bind 'quit          'bm-show-mode-map 'bm-show-quit-window)
  (evil-collection-bind 'describe-mode 'bm-show-mode-map 'describe-mode)
  (evil-collection-bind 'refresh       'bm-show-mode-map 'revert-buffer))


(provide 'evil-collection-bm)
;;; evil-collection-bm.el ends here
