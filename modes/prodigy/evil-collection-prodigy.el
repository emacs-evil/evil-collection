;;; evil-collection-prodigy.el --- Evil bindings for prodigy -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, prodigy, tools

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
;; Evil bindings for `prodigy'.

;;; Code:
(require 'evil-collection)
(require 'prodigy nil t)

(defconst evil-collection-prodigy-maps '(prodigy-mode-map
                                         prodigy-view-mode-map))

;;;###autoload
(defun evil-collection-prodigy-setup ()
  "Set up `evil' bindings for `prodigy'."
  (evil-collection-bind 'quit 'prodigy-mode-map 'quit-window)
  (evil-collection-define-key 'normal 'prodigy-mode-map
    "j" 'prodigy-next
    "k" 'prodigy-prev
    "gg" 'prodigy-first
    "G" 'prodigy-last

    ;; mark
    "*t" 'prodigy-mark-tag
    "*T" 'prodigy-unmark-tag

    "s" 'prodigy-start
    "S" 'prodigy-stop

    "`" 'prodigy-display-process
    "it" 'prodigy-add-tag-filter
    "in" 'prodigy-add-name-filter
    "I" 'prodigy-clear-filters
    "Jm" 'prodigy-jump-magit
    "Jd" 'prodigy-jump-file-manager

    (kbd "Y") 'prodigy-copy-cmd)
  (evil-collection-bind 'mark         'prodigy-mode-map 'prodigy-mark)
  (evil-collection-bind 'mark-all     'prodigy-mode-map 'prodigy-mark-all)
  (evil-collection-bind 'unmark       'prodigy-mode-map 'prodigy-unmark)
  (evil-collection-bind 'unmark-all   'prodigy-mode-map 'prodigy-unmark-all)
  (evil-collection-bind 'action       'prodigy-mode-map 'prodigy-browse)
  (evil-collection-bind 'next-item    'prodigy-mode-map 'prodigy-next-with-status)
  (evil-collection-bind 'prev-item    'prodigy-mode-map 'prodigy-prev-with-status)
  (evil-collection-bind 'next-section 'prodigy-mode-map 'prodigy-next-with-status)
  (evil-collection-bind 'prev-section 'prodigy-mode-map 'prodigy-prev-with-status)

  (evil-collection-define-key 'normal 'prodigy-view-mode-map
    "s" 'prodigy-start
    "S" 'prodigy-stop
    "r" 'prodigy-restart
    "c" 'prodigy-view-clear-buffer
    (kbd "C-l") 'prodigy-view-clear-buffer
    "x" 'prodigy-view-clear-buffer)

  (evil-collection-bind 'refresh 'prodigy-mode-map      'prodigy-restart)
  (evil-collection-bind 'refresh 'prodigy-view-mode-map 'prodigy-restart))

(provide 'evil-collection-prodigy)
;;; evil-collection-prodigy.el ends here
