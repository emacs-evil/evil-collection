;;; evil-collection-prodigy.el --- Evil bindings for prodigy -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
  (evil-collection-bind 'prodigy-mode-map 'quit 'quit-window)
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
  (evil-collection-bind 'prodigy-mode-map
                        'mark 'prodigy-mark
                        'mark-all 'prodigy-mark-all
                        'unmark 'prodigy-unmark
                        'unmark-all 'prodigy-unmark-all
                        'action 'prodigy-browse
                        'next-item 'prodigy-next-with-status
                        'prev-item 'prodigy-prev-with-status
                        'next-section 'prodigy-next-with-status
                        'prev-section 'prodigy-prev-with-status)

  (evil-collection-define-key 'normal 'prodigy-view-mode-map
    "s" 'prodigy-start
    "S" 'prodigy-stop
    "r" 'prodigy-restart
    "c" 'prodigy-view-clear-buffer
    (kbd "C-l") 'prodigy-view-clear-buffer
    "x" 'prodigy-view-clear-buffer)

  (evil-collection-bind 'prodigy-mode-map 'refresh      'prodigy-restart)
  (evil-collection-bind 'prodigy-view-mode-map 'refresh 'prodigy-restart))

(provide 'evil-collection-prodigy)
;;; evil-collection-prodigy.el ends here
