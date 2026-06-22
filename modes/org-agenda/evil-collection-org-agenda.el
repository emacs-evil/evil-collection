;;; evil-collection-org-agenda.el --- Evil bindings for org-agenda -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ashish Panigrahi

;; Author: Ashish Panigrahi <public@ashishpanigrahi.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, org, org-agenda, tools

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
;; Evil basic bindings for `org-agenda-mode'.

;;; Code:
(require 'evil-collection)
(require 'org-agenda nil t)

(defconst evil-collection-org-agenda-maps '(org-agenda-mode-map))

(declare-function org-agenda-next-line "org-agenda")
(declare-function org-agenda-previous-line "org-agenda")
(declare-function org-agenda-next-item "org-agenda")
(declare-function org-agenda-previous-item "org-agenda")
(declare-function org-agenda-forward-block "org-agenda")
(declare-function org-agenda-backward-block "org-agenda")
(declare-function org-agenda-goto "org-agenda")
(declare-function org-agenda-switch-to "org-agenda")
(declare-function org-agenda-show "org-agenda")
(declare-function org-agenda-quit "org-agenda")
(declare-function org-agenda-exit "org-agenda")
(declare-function org-agenda-redo "org-agenda")
(declare-function org-agenda-redo-all "org-agenda")
(declare-function org-agenda-later "org-agenda")
(declare-function org-agenda-earlier "org-agenda")
(declare-function org-agenda-goto-today "org-agenda")
(declare-function org-agenda-goto-date "org-agenda")
(declare-function org-agenda-day-view "org-agenda")
(declare-function org-agenda-week-view "org-agenda")
(declare-function org-agenda-month-view "org-agenda")
(declare-function org-agenda-year-view "org-agenda")
(declare-function org-agenda-todo "org-agenda")
(declare-function org-agenda-schedule "org-agenda")
(declare-function org-agenda-deadline "org-agenda")
(declare-function org-agenda-set-tags "org-agenda")
(declare-function org-agenda-priority "org-agenda")
(declare-function org-agenda-refile "org-agenda")
(declare-function org-agenda-archive "org-agenda")
(declare-function org-agenda-kill "org-agenda")
(declare-function org-agenda-add-note "org-agenda")
(declare-function org-agenda-open-link "org-agenda")
(declare-function org-agenda-filter-by-tag "org-agenda")
(declare-function org-agenda-filter-remove-all "org-agenda")
(declare-function org-agenda-clock-in "org-agenda")
(declare-function org-agenda-clock-out "org-agenda")
(declare-function org-agenda-clock-cancel "org-agenda")
(declare-function org-agenda-clock-goto "org-agenda")
(declare-function org-agenda-bulk-mark "org-agenda")
(declare-function org-agenda-bulk-unmark "org-agenda")
(declare-function org-agenda-bulk-action "org-agenda")
(declare-function org-save-all-org-buffers "org")

;;;###autoload
(defun evil-collection-org-agenda-setup ()
  "Set up `evil' bindings for `org-agenda'."
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-collection-set-readonly-bindings 'org-agenda-mode-map)

  (evil-collection-define-key 'normal 'org-agenda-mode-map
    ;; Motion
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line

    ;; Open / visit
    [tab] 'org-agenda-goto
    (kbd "TAB") 'org-agenda-goto
    (kbd "SPC") 'org-agenda-show

    ;; Quit / refresh
    "x" 'org-agenda-exit
    "S" 'org-save-all-org-buffers

    ;; Date navigation
    "L" 'org-agenda-later
    "H" 'org-agenda-earlier
    "." 'org-agenda-goto-today
    "vd" 'org-agenda-day-view
    "vw" 'org-agenda-week-view
    "vm" 'org-agenda-month-view
    "vy" 'org-agenda-year-view

    ;; Entry actions
    "t" 'org-agenda-todo
    "s" 'org-agenda-schedule
    "T" 'org-agenda-set-tags
    "p" 'org-agenda-priority
    "r" 'org-agenda-refile
    "A" 'org-agenda-archive
    "D" 'org-agenda-deadline
    "n" 'org-agenda-add-note

    ;; Filtering
    "f" 'org-agenda-filter-by-tag
    "F" 'org-agenda-filter-remove-all

    ;; Clocking
    "I" 'org-agenda-clock-in
    "O" 'org-agenda-clock-out
    "X" 'org-agenda-clock-cancel
    "gJ" 'org-agenda-clock-goto

    ;; Bulk actions
    "m" 'org-agenda-bulk-mark
    "u" 'org-agenda-bulk-unmark
    "B" 'org-agenda-bulk-action)
  (evil-collection-bind 'org-agenda-mode-map
                        'next-item 'org-agenda-next-item
                        'prev-item 'org-agenda-previous-item
                        'next-section 'org-agenda-forward-block
                        'prev-section 'org-agenda-backward-block
                        'next-section-2 'org-agenda-next-item
                        'prev-section-2 'org-agenda-previous-item
                        'quit 'org-agenda-quit
                        'quit-save 'org-agenda-quit
                        'quit-cancel 'org-agenda-exit
                        'delete 'org-agenda-kill
                        'refresh 'org-agenda-redo
                        'refresh-all 'org-agenda-redo-all
                        'action 'org-agenda-switch-to
                        'action-other 'org-agenda-open-link
                        'jump 'org-agenda-goto-date))

(provide 'evil-collection-org-agenda)
;;; evil-collection-org-agenda.el ends here
