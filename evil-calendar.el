;;; evil-calendar.el --- Add Evil bindings to calendar

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Package-Requires: ((evil "1.2.3"))
;; Package-Version: 20170724.1223
;; Homepage: https://github.com/Ambrevar/evil-special-modes
;; Version: 0

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

;;; Code:

(require 'evil)
(require 'calendar)

;;;###autoload
(defun evil-calendar-set-keys ()
  (evil-define-key 'motion calendar-mode-map
    ;; motion
    "h" 'calendar-backward-day
    "j" 'calendar-forward-week
    "k" 'calendar-backward-week
    "l" 'calendar-forward-day
    "0" 'calendar-beginning-of-week
    "^" 'calendar-beginning-of-week
    "$" 'calendar-end-of-week
    "[" 'calendar-backward-year
    "]" 'calendar-forward-year
    (kbd "M-<") 'calendar-beginning-of-year
    (kbd "M->") 'calendar-end-of-year
    "(" 'calendar-beginning-of-month
    ")" 'calendar-end-of-month
    (kbd "SPC") 'scroll-other-window
    (kbd "S-SPC") 'scroll-other-window-down
    (kbd "<delete>") 'scroll-other-window-down
    "<" 'calendar-scroll-right
    ">" 'calendar-scroll-left
    (kbd "C-b") 'calendar-scroll-right-three-months
    (kbd "C-f") 'calendar-scroll-left-three-months
    "{" 'calendar-backward-month
    "}" 'calendar-forward-month
    (kbd "C-k") 'calendar-backward-month
    (kbd "C-j") 'calendar-forward-month

    ;; visual
    "v" 'calendar-set-mark

    ;; goto
    "." 'calendar-gototoday
    "gd" 'calendar-goto-date ; "gd" in evil-org-agenda, "gd" in Emacs.
    ;; "gd" 'calendar-other-month ; Not very useful if we have `calendar-goto-date'.


    ;; diary
    "D" 'diary-view-other-diary-entries
    "d" 'diary-view-entries
    "m" 'diary-mark-entries
    "s" 'diary-show-all-entries

    "u" 'calendar-unmark
    "x" 'calendar-mark-holidays

    ;; show
    "gm" 'calendar-lunar-phases ; "gm" in evil-org-agenda. TODO: Shadows calendar-mayan.
    "gs" 'calendar-sunrise-sunset ; "gs" in evil-org-agenda
    "gh" 'calendar-list-holidays ; "gh" in evil-org-agenda. TODO: Shadows calendar-hebrew.
    "gc" 'org-calendar-goto-agenda ; "gc" in evil-org-agenda. TODO: Shadows calendar-iso.
    (kbd "r") 'calendar-cursor-holidays

    ;; update
    "gr" 'calendar-redraw

    "?" 'calendar-goto-info-node ; Search is not very useful.
    (kbd "M-=") 'calendar-count-days-region

    ;; quit
    "q" 'calendar-exit
    "ZQ" 'evil-quit
    "ZZ" 'calendar-exit))

(provide 'evil-calendar)
;;; evil-calendar.el ends here
