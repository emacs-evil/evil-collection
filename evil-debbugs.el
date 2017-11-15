;;; evil-debbugs.el --- Evil bindings for debbugs -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, debbugs, tools

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
;; Evil bindings for debbugs.

;;; Code:
(require 'debbugs nil t)
(require 'evil)

(defun evil-diff-mode-setup ()
  (evil-set-initial-state 'debbugs-gnu-mode 'motion)

  (evil-define-key 'motion debbugs-gnu-mode-map
    ;; motion
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button
    (kbd "SPC") 'scroll-up-command

    (kbd "<return>") 'debbugs-gnu-select-report
    "c" 'debbugs-gnu-send-control-message
    "d" 'debbugs-gnu-display-status

    ;; filtering
    (kbd "s") 'debbugs-gnu-narrow-to-status
    ;; "S" 'debbugs-gnu-widen ; Useless if we can just press "s RET" (empty filter).
    "S" 'debbugs-gnu-toggle-suppress
    "r" 'debbugs-gnu-show-all-blocking-reports

    ;; sorting
    "o" 'debbugs-gnu-toggle-sort
    "O" 'tabulated-list-sort

    ;; show
    "gB" 'debbugs-gnu-show-blocking-reports
    "gb" 'debbugs-gnu-show-blocked-by-reports

    ;; marking
    "m" 'debbugs-gnu-toggle-tag

    ;; update
    "gr" 'debbugs-gnu-rescan

    ;; quit
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'quit-window))

(provide 'evil-debbugs)
;;; evil-debbugs.el ends here
