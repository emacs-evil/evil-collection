;;; evil-collection-debbugs.el --- Evil bindings for debbugs -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'evil-collection)

(defconst evil-collection-debbugs-maps '(debbugs-gnu-mode-map))

;;;###autoload
(defun evil-collection-debbugs-setup ()
  "Set up `evil' bindings for `debbugs-gnu-mode'."
  (evil-set-initial-state 'debbugs-gnu-mode 'normal)

  (evil-collection-bind 'debbugs-gnu-mode-map
                        'next-button 'forward-button
                        'previous-button 'backward-button)
  (evil-collection-define-key 'normal 'debbugs-gnu-mode-map
    "c" 'debbugs-gnu-send-control-message
    "d" 'debbugs-gnu-display-status

    ;; filter
    "S" 'debbugs-gnu-search
    ;; "S" 'debbugs-gnu-widen ; Useless if we can just press "s RET" (empty filter).
    "x" 'debbugs-gnu-toggle-suppress
    "r" 'debbugs-gnu-show-all-blocking-reports

    ;; sort
    "o" 'debbugs-gnu-toggle-sort
    "O" 'tabulated-list-sort

    ;; show
    "gB" 'debbugs-gnu-show-blocking-reports
    "gb" 'debbugs-gnu-show-blocked-by-reports

    ;; mark
    "m" 'debbugs-gnu-toggle-tag

    "gt" 'debbugs-gnu-view-bug-triage
    "g#" 'debbugs-gnu-bugs)
  (evil-collection-bind 'debbugs-gnu-mode-map
                        'scroll-down 'scroll-up-command
                        'action 'debbugs-gnu-select-report
                        'quit 'quit-window
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window
                        'describe-mode 'debbugs-gnu-manual
                        'refresh 'debbugs-gnu-rescan
                        'search-or-filter 'debbugs-gnu-narrow-to-status))

(provide 'evil-collection-debbugs)
;;; evil-collection-debbugs.el ends here
