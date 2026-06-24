;;; evil-collection-profiler.el --- Evil bindings for profiler -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, profiler, tools

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
;; Evil bindings for `profiler'.

;;; Code:
(require 'evil-collection)
(require 'profiler)

(defconst evil-collection-profiler-maps '(profiler-report-mode-map))

(defun evil-collection-profiler-toggle-entry-all ()
  "Toggle the entry at point with all of its subentries expanded."
  (interactive)
  (profiler-report-toggle-entry t))

;;;###autoload
(defun evil-collection-profiler-setup ()
  "Set up `evil' bindings for `profiler'."
  (evil-set-initial-state 'profiler-report-mode 'normal)

  (evil-collection-define-key 'normal 'profiler-report-mode-map
    ;; motion
    (kbd "<delete>") 'scroll-down-command
    "j" 'profiler-report-next-entry
    "k" 'profiler-report-previous-entry

    ;; sort
    "o" 'profiler-report-ascending-sort
    "O" 'profiler-report-descending-sort

    "c" 'profiler-report-render-calltree
    "C" 'profiler-report-render-reversed-calltree
    "i" 'profiler-report-describe-entry
    "=" 'profiler-report-compare-profile)
  (evil-collection-bind 'profiler-report-mode-map
                        'scroll-down 'scroll-up-command
                        'scroll-up 'scroll-down-command
                        'action 'profiler-report-find-entry
                        'quit 'quit-window
                        'quit-save 'quit-window
                        'quit-cancel 'evil-quit
                        'refresh 'revert-buffer
                        'section-toggle 'profiler-report-toggle-entry
                        'section-toggle-all
                        'evil-collection-profiler-toggle-entry-all))

(provide 'evil-collection-profiler)
;;; evil-collection-profiler.el ends here
