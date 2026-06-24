;;; evil-collection-dape.el --- Bindings for `dape' -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Pettersson

;; Author: Daniel Pettersson <daniel@dpettersson.net>
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools, dape

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
;; Bindings for `dape'.

;;; Code:
(require 'evil-collection)
(require 'dape nil t)

(defvar dape-info-scope-mode-map)
(defvar dape-info-watch-mode-map)
(defvar dape-info-stack-mode-map)
(defvar dape-info-sources-mode-map)
(defvar dape-info-modules-mode-map)
(defvar dape-info-breakpoints-mode-map)
(defvar dape-info-threads-mode-map)
(defvar dape-info-watch-edit-mode-map)
(defvar dape-memory-mode-map)

(declare-function dape-info-buffer-tab "dape")

(defconst evil-collection-dape--info-mode-maps '(dape-info-scope-mode-map
                                                 dape-info-watch-mode-map
                                                 dape-info-stack-mode-map
                                                 dape-info-sources-mode-map
                                                 dape-info-modules-mode-map
                                                 dape-info-breakpoints-mode-map
                                                 dape-info-threads-mode-map))

(defconst evil-collection-dape-maps `(dape-info-watch-edit-mode-map
                                      dape-memory-mode-map
                                      . ,evil-collection-dape--info-mode-maps))

(defun evil-collection-dape--info-buffer-tab-backward ()
  "Switch to the previous `dape' info tab."
  (interactive)
  (dape-info-buffer-tab t))

;;;###autoload
(defun evil-collection-dape-setup ()
  "Set up `evil' bindings for `dape'."

  (dolist (mode '(dape-info-scope-mode
                  dape-info-watch-mode
                  dape-info-stack-mode
                  dape-info-sources-mode
                  dape-info-modules-mode
                  dape-info-breakpoints-mode
                  dape-info-threads-mode))
    (evil-set-initial-state mode 'normal))

  (dolist (map-symbol evil-collection-dape--info-mode-maps)
    (evil-collection-define-key 'normal map-symbol
      (kbd "<tab>") 'dape-info-buffer-tab
      (kbd "<backtab>") 'evil-collection-dape--info-buffer-tab-backward))

  (evil-collection-bind 'dape-info-breakpoints-mode-map 'edit 'dape-info-breakpoint-log-edit)

  (evil-collection-define-key 'normal 'dape-info-breakpoints-mode-map
    [mouse-2] 'dape-info-breakpoint-dwim
    [follow-link] 'mouse-face
    "D" 'dape-info-breakpoint-disable
    "d" 'dape-info-breakpoint-delete)
  (evil-collection-bind 'dape-info-breakpoints-mode-map 'action 'dape-info-breakpoint-dwim)

  (evil-collection-define-key 'normal 'dape-info-threads-mode-map
    [mouse-2] 'dape-info-select-thread
    [follow-link] 'mouse-face)
  (evil-collection-bind 'dape-info-threads-mode-map 'action 'dape-info-select-thread)

  (evil-collection-define-key 'normal 'dape-info-stack-mode-map
    [mouse-2] 'dape-info-stack-select
    [follow-link] 'mouse-face)
  (evil-collection-bind 'dape-info-stack-mode-map 'action 'dape-info-stack-select)

  (evil-collection-define-key 'normal 'dape-info-sources-mode-map
    [mouse-2] 'dape-info-sources-goto
    [follow-link] 'mouse-face)
  (evil-collection-bind 'dape-info-sources-mode-map 'action 'dape-info-sources-goto)

  (evil-collection-define-key 'normal 'dape-info-modules-mode-map
    [mouse-2] 'dape-info-modules-goto
    [follow-link] 'mouse-face)
  (evil-collection-bind 'dape-info-modules-mode-map 'action 'dape-info-modules-goto)

  (evil-collection-define-key 'normal 'dape-info-scope-mode-map
    "e" 'dape-info-scope-toggle
    "W" 'dape-info-scope-watch-dwim
    "=" 'dape-info-variable-edit
    "B" 'dape-info-scope-data-breakpoint
    "m" 'dape-info-variable-memory)

  (evil-collection-define-key 'normal 'dape-info-watch-mode-map
    "e" 'dape-info-scope-toggle
    "W" 'dape-info-scope-watch-dwim
    "=" 'dape-info-variable-edit
    "B" 'dape-info-scope-data-breakpoint
    "m" 'dape-info-variable-memory
    "I" 'dape-info-watch-edit-mode)

  (evil-collection-define-key nil 'dape-info-watch-edit-mode-map
    [remap evil-write] 'dape-info-watch-finish-edit)
  (evil-collection-define-key 'normal 'dape-info-watch-edit-mode-map
    (kbd "<escape>") 'dape-info-watch-finish-edit)
  (evil-collection-bind 'dape-info-watch-edit-mode-map
                        'quit-save 'dape-info-watch-finish-edit
                        'quit-cancel 'dape-info-watch-abort-changes)

  (evil-collection-define-key 'normal 'dape-memory-mode-map
    [remap evil-write] 'save-buffer)
  (evil-collection-bind 'dape-memory-mode-map 'quit-save 'save-buffer)

  ;; Dape debug commands available in every info buffer (and memory).
  ;; In scope/watch, the original `b' (data-breakpoint) and watch's `i'
  ;; (watch-edit-mode) were relocated to `B' and `I' to free those slots
  ;; for `debug-breakpoint' and `debug-step-into'.
  (dolist (map-symbol '(dape-info-breakpoints-mode-map
                        dape-info-threads-mode-map
                        dape-info-stack-mode-map
                        dape-info-sources-mode-map
                        dape-info-modules-mode-map
                        dape-info-scope-mode-map
                        dape-info-watch-mode-map
                        dape-memory-mode-map))
    (evil-collection-bind map-symbol
                          'debug-continue 'dape-continue
                          'debug-step-over 'dape-next
                          'debug-step-into 'dape-step-in
                          'debug-step-out 'dape-step-out
                          'debug-breakpoint 'dape-breakpoint-toggle
                          'debug-restart 'dape-restart)))

(provide 'evil-collection-dape)
;;; evil-collection-dape.el ends here
