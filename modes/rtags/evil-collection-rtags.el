;;; evil-collection-rtags.el --- Evil bindings for `rtags' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, rtags, tools

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
;;; Evil bindings for `rtags'.

;;; Code:
(require 'evil-collection)
(require 'rtags nil t)

(defvar rtags-mode-map)
(defvar rtags-dependency-tree-mode-map)
(defvar rtags-references-tree-mode-map)
(defvar rtags-location-stack-visualize-mode-map)

(defconst evil-collection-rtags-maps '(rtags-mode-map
                                       rtags-dependency-tree-mode-map
                                       rtags-references-tree-mode-map
                                       rtags-location-stack-visualize-mode-map))

;;;###autoload
(defun evil-collection-rtags-setup ()
  "Set up `evil' bindings for `rtags'."
  (evil-set-initial-state 'rtags-mode 'normal)
  (evil-set-initial-state 'rtags-dependency-tree-mode 'normal)
  (evil-set-initial-state 'rtags-references-tree-mode 'normal)
  (evil-set-initial-state 'rtags-location-stack-visualize-mode 'normal)

  (evil-collection-define-key 'normal 'rtags-mode-map
    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window

    "c" 'rtags-select-caller
    "C" 'rtags-select-caller-other-window
    "x" 'rtags-select-and-remove-rtags-buffer)
  (evil-collection-bind 'action       'rtags-mode-map 'rtags-select)
  (evil-collection-bind 'action-other 'rtags-mode-map 'rtags-select-other-window)
  (evil-collection-bind 'action-stay  'rtags-mode-map 'rtags-show-in-other-window)
  (evil-collection-bind 'quit 'rtags-mode-map 'rtags-call-bury-or-delete)

  (evil-collection-define-key 'normal 'rtags-dependency-tree-mode-map
    (kbd "<tab>") 'rtags-dependency-tree-toggle-current-expanded
    "E" 'rtags-dependency-tree-expand-all
    "c" 'rtags-dependency-tree-collapse-all
    "-" 'rtags-dependency-tree-collapse-current
    "+" 'rtags-dependency-tree-expand-current
    "P" 'rtags-dependency-tree-find-path

    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window
    "s" 'rtags-show-in-other-window

    "x" 'rtags-select-and-remove-rtags-buffer)
  (evil-collection-bind 'action       'rtags-dependency-tree-mode-map 'rtags-select)
  (evil-collection-bind 'action-other 'rtags-dependency-tree-mode-map 'rtags-select-other-window)
  (evil-collection-bind 'action-stay  'rtags-dependency-tree-mode-map 'rtags-show-in-other-window)
  (evil-collection-bind 'next-item    'rtags-dependency-tree-mode-map 'rtags-dependency-tree-next-level)
  (evil-collection-bind 'prev-item    'rtags-dependency-tree-mode-map 'rtags-dependency-tree-previous-level)
  (evil-collection-bind 'next-section 'rtags-dependency-tree-mode-map 'rtags-dependency-tree-next-level)
  (evil-collection-bind 'prev-section 'rtags-dependency-tree-mode-map 'rtags-dependency-tree-previous-level)
  (evil-collection-bind 'quit 'rtags-dependency-tree-mode-map 'rtags-call-bury-or-delete)

  (evil-collection-bind 'find-file 'rtags-dependency-tree-mode-map 'rtags-dependency-tree-find-path)

  (evil-collection-define-key 'normal 'rtags-references-tree-mode-map
    (kbd "<tab>") 'rtags-references-tree-toggle-current-expanded

    "E" 'rtags-references-tree-expand-all
    "c" 'rtags-references-tree-collapse-all
    "-" 'rtags-references-tree-collapse-current
    "+" 'rtags-references-tree-expand-current

    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window
    "s" 'rtags-show-in-other-window

    "x" 'rtags-select-and-remove-rtags-buffer)
  (evil-collection-bind 'action       'rtags-references-tree-mode-map 'rtags-select)
  (evil-collection-bind 'action-other 'rtags-references-tree-mode-map 'rtags-select-other-window)
  (evil-collection-bind 'action-stay  'rtags-references-tree-mode-map 'rtags-show-in-other-window)
  (evil-collection-bind 'next-item    'rtags-references-tree-mode-map 'rtags-references-tree-next-level)
  (evil-collection-bind 'prev-item    'rtags-references-tree-mode-map 'rtags-references-tree-previous-level)
  (evil-collection-bind 'next-section 'rtags-references-tree-mode-map 'rtags-references-tree-next-level)
  (evil-collection-bind 'prev-section 'rtags-references-tree-mode-map 'rtags-references-tree-previous-level)
  (evil-collection-bind 'quit 'rtags-references-tree-mode-map 'rtags-call-bury-or-delete)

  (evil-collection-define-key 'normal 'rtags-location-stack-visualize-mode-map
    [mouse-1] 'rtags-select-other-window
    [mouse-2] 'rtags-select-other-window
    "s" 'rtags-show-in-other-window

    "x" 'rtags-select-and-remove-rtags-buffer)
  (evil-collection-bind 'action       'rtags-location-stack-visualize-mode-map 'rtags-select)
  (evil-collection-bind 'action-other 'rtags-location-stack-visualize-mode-map 'rtags-select-other-window)
  (evil-collection-bind 'action-stay  'rtags-location-stack-visualize-mode-map 'rtags-show-in-other-window)
  (evil-collection-bind 'quit 'rtags-location-stack-visualize-mode-map 'rtags-call-bury-or-delete))

(provide 'evil-collection-rtags)
;;; evil-collection-rtags.el ends here
