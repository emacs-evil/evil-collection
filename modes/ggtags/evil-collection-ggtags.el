;;; evil-collection-ggtags.el --- Evil bindings for ggtags -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, ggtags, tools

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
;; Evil bindings for `ggtags-mode'.

;;; Code:
(require 'evil-collection)
(require 'ggtags nil t)

(defvar ggtags-global-mode-map)
(defvar ggtags-mode-map)
(defvar ggtags-view-search-history-mode-map)
(defvar ggtags-view-tag-history-mode-map)
(defvar ggtags-navigation-map)

(defconst evil-collection-ggtags-maps '(ggtags-mode-map
                                        ggtags-view-search-history-mode-map
                                        ggtags-view-tag-history-mode-map
                                        ggtags-navigation-map))

;;;###autoload
(defun evil-collection-ggtags-setup ()
  "Set up `evil' bindings for `ggtags'."
  (evil-set-initial-state 'ggtags-global-mode 'normal)
  (evil-set-initial-state 'ggtags-view-search-history-mode 'normal)
  (evil-set-initial-state 'ggtags-view-tag-history-mode 'normal)

  ;; `ggtags-navigation-mode' is global and will conflict with other bindings.
  ;; https://github.com/leoliu/ggtags/issues/124
  (when (boundp 'ggtags-enable-navigation-keys)
    (setq ggtags-enable-navigation-keys nil))

  (evil-collection-bind 'ggtags-mode-map     'find-usages 'ggtags-find-reference)
  (evil-collection-bind 'ggtags-mode-map 'find-definition 'ggtags-find-tag-dwim)
  (evil-collection-bind 'ggtags-mode-map  'pop-definition 'ggtags-prev-mark)
  (evil-collection-bind 'ggtags-mode-map       'find-file 'ggtags-find-file)

  (evil-collection-define-key 'normal 'ggtags-view-search-history-mode-map
    "x" 'ggtags-view-search-history-kill
    "r" 'ggtags-save-to-register
    "R" 'ggtags-view-search-history-action)
  (evil-collection-bind 'ggtags-view-search-history-mode-map    'next-item 'ggtags-view-search-history-next)
  (evil-collection-bind 'ggtags-view-search-history-mode-map    'prev-item 'ggtags-view-search-history-prev)
  (evil-collection-bind 'ggtags-view-search-history-mode-map 'next-section 'ggtags-view-search-history-next)
  (evil-collection-bind 'ggtags-view-search-history-mode-map 'prev-section 'ggtags-view-search-history-prev)
  (evil-collection-bind 'ggtags-view-search-history-mode-map    'quit 'ggtags-kill-window)
  (evil-collection-bind 'ggtags-view-search-history-mode-map 'refresh 'ggtags-view-search-history-update)

  (evil-collection-bind 'ggtags-view-tag-history-mode-map    'next-item 'next-error-no-select)
  (evil-collection-bind 'ggtags-view-tag-history-mode-map    'prev-item 'previous-error-no-select)
  (evil-collection-bind 'ggtags-view-tag-history-mode-map 'next-section 'next-error-no-select)
  (evil-collection-bind 'ggtags-view-tag-history-mode-map 'prev-section 'previous-error-no-select)
  (evil-collection-bind 'ggtags-view-tag-history-mode-map 'quit 'ggtags-kill-window)

  (evil-collection-define-key 'normal 'ggtags-navigation-map
    ;; search
    "s" 'ggtags-navigation-isearch-forward
    "S" 'ggtags-navigation-isearch-forward

    "go" 'ggtags-navigation-visible-mode ;; FIXME: This can be anything.
    )
  (evil-collection-bind 'ggtags-navigation-map       'action 'ggtags-navigation-mode-done)
  (evil-collection-bind 'ggtags-navigation-map    'next-item 'next-error)
  (evil-collection-bind 'ggtags-navigation-map    'prev-item 'previous-error)
  (evil-collection-bind 'ggtags-navigation-map 'next-section 'ggtags-navigation-next-file)
  (evil-collection-bind 'ggtags-navigation-map 'prev-section 'ggtags-navigation-previous-file)
  (evil-collection-bind 'ggtags-navigation-map 'next-section-2 'next-error)
  (evil-collection-bind 'ggtags-navigation-map 'prev-section-2 'previous-error))

(provide 'evil-collection-ggtags)
;;; evil-collection-ggtags.el ends here
