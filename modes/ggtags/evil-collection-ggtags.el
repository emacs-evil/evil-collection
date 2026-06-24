;;; evil-collection-ggtags.el --- Evil bindings for ggtags -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

  (evil-collection-bind 'ggtags-mode-map
                        'find-usages 'ggtags-find-reference
                        'find-definition 'ggtags-find-tag-dwim
                        'pop-definition 'ggtags-prev-mark
                        'find-file 'ggtags-find-file)

  (evil-collection-define-key 'normal 'ggtags-view-search-history-mode-map
    "r" 'ggtags-save-to-register
    "R" 'ggtags-view-search-history-action)
  (evil-collection-bind 'ggtags-view-search-history-mode-map
                        'next-item 'ggtags-view-search-history-next
                        'prev-item 'ggtags-view-search-history-prev
                        'next-section 'ggtags-view-search-history-next
                        'prev-section 'ggtags-view-search-history-prev
                        'quit 'ggtags-kill-window
                        'refresh 'ggtags-view-search-history-update
                        'delete 'ggtags-view-search-history-kill
                        'delete-2 'ggtags-view-search-history-kill)

  (evil-collection-bind 'ggtags-view-tag-history-mode-map
                        'next-item 'next-error-no-select
                        'prev-item 'previous-error-no-select
                        'next-section 'next-error-no-select
                        'prev-section 'previous-error-no-select
                        'quit 'ggtags-kill-window)

  (evil-collection-define-key 'normal 'ggtags-navigation-map
    ;; search
    "S" 'ggtags-navigation-isearch-forward

    "go" 'ggtags-navigation-visible-mode ;; FIXME: This can be anything.
    )
  (evil-collection-bind 'ggtags-navigation-map
                        'action 'ggtags-navigation-mode-done
                        'next-item 'next-error
                        'prev-item 'previous-error
                        'next-section 'ggtags-navigation-next-file
                        'prev-section 'ggtags-navigation-previous-file
                        'next-section-2 'next-error
                        'prev-section-2 'previous-error
                        'search-or-filter 'ggtags-navigation-isearch-forward))

(provide 'evil-collection-ggtags)
;;; evil-collection-ggtags.el ends here
