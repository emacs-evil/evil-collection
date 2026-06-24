;;; evil-collection-ebuku.el --- Evil bindings for Ebuku -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Alexis

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, ebuku, tools

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

;; Evil bindings for Ebuku.

;;; Code:

(require 'evil-collection)
(require 'ebuku nil t)


;;;###autoload
(defun evil-collection-ebuku-setup ()
  "Set up `evil' bindings for `Ebuku'."
  (evil-collection-bind 'ebuku-mode-map
                        'edit 'ebuku-edit-bookmark
                        'quit 'quit-window
                        'refresh 'ebuku-refresh
                        'next-section 'ebuku-next-bookmark
                        'prev-section 'ebuku-previous-bookmark)
  (evil-collection-define-key 'normal 'ebuku-mode-map
                              "a" 'ebuku-add-bookmark
                              "j" 'ebuku-next-bookmark
                              "k" 'ebuku-previous-bookmark
                              "r" 'ebuku-search-on-recent
                              "*" 'ebuku-show-all
                              "-" 'ebuku-toggle-results-limit
                              "C" 'ebuku-copy-url
                              "T" 'ebuku-copy-title
                              "I" 'ebuku-copy-index
                              [mouse-1] 'ebuku-open-url
                              [mouse-2] 'ebuku-open-url)
  (evil-collection-bind 'ebuku-mode-map
                        'action 'ebuku-open-url
                        'delete 'ebuku-delete-bookmark
                        'delete-2 'ebuku-delete-bookmark
                        'search-or-filter 'ebuku-search
                        'toggle 'ebuku-toggle-results-limit))


(provide 'evil-collection-ebuku)
;;; evil-collection-ebuku.el ends here
