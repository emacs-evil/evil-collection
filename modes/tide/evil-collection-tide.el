;;; evil-collection-tide.el --- Bindings for `tide-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tide, typescript, languages

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
;;; Bindings for `tide-mode'.

;;; Code:
(require 'tide nil t)
(require 'evil-collection)

(defconst evil-collection-tide-maps '(tide-mode-map
                                      tide-references-mode-map
                                      tide-project-errors-mode-map))

;;;###autoload
(defun evil-collection-tide-setup ()
  "Set up `evil' bindings for `tide'."
  (evil-collection-theme-bind 'find-definition 'tide-mode-map 'tide-jump-to-definition)
  (evil-collection-theme-bind 'pop-definition  'tide-mode-map 'tide-jump-back)
  (evil-collection-theme-bind 'lookup-doc      'tide-mode-map 'tide-documentation-at-point)

  (evil-collection-define-key 'normal 'tide-references-mode-map
    (kbd "RET") 'tide-goto-line-reference)
  (evil-collection-theme-bind 'next-item    'tide-references-mode-map 'tide-find-next-reference)
  (evil-collection-theme-bind 'prev-item    'tide-references-mode-map 'tide-find-previous-reference)
  (evil-collection-theme-bind 'next-section 'tide-references-mode-map 'tide-find-next-reference)
  (evil-collection-theme-bind 'prev-section 'tide-references-mode-map 'tide-find-previous-reference)
  (evil-collection-theme-bind 'quit 'tide-references-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'tide-project-errors-mode-map
    (kbd "RET") 'tide-goto-error)
  (evil-collection-theme-bind 'next-item    'tide-project-errors-mode-map 'tide-find-next-error)
  (evil-collection-theme-bind 'prev-item    'tide-project-errors-mode-map 'tide-find-previous-error)
  (evil-collection-theme-bind 'next-section 'tide-project-errors-mode-map 'tide-find-next-error)
  (evil-collection-theme-bind 'prev-section 'tide-project-errors-mode-map 'tide-find-previous-error)
  (evil-collection-theme-bind 'quit 'tide-project-errors-mode-map 'quit-window))

(provide 'evil-collection-tide)
;;; evil-collection-tide.el ends here
