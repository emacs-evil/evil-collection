;;; evil-collection-tide.el --- Bindings for `tide-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
  (evil-collection-bind 'tide-mode-map
                        'find-definition 'tide-jump-to-definition
                        'pop-definition 'tide-jump-back
                        'lookup-doc 'tide-documentation-at-point)

  (evil-collection-bind 'tide-references-mode-map
                        'action 'tide-goto-line-reference
                        'next-item 'tide-find-next-reference
                        'prev-item 'tide-find-previous-reference
                        'next-section 'tide-find-next-reference
                        'prev-section 'tide-find-previous-reference
                        'quit 'quit-window)

  (evil-collection-bind 'tide-project-errors-mode-map
                        'action 'tide-goto-error
                        'next-item 'tide-find-next-error
                        'prev-item 'tide-find-previous-error
                        'next-section 'tide-find-next-error
                        'prev-section 'tide-find-previous-error
                        'quit 'quit-window))

(provide 'evil-collection-tide)
;;; evil-collection-tide.el ends here
