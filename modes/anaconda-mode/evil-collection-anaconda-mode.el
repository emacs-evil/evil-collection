;;; evil-collection-anaconda-mode.el --- Bindings for `anaconda-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, python, tools

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
;;; Bindings for `anaconda-mode'.

;;; Code:
(require 'anaconda-mode nil t)
(require 'evil-collection)

(defconst evil-collection-anaconda-mode-maps '(anaconda-view-mode-map
                                               anaconda-mode-map))

;;;###autoload
(defun evil-collection-anaconda-mode-setup ()
  "Set up `evil' bindings for `anaconda-mode'."
  ;; Bindings don't seem to be set the first time.
  (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps)

  ;; latest anaconda has replaced view mode by an xref implementation,
  ;; anaconda stable uses `anaconda-view-mode-map'
  (when (boundp 'anaconda-view-mode-map)
    (evil-collection-bind 'next-item    'anaconda-view-mode-map 'next-error-no-select)
    (evil-collection-bind 'prev-item    'anaconda-view-mode-map 'previous-error-no-select)
    (evil-collection-bind 'next-section 'anaconda-view-mode-map 'next-error-no-select)
    (evil-collection-bind 'prev-section 'anaconda-view-mode-map 'previous-error-no-select)
    (evil-collection-bind 'quit 'anaconda-view-mode-map 'quit-window))

  (evil-collection-bind 'find-usages     'anaconda-mode-map 'anaconda-mode-find-references)
  (evil-collection-bind 'lookup-doc      'anaconda-mode-map 'anaconda-mode-show-doc)
  (evil-collection-bind 'find-definition 'anaconda-mode-map 'anaconda-mode-find-definitions)
  (evil-collection-bind 'pop-definition  'anaconda-mode-map
                              (if (fboundp 'anaconda-mode-go-back)
                                  'anaconda-mode-go-back
                                'xref-pop-marker-stack))
  (when (evil-collection-binding-enabled-p 'find-usages)
    (evil-collection-define-key 'normal 'anaconda-mode-map
      "gA" 'anaconda-mode-find-assignments)))

(provide 'evil-collection-anaconda-mode)
;;; evil-collection-anaconda-mode.el ends here
