;;; evil-collection-man.el --- Evil bindings for Man-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, man, tools

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
;; Evil bindings for `man'.

;;; Code:
(require 'evil-collection)
(require 'man)

(defconst evil-collection-man-maps '(Man-mode-map))

;;;###autoload
(defun evil-collection-man-setup ()
  "Set up `evil' bindings for `man'."
  (evil-set-initial-state 'Man-mode 'normal)
  (evil-collection-bind 'Man-mode-map     'next-button 'forward-button)
  (evil-collection-bind 'Man-mode-map 'previous-button 'backward-button)
  (evil-collection-define-key 'normal 'Man-mode-map
    ;; goto
    "gm" 'man
    "gd" 'Man-goto-section ; TODO: "gd" does not match the rationale of "go to definition". Change?
    "gR" 'Man-follow-manual-reference ; TODO: Make this match Info-follow-reference?
    "gs" 'Man-goto-see-also-section)
  (evil-collection-bind 'Man-mode-map  'scroll-down 'scroll-up-command)
  (evil-collection-bind 'Man-mode-map    'scroll-up 'scroll-down-command)
  (evil-collection-bind 'Man-mode-map    'next-item 'Man-next-manpage)
  (evil-collection-bind 'Man-mode-map    'prev-item 'Man-previous-manpage)
  (evil-collection-bind 'Man-mode-map 'next-section 'Man-next-manpage)
  (evil-collection-bind 'Man-mode-map 'prev-section 'Man-previous-manpage)
  (evil-collection-bind 'Man-mode-map 'next-section-2 'Man-next-section)
  (evil-collection-bind 'Man-mode-map 'prev-section-2 'Man-previous-section)
  (evil-collection-bind 'Man-mode-map        'quit 'quit-window)
  (evil-collection-bind 'Man-mode-map   'quit-save 'quit-window)
  (evil-collection-bind 'Man-mode-map 'quit-cancel 'quit-window)
  (evil-collection-bind 'Man-mode-map     'refresh 'Man-update-manpage))

(provide 'evil-collection-man)
;;; evil-collection-man.el ends here
