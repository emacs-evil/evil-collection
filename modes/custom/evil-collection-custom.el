;;; evil-collection-custom.el --- Evil bindings for Customize -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, custom, tools

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
;; Evil bindings for the Customize interface.

;;; Code:
(require 'cus-edit)
(require 'evil-collection)

(defconst evil-collection-custom-maps '(custom-mode-map))

;;;###autoload
(defun evil-collection-custom-setup ()
  "Set up `evil' bindings for `Custom-mode'."
  (evil-set-initial-state 'Custom-mode 'normal)

  (evil-collection-define-key 'normal 'custom-mode-map
    ;; motion
    (kbd "<tab>") 'widget-forward
    (kbd "S-<tab>") 'widget-backward
    (kbd "<backtab>") 'widget-backward
    (kbd "<delete>") 'scroll-down-command

    "^" 'Custom-goto-parent
    (kbd "C-o") 'Custom-goto-parent
    ;; TODO: Should the following be added?
    "<" 'Custom-goto-parent)
  (evil-collection-bind 'scroll-down  'custom-mode-map 'scroll-up-command)
  (evil-collection-bind 'scroll-up    'custom-mode-map 'scroll-down-command)
  (evil-collection-bind 'action       'custom-mode-map 'Custom-newline)
  (evil-collection-bind 'next-item    'custom-mode-map 'widget-forward)
  (evil-collection-bind 'prev-item    'custom-mode-map 'widget-backward)
  (evil-collection-bind 'next-section 'custom-mode-map 'widget-forward)
  (evil-collection-bind 'prev-section 'custom-mode-map 'widget-backward)
  (evil-collection-bind 'quit         'custom-mode-map 'Custom-buffer-done)
  (evil-collection-bind 'quit-save    'custom-mode-map 'Custom-buffer-done)
  (evil-collection-bind 'quit-cancel  'custom-mode-map 'evil-quit))

(provide 'evil-collection-custom)
;;; evil-collection-custom.el ends here
