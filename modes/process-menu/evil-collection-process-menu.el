;;; evil-collection-process-menu.el --- Evil bindings for process-menu-mode -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, process, tools

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
;; Evil bindings for process-menu-mode (M-x list-processes).

;;; Code:
(require 'evil-collection)

(defconst evil-collection-process-menu-maps '(process-menu-mode-map))

;;;###autoload
(defun evil-collection-process-menu-setup ()
  "Set up `evil' bindings for `list-processes'."
  (evil-collection-set-readonly-bindings 'process-menu-mode-map)
  (evil-collection-define-key 'normal 'process-menu-mode-map
    "S" 'tabulated-list-sort)

  ;; motion
  ;; TODO: Implement beginning-of-buffer / end-of-buffer.
  (evil-collection-bind 'process-menu-mode-map
                        'next-button 'forward-button
                        'previous-button 'backward-button
                        'scroll-down 'evil-scroll-down
                        'scroll-up 'evil-scroll-up
                        'refresh 'revert-buffer
                        'delete 'process-menu-delete-process
                        'delete-2 'process-menu-delete-process))

(provide 'evil-collection-process-menu)
;;; evil-collection-process-menu.el ends here
