;;; evil-collection-dired-sidebar.el --- Evil bindings for dired-sidebar -*- lexical-binding: t; -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, dired, tools

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
;; Evil bindings for `dired-sidebar'

;;; Code:

(require 'evil-collection)
(require 'dired-sidebar nil t)

(defconst evil-collection-dired-sidebar-maps '(dired-sidebar-mode-map))

;;;###autoload
(defun evil-collection-dired-sidebar-setup ()
  "Set up `evil' bindings for `dired-sidebar'."
  (evil-collection-define-key 'normal 'dired-sidebar-mode-map
    "^" 'dired-sidebar-up-directory
    "-" 'dired-sidebar-up-directory
    [mouse-2] 'dired-sidebar-mouse-subtree-cycle-or-find-file)
  (evil-collection-bind 'dired-sidebar-mode-map
                        'section-toggle 'dired-sidebar-subtree-toggle
                        'action 'dired-sidebar-find-file
                        'action-other 'dired-sidebar-find-file-alt
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window))

(provide 'evil-collection-dired-sidebar)
;;; evil-collection-dired-sidebar.el ends here
