;;; evil-collection-org-present.el --- Bindings for `org-present' -*- lexical-binding: t -*-

;; Copyright (C) 2020 Carla Cao

;; Author: Carla Cao <ccao001@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools, minimalist, presentation

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
;;; Bindings for`org-present'.

;;; Code:
(require 'evil-collection)
(require 'org-present nil t)

(defconst evil-collection-org-present-maps '(org-present-mode-keymap))

;;;###autoload
(defun evil-collection-org-present-setup ()
  "Set up `evil' bindings for `org-present'."
  (evil-collection-define-key 'normal 'org-present-mode-keymap
    "J" 'org-present-next
    "K" 'org-present-prev
    (kbd "SPC") 'org-present-next
    (kbd "S-SPC") 'org-present-prev
    (kbd "M-j") 'org-present-next
    (kbd "M-k") 'org-present-prev
    "zi" 'org-present-big
    "zo" 'org-present-small
    "+" 'org-present-big
    "=" 'org-present-big
    "-" 'org-present-small
    "r" 'org-present-read-only
    "gw" 'org-present-read-write
    "gg" 'org-present-beginning
    "G" 'org-present-end)
  (evil-collection-bind 'org-present-mode-keymap
                        'next-item 'org-present-next
                        'prev-item 'org-present-prev
                        'next-section 'org-present-next
                        'prev-section 'org-present-prev
                        'quit 'org-present-quit
                        'quit-save 'org-present-quit
                        'quit-cancel 'org-present-quit))

(provide 'evil-collection-org-present)
;;; evil-collection-org-present.el ends here
