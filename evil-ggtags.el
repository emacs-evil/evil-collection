;;; evil-ggtags.el --- Evil bindings for ggtags -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
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
(require 'evil-collection-util)
(require 'ggtags nil t)

(defvar ggtags-global-mode-map)
(defvar ggtags-view-search-history-mode-map)
(defvar ggtags-view-tag-history-mode-map)
(defvar ggtags-navigation-map)

(defun evil-ggtags-setup ()
  "Set up `evil' bindings for `ggtags'."
  (evil-collection-util-evilify-map
   ggtags-global-mode-map
   :mode ggtags-global-mode)

  (evil-collection-util-evilify-map
   ggtags-view-search-history-mode-map
   :bindings
   "j" 'ggtags-view-search-history-prev
   "k" 'ggtags-view-search-history-next
   (kbd "C-j") 'ggtags-view-search-history-prev
   (kbd "C-k") 'ggtags-view-search-history-next
   "x" 'ggtags-view-search-history-kill
   "r" 'ggtags-save-to-register
   "\r" 'ggtags-view-search-history-action
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end)

  (evil-collection-util-evilify-map
   ggtags-view-tag-history-mode-map
   :bindings
   (kbd "C-j") 'next-error-no-select
   (kbd "C-k") 'previous-error-no-select
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end)

  (evil-collection-util-evilify-map
   ggtags-view-tag-history-mode-map
   :bindings
   (kbd "C-j") 'next-error-no-select
   (kbd "C-k") 'previous-error-no-select
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end)

  (evil-collection-util-evilify-map
   ggtags-navigation-map
   :bindings
   (kbd "C-j") 'next-error
   (kbd "C-k") 'previous-error
   (kbd "M-j") 'ggtags-navigation-next-file
   (kbd "M-k") 'ggtags-navigation-previous-file
   (kbd "M-=") 'ggtags-navigation-start-file
   (kbd "M->") 'ggtags-navigation-last-error
   (kbd "M-<") 'first-error
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end))

(provide 'evil-ggtags)
;;; evil-ggtags.el ends here
