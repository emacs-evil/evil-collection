;;; evil-ggtags.el --- Evil integration for ggtags. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, ggtags
;; HomePage: https://github.com/jojojames/evil-collection

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
;; Evil integration for `ggtags-mode'.

;;; Code:
(require 'evil-collection-util)
(require 'ggtags)

(defun evil-ggtags-setup ()
  (+evilify-map
   ggtags-global-mode-map
   :mode ggtags-global-mode)

  (+evilify-map
   ggtags-view-search-history-mode-map
   :bindings
   "j" 'ggtags-view-search-history-prev
   "\C-j" 'ggtags-view-search-history-prev
   "k" 'ggtags-view-search-history-next
   "\C-k" 'ggtags-view-search-history-next
   "x" 'ggtags-view-search-history-kill
   "r" 'ggtags-save-to-register
   "\r" 'ggtags-view-search-history-action
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end)

  (+evilify-map
   ggtags-view-tag-history-mode-map
   :bindings
   "\C-j" 'next-error-no-select
   "\C-k" 'previous-error-no-select
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end)

  (+evilify-map
   ggtags-view-tag-history-mode-map
   :bindings
   "\C-j" 'next-error-no-select
   "\C-k" 'previous-error-no-select
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end)

  (+evilify-map
   ggtags-navigation-map
   :bindings
   "\C-j" 'next-error
   "\C-k" 'previous-error
   "\M-j" 'ggtags-navigation-next-file
   "\M-k" 'ggtags-navigation-previous-file
   "\M-=" 'ggtags-navigation-start-file
   "\M->" 'ggtags-navigation-last-error
   "\M-<" 'first-error
   "e" 'evil-forward-word-end
   "E" 'evil-forward-WORD-end))

(provide 'evil-ggtags)
;;; evil-ggtags.el ends here
