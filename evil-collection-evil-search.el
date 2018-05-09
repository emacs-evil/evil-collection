;;; evil-collection-evil-search.el --- Utility for `evil-search'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, tools

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
;;; Utility for `evil-search'.
;;; https://github.com/emacs-evil/evil-collection/issues/49
;;; https://github.com/emacs-evil/evil-magit/issues/33

;;; Code:
(require 'evil-search)

(defun evil-collection-evil-search-enabled ()
  (eq evil-search-module 'evil-search))

(defvar evil-collection-evil-search-forward
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-forward
                                 #'evil-search-forward))))

(defvar evil-collection-evil-search-backward
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-backward
                                 #'evil-search-backward))))

(defvar evil-collection-evil-search-next
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-next
                                 #'evil-search-next))))

(defvar evil-collection-evil-search-previous
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-previous
                                 #'evil-search-previous))))

(provide 'evil-collection-evil-search)
;;; evil-collection-evil-search.el ends here
