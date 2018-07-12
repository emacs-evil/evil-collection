;;; evil-collection-company.el --- Bindings for `company-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, company, abbrev, convenience, matching

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
;;; Bindings for `company-mode'.

;;; Code:
(require 'company nil t)
(require 'evil-collection)

(declare-function company-tng-configure-default "company-tng")

(defgroup evil-collection-company nil
  "Evil bindings for `company-mode'."
  :group 'evil-collection)

(defcustom evil-collection-company-use-tng t
  "Enable company-tng through `company-tng-configure-default'.

This mirrors ycmd's behavior for a completion experience more
similar to YouCompleteMe.

Note that for changes to take effect, this variable may have to
be set through custom or before evil-collection loads."
  :group 'evil-collection-company
  :type 'boolean)

(defvar company-active-map)
(defvar company-search-map)

(defconst evil-collection-company-maps '(company-active-map company-search-map))

(defun evil-collection-company-setup ()
  "Set up `evil' bindings for `company'."
  (evil-collection-define-key nil 'company-active-map
    (kbd "C-n") 'company-select-next-or-abort
    (kbd "C-p") 'company-select-previous-or-abort
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous)

  (when evil-want-C-u-scroll
    (evil-collection-define-key nil 'company-active-map (kbd "C-u") 'company-previous-page))

  (when evil-want-C-d-scroll
    (evil-collection-define-key nil 'company-active-map (kbd "C-d") 'company-next-page))

  (evil-collection-define-key nil 'company-search-map
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)

  ;; Sets up YCMD like behavior.
  (when evil-collection-company-use-tng (company-tng-configure-default)))

(provide 'evil-collection-company)
;;; evil-collection-company.el ends here
