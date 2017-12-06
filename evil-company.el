;;; evil-company.el --- Bindings for `company-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
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
(require 'evil)

(defgroup evil-company nil
  "Evil bindings for `company-mode'."
  :group 'evil-collection)

(defcustom evil-company-use-tng t
  "Enable company-tng through `company-tng-configure-default'.

This mirrors ycmd's behavior for a completion experience more
similar to YouCompleteMe.

Note that for changes to take effect, this variable may have to
be set through custom or before evil-collection loads."
  :group 'evil-company
  :type 'boolean)

(defvar company-active-map)
(defvar company-search-map)

(defun evil-company-setup ()
  "Set up `evil' bindings for `company'."
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "M-j") 'company-select-next)
  (define-key company-active-map (kbd "M-k") 'company-select-previous)

  (when evil-want-C-u-scroll
    (define-key company-active-map (kbd "C-u") 'company-previous-page))

  (when evil-want-C-d-scroll
    (define-key company-active-map (kbd "C-d") 'company-next-page))

  (define-key company-search-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-search-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-search-map (kbd "M-j") 'company-select-next)
  (define-key company-search-map (kbd "M-k") 'company-select-previous)

  ;; Sets up YCMD like behavior.
  (when evil-company-use-tng
    (with-no-warnings (company-tng-configure-default))))

(provide 'evil-company)
;;; evil-company.el ends here
