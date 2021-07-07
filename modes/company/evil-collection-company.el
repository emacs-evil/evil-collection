;;; evil-collection-company.el --- Bindings for `company-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
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

(declare-function company-tng-mode "company-tng")

(defgroup evil-collection-company nil
  "Evil bindings for `company-mode'."
  :group 'evil-collection)

(defcustom evil-collection-company-supported-states '(insert replace emacs)
  "The `evil-state's which `company' function can be requested."
  :type '(repeat symbol))

(defvar company-active-map)
(defvar company-search-map)

(defconst evil-collection-company-maps '(company-active-map company-search-map))

(defun evil-collection-company-supported-p (command &rest _)
  "Return non-nil if `evil-state' is in supported states."
  (cond
   ((not (bound-and-true-p evil-mode)) t)
   ((eq command 'prefix)
    (memq evil-state evil-collection-company-supported-states))
   (t t)))

;;;###autoload
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
    (evil-collection-define-key nil 'company-active-map
      (kbd "C-u") 'company-previous-page))

  (when evil-want-C-d-scroll
    (evil-collection-define-key nil 'company-active-map
      (kbd "C-d") 'company-next-page))

  (evil-collection-define-key nil 'company-search-map
    (kbd "C-j") 'company-select-next-or-abort
    (kbd "C-k") 'company-select-previous-or-abort
    (kbd "M-j") 'company-select-next
    (kbd "M-k") 'company-select-previous
    (kbd "<escape>") 'company-search-abort)

  ;; Make `company-mode' not show popup when not in supported state
  (advice-add 'company-call-backend
              :before-while 'evil-collection-company-supported-p))


(provide 'evil-collection-company)
;;; evil-collection-company.el ends here
