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

;; TODO: `company-tng-configure-default' will be replaced by `company-tng-mode' in 0.9.14.
;; See https://github.com/company-mode/company-mode/blob/master/NEWS.md
(declare-function company-tng-configure-default "company-tng")
(declare-function company-tng-mode "company-tng")

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

(defcustom evil-collection-company-supported-states '(insert replace emacs)
  "The `evil-state's which `company' function can be requested."
  :type '(repeat symbol))

(defvar company-active-map)
(defvar company-search-map)

(defconst evil-collection-company-maps '(company-active-map company-search-map))

(defun evil-collection-company-supported-p (command &rest _)
  "Return non-nil if `evil-state' is in supported states."
  (cond
    ((eq command 'prefix)
     (memq evil-state evil-collection-company-supported-states))
    (t t)))

;;;###autoload
(defun evil-collection-company-setup ()
  "Set up `evil' bindings for `company'."
  (evil-collection-move-bindings-to-emacs-state 'company-active-map
    (kbd "C-n") (kbd "C-p") (kbd "C-j") (kbd "C-k") (kbd "M-j") (kbd "M-k"))

  (when evil-want-C-u-scroll
    (evil-collection-move-bindings-to-emacs-state 'company-active-map
      (kbd "C-u")))

  (when evil-want-C-d-scroll
    (evil-collection-move-bindings-to-emacs-state 'company-active-map
      (kbd "C-d")))

  (evil-collection-move-bindings-to-emacs-state 'company-search-map
    (kbd "C-j") (kbd "C-k") (kbd "M-j") (kbd "M-k") (kbd "<escape>"))

  ;; Sets up YCMD like behavior.
  (when evil-collection-company-use-tng
    (require 'company-tng)
    (if (fboundp 'company-tng-mode)
        (company-tng-mode +1)
      (company-tng-configure-default)))

  ;; Make `company-mode' not show popup when not in supported state
  (advice-add 'company-call-backend :before-while 'evil-collection-company-supported-p))

(provide 'evil-collection-company)
;;; evil-collection-company.el ends here
