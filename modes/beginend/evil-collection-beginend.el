;;; evil-collection-beginend.el --- Evil bindings for beginend -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>
;; Maintainer: Balaji Sivaraman <balaji@balajisivaraman.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, tools

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
;; Evil bindings for beginend.

;;; Code:

(require 'evil-collection)
(require 'beginend nil t)

(defconst evil-collection-beginend-maps '(beginend-bs-mode-map
                                          beginend-rg-mode-map
                                          beginend-org-mode-map
                                          beginend-deft-mode-map
                                          beginend-prog-mode-map
                                          beginend-LaTeX-mode-map
                                          beginend-dired-mode-map
                                          beginend-latex-mode-map
                                          beginend-nroam-mode-map
                                          beginend-occur-mode-map
                                          beginend-vc-dir-mode-map
                                          beginend-ibuffer-mode-map
                                          beginend-message-mode-map
                                          beginend-outline-mode-map
                                          beginend-prodigy-mode-map
                                          beginend-org-agenda-mode-map
                                          beginend-compilation-mode-map
                                          beginend-epa-key-list-mode-map
                                          beginend-magit-status-mode-map
                                          beginend-elfeed-search-mode-map
                                          beginend-magit-revision-mode-map
                                          beginend-notmuch-search-mode-map
                                          beginend-recentf-dialog-mode-map))


(defmacro evil-beginend--define-goto-beginning-motion (ec-mode-name)
  "Macro to define new Evil motion that will use the corresponding
beginend-goto-beginning function for EC-MODE-NAME when count is not
provided for the motion; otherwise behave like `evil-goto-first-line'
if count is provided.

This will also associate the regular Evil `gg' keybinding with the
newly defined motion."
  (let ((motion-name (intern (format "evil-beginend-%s-goto-beginning" ec-mode-name)))
        (beginend-beginning-fn-name (intern (format "beginend-%s-goto-beginning" ec-mode-name)))
        (beginend-map-name (intern (format "beginend-%s-map" ec-mode-name))))
    `(progn
       (declare-function ,beginend-beginning-fn-name "beginend")
       (evil-define-motion ,motion-name (count)
         :jump t
         :type line
         (if count
             (evil-goto-first-line count)
           (,beginend-beginning-fn-name)))
       (evil-collection-define-key 'normal ',beginend-map-name
         "gg" ',motion-name))))

(defmacro evil-beginend--define-goto-end-motion (ec-mode-name)
  "Macro to define new Evil motion that will use the corresponding
beginend-goto-end function for EC-MODE-NAME when count is not
provided for the motion; otherwise behave like `evil-goto-line'
if count is provided.

This will also associate the regular Evil `G' keybinding with the
newly defined motion."
  (let ((motion-name (intern (format "evil-beginend-%s-goto-end" ec-mode-name)))
        (beginend-end-fn-name (intern (format "beginend-%s-goto-end" ec-mode-name)))
        (beginend-map-name (intern (format "beginend-%s-map" ec-mode-name))))
    `(progn
       (declare-function ,beginend-end-fn-name "beginend")
       (evil-define-motion ,motion-name (count)
         :jump t
         :type line
         (if count
             (evil-goto-line count)
           (,beginend-end-fn-name)))
       (evil-collection-define-key 'normal ',beginend-map-name
         "G" ',motion-name))))

;;###autoload
(defun evil-collection-beginend-setup ()
  "Set up `evil' bindings for `beginend'."
  (evil-beginend--define-goto-beginning-motion "bs-mode")
  (evil-beginend--define-goto-end-motion "bs-mode")
  (evil-beginend--define-goto-beginning-motion "rg-mode")
  (evil-beginend--define-goto-end-motion "rg-mode")
  (evil-beginend--define-goto-beginning-motion "org-mode")
  (evil-beginend--define-goto-end-motion "org-mode")
  (evil-beginend--define-goto-beginning-motion "deft-mode")
  (evil-beginend--define-goto-end-motion "deft-mode")
  (evil-beginend--define-goto-beginning-motion "prog-mode")
  (evil-beginend--define-goto-end-motion "prog-mode")
  (evil-beginend--define-goto-beginning-motion "LaTeX-mode")
  (evil-beginend--define-goto-end-motion "LaTeX-mode")
  (evil-beginend--define-goto-beginning-motion "nroam-mode")
  (evil-beginend--define-goto-end-motion "nroam-mode")
  (evil-beginend--define-goto-beginning-motion "occur-mode")
  (evil-beginend--define-goto-end-motion "occur-mode")
  (evil-beginend--define-goto-beginning-motion "vc-dir-mode")
  (evil-beginend--define-goto-end-motion "vc-dir-mode")
  (evil-beginend--define-goto-beginning-motion "ibuffer-mode")
  (evil-beginend--define-goto-end-motion "ibuffer-mode")
  (evil-beginend--define-goto-beginning-motion "message-mode")
  (evil-beginend--define-goto-end-motion "message-mode")
  (evil-beginend--define-goto-beginning-motion "outline-mode")
  (evil-beginend--define-goto-end-motion "outline-mode")
  (evil-beginend--define-goto-beginning-motion "prodigy-mode")
  (evil-beginend--define-goto-end-motion "prodigy-mode")
  (evil-beginend--define-goto-beginning-motion "org-agenda-mode")
  (evil-beginend--define-goto-end-motion "org-agenda-mode")
  (evil-beginend--define-goto-beginning-motion "compilation-mode")
  (evil-beginend--define-goto-end-motion "compilation-mode")
  (evil-beginend--define-goto-beginning-motion "epa-key-list-mode")
  (evil-beginend--define-goto-end-motion "epa-key-list-mode")
  (evil-beginend--define-goto-beginning-motion "magit-status-mode")
  (evil-beginend--define-goto-end-motion "magit-status-mode")
  (evil-beginend--define-goto-beginning-motion "elfeed-search-mode")
  (evil-beginend--define-goto-end-motion "elfeed-search-mode")
  (evil-beginend--define-goto-beginning-motion "magit-revision-mode")
  (evil-beginend--define-goto-end-motion "magit-revision-mode")
  (evil-beginend--define-goto-beginning-motion "notmuch-search-mode")
  (evil-beginend--define-goto-end-motion "notmuch-search-mode")
  (evil-beginend--define-goto-beginning-motion "recentf-dialog-mode")
  (evil-beginend--define-goto-end-motion "recentf-dialog-mode"))

(provide 'evil-collection-beginend)
;;; evil-collection-beginend.el ends here
