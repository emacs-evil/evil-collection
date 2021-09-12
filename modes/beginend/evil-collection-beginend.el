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

;;###autoload
(defun evil-collection-beginend-setup ()
  "Set up `evil' bindings for `beginend'."
  (evil-collection-define-key 'normal 'beginend-bs-mode-map
    "gg" 'beginend-bs-mode-goto-beginning
    "G" 'beginend-bs-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-rg-mode-map
    "gg" 'beginend-rg-mode-goto-beginning
    "G" 'beginend-rg-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-org-mode-map
    "gg" 'beginend-org-mode-goto-beginning
    "G" 'beginend-org-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-deft-mode-map
    "gg" 'beginend-deft-mode-goto-beginning
    "G" 'beginend-deft-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-prog-mode-map
    "gg" 'beginend-prog-mode-goto-beginning
    "G" 'beginend-prog-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-LaTeX-mode-map
    "gg" 'beginend-LaTeX-mode-goto-beginning
    "G" 'beginend-LaTeX-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-dired-mode-map
    "gg" 'beginend-dired-mode-goto-beginning
    "G" 'beginend-dired-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-latex-mode-map
    "gg" 'beginend-latex-mode-goto-beginning
    "G" 'beginend-latex-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-nroam-mode-map
    "gg" 'beginend-nroam-mode-goto-beginning
    "G" 'beginend-nroam-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-occur-mode-map
    "gg" 'beginend-occur-mode-goto-beginning
    "G" 'beginend-occur-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-vc-dir-mode-map
    "gg" 'beginend-vc-dir-mode-goto-beginning
    "G" 'beginend-vc-dir-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-ibuffer-mode-map
    "gg" 'beginend-ibuffer-mode-goto-beginning
    "G" 'beginend-ibuffer-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-message-mode-map
    "gg" 'beginend-message-mode-goto-beginning
    "G" 'beginend-message-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-outline-mode-map
    "gg" 'beginend-outline-mode-goto-beginning
    "G" 'beginend-outline-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-prodigy-mode-map
    "gg" 'beginend-prodigy-mode-goto-beginning
    "G" 'beginend-prodigy-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-org-agenda-mode-map
    "gg" 'beginend-org-agenda-mode-goto-beginning
    "G" 'beginend-org-agenda-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-compilation-mode-map
    "gg" 'beginend-compilation-mode-goto-beginning
    "G" 'beginend-compilation-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-epa-key-mode-map
    "gg" 'beginend-epa-key-mode-goto-beginning
    "G" 'beginend-epa-key-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-magit-status-mode-map
    "gg" 'beginend-magit-status-mode-goto-beginning
    "G" 'beginend-magit-status-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-elfeed-search-mode-map
    "gg" 'beginend-elfeed-search-mode-goto-beginning
    "G" 'beginend-elfeed-search-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-magit-revision-mode-map
    "gg" 'beginend-magit-revision-mode-goto-beginning
    "G" 'beginend-magit-revision-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-notmuch-search-mode-map
    "gg" 'beginend-notmuch-search-mode-goto-beginning
    "G" 'beginend-notmuch-search-mode-goto-end)

  (evil-collection-define-key 'normal 'beginend-recentf-dialog-mode-map
    "gg" 'beginend-recentf-dialog-mode-goto-beginning
    "G" 'beginend-recentf-dialog-mode-goto-end))

(provide 'evil-collection-beginend)
;;; evil-collection-beginend.el ends here
