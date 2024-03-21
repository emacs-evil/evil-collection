;;; evil-collection-elpaca.el --- Bindings for `elpaca' -*- lexical-binding: t -*-

;; Copyright (C) 2023 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, emacs, convenience, tools

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
;;; Bindings for `elpaca'.

;;; Code:
(require 'evil-collection)
(require 'elpaca nil t)
(require 'elpaca-ui nil t)

(defvar elpaca-info-mode-map)
(defvar elpaca-ui-mode-map)
(defvar elpaca-ui-view-map)
(declare-function elpaca-ui-search "elpaca-ui")
(declare-function elpaca-ui-visit "elpaca-ui")


(defconst evil-collection-elpaca-maps '(elpaca-info-mode-map
                                        elpaca-ui-mode-map))

(defcustom evil-collection-elpaca-want-movement t
  "When non nil, prioritize movement keys like \"h\", \"l\", \"b\", etc."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-elpaca-want-v t
  "When non nil, use v for visual mode.
When this is true, move the `elpaca-visit' to gv and gd."
  :group 'evil-collection
  :type 'boolean)

(defun evil-collection-elpaca-ui-visit-build-dir ()
  "Visit package's build-dir."
  (interactive)
  (elpaca-ui-visit 'build))

(defmacro evil-collection-elpaca-defsearch (name query)
  "Return search command with NAME for QUERY."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "elpaca-ui-search-%s" name)) ()
     ,(format "Search for %S" query)
     (interactive)
     (elpaca-ui-search ,query)))

(defun evil-collection-elpaca-setup ()
  "Set up `evil' bindings for elpaca."
  (evil-collection-define-key 'normal 'elpaca-ui-view-map
    (kbd "a") (evil-collection-elpaca-defsearch marked   "#unique #marked")
    (kbd "i") (evil-collection-elpaca-defsearch installed "#unique #installed")
    (kbd "l") 'elpaca-log
    (kbd "m") 'elpaca-manager
    (kbd "o") (evil-collection-elpaca-defsearch orphaned "#unique #orphan")
    (kbd "r") 'elpaca-ui-search-refresh
    (kbd "t") (evil-collection-elpaca-defsearch tried "#unique #installed !#declared")
    (kbd "b") 'evil-collection-elpaca-ui-visit-build-dir)

  (evil-collection-define-key 'normal 'elpaca-info-mode-map
    (kbd "TAB") 'forward-button
    (kbd "<tab>") 'forward-button
    (kbd "S-TAB") 'backward-button
    (kbd "<backtab>") 'backward-button
    (kbd "i") 'elpaca-info
    (kbd "s") 'elpaca-info)

  (evil-collection-define-key 'normal 'elpaca-ui-mode-map
    (kbd "RET") 'elpaca-ui-info
    (kbd "!") 'elpaca-ui-send-input
    (kbd "+") 'elpaca-ui-show-hidden-rows
    (kbd "c") 'elpaca-ui-copy
    (kbd "d") 'elpaca-ui-mark-delete
    (kbd "g") elpaca-ui-view-map
    (kbd "i") 'elpaca-ui-mark-try
    (kbd "m") 'elpaca-ui-mark-merge
    (kbd "p") 'elpaca-ui-mark-pull
    (kbd "r") 'elpaca-ui-mark-rebuild
    (kbd "s") 'elpaca-ui-search
    (kbd "u") 'elpaca-ui-unmark
    (kbd "x") 'elpaca-ui-execute-marks)

  (if evil-collection-elpaca-want-v
      (evil-collection-define-key 'normal 'elpaca-ui-view-map
        (kbd "v") 'elpaca-ui-visit)
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "v") 'elpaca-ui-visit))

  (if evil-collection-elpaca-want-movement
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "B") 'elpaca-ui-browse-package ;; b -> B
        (kbd "F") 'elpaca-ui-mark-fetch ;; f -> F
        )
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "b") 'elpaca-ui-browse-package
      (kbd "f") 'elpaca-ui-mark-fetch)))

(provide 'evil-collection-elpaca)
;;; evil-collection-elpaca.el ends here
