;;; evil-collection-elpaca.el --- Bindings for `elpaca'. -*- lexical-binding: t -*-

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
;;; Bindings for elpaca.

;;; Code:
(require 'evil-collection)
(require 'elpaca nil t)
(require 'elpaca-ui nil t)

(defvar elpaca-info-mode-map)
(defvar elpaca-ui-mode-map)
(declare-function "elpaca-defsearch" "elpaca-ui")

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

(defcustom evil-collection-elpaca-want-u-unmark t
  "When non nil, use u to unmark.

If this is t, flip the u/U bindings.
If this is nil, match original `elpaca' behavior."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-elpaca-want-g-filters t
  "When non nil, put `elpaca' filters on g prefix."
  :group 'evil-collection
  :type 'boolean)

(defun evil-collection-elpaca-setup ()
  "Set up `evil' bindings for elpaca."
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
    (kbd "R") 'elpaca-ui-search-refresh
    (kbd "gr") 'elpaca-ui-search-refresh
    (kbd "c") 'elpaca-ui-copy
    (kbd "d") 'elpaca-ui-mark-delete
    (kbd "i") 'elpaca-ui-mark-install
    (kbd "m") 'elpaca-manager
    (kbd "r") 'elpaca-ui-mark-rebuild
    (kbd "s") 'elpaca-ui-search
    (kbd "x") 'elpaca-ui-execute-marks)

  (if evil-collection-elpaca-want-u-unmark
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "u") 'elpaca-ui-unmark
        (kbd "U") 'elpaca-ui-mark-update)
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "U") 'elpaca-ui-unmark
      (kbd "u") 'elpaca-ui-mark-update))

  (if evil-collection-elpaca-want-v
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "gv") 'elpaca-visit
        (kbd "gd") 'elpaca-visit)
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "v") 'elpaca-visit))

  (if evil-collection-elpaca-want-g-filters
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "gI") (elpaca-defsearch 'installed "#unique #installed")
        (kbd "gM") (elpaca-defsearch 'marked   "#unique #marked")
        (kbd "gO") (elpaca-defsearch 'orphaned "#unique #orphan")
        (kbd "gT") (elpaca-defsearch 'tried "#unique #installed !#declared"))
    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "I") (elpaca-defsearch 'installed "#unique #installed")
      (kbd "M") (elpaca-defsearch 'marked   "#unique #marked")
      (kbd "O") (elpaca-defsearch 'orphaned "#unique #orphan")
      (kbd "T") (elpaca-defsearch 'tried "#unique #installed !#declared")))

  (if evil-collection-elpaca-want-movement
      (evil-collection-define-key 'normal 'elpaca-ui-mode-map
        (kbd "B") 'elpaca-ui-browse-package ;; b -> B
        (kbd "F") 'elpaca-ui-mark-fetch ;; f -> F
        (kbd "L") 'elpaca-log ;; l -> L
        ;; The original is on t but T is also a movement key as well as a
        ;; key bound to a filter. S is still an open key though and has
        ;; matches [S]tatus.
        ;; t -> S
        (kbd "S") 'elpaca-status)

    (evil-collection-define-key 'normal 'elpaca-ui-mode-map
      (kbd "b") 'elpaca-ui-browse-package
      (kbd "f") 'elpaca-ui-mark-fetch
      (kbd "l") 'elpaca-log
      (kbd "t") 'elpaca-status)))

(provide 'evil-collection-elpaca)
;;; evil-collection-elpaca.el ends here
