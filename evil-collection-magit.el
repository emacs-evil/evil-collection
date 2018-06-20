;;; evil-collection-magit.el --- Bindings for `magit' -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

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
;;; Bindings for `magit'
;;; This file is to work around an issue described in
;;; https://github.com/emacs-evil/evil-collection/issues/108
;;; Ideally this file is only temporary and should be removed once
;;; #108 is resolved.

;;; Code:
(require 'evil-collection)
(require 'magit nil t)

(defvar magit-blame-mode-map)

(defconst evil-collection-magit-maps '(magit-blame-mode-map))

(defun evil-collection-magit-setup ()
  "Set up `evil' bindings for `magit'."
  (evil-collection-define-key 'normal 'magit-blame-mode-map
    "q" 'magit-blame-quit))

(provide 'evil-collection-magit)
;;; evil-collection-magit.el ends here
