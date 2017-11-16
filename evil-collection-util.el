;;; evil-collection-util.el --- Base Evil Utilities -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
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
;; This package provides a set of utility functions to integrate with
;; `evil-mode'.

;;; Code:
(require 'evil)
(require 'evil-evilified-state)

(defmacro +evil-set-default-state-for-mode (mode state)
  "Set the default STATE for MODE."
  (let* ((mode-str (symbol-name mode))
         (state-str (symbol-name state))
         (defun-name (intern (format "evil-integration-%s-set-%s-default"
                                     mode-str
                                     state-str))))
    `(progn
       (defun ,defun-name (&rest _)
         ,(format "Default `evil-state' of `%s' to '%s." mode-str state-str)
         (if ,mode
             (,(intern (format "evil-%s-state" state)))
           (evil-normal-state)))
       (advice-add #',mode :after #',defun-name))))

(defmacro +evilify-map (map &rest props)
  "`evilified-state-evilify-map' with additional bindings.
This assumes the :bindings key is at the end."
  (let ((contains-bindings (plist-get props :bindings)))
    `(evilified-state-evilify-map
       ,map
       ,@props
       ,@(unless contains-bindings
           '(:bindings))
       "\C-h" 'help-command
       "#" 'evil-search-word-backward
       "*" 'evil-search-forward
       "$" 'evil-end-of-line
       "^" 'evil-first-non-blank
       "0" 'evil-digit-argument-or-evil-beginning-of-line
       "b" 'evil-backward-word-begin
       "B" 'evil-backward-WORD-begin
       "w" 'evil-forward-word-begin
       "W" 'evil-forward-WORD-begin)))

(provide 'evil-collection-util)
;;; evil-collection-util.el ends here
