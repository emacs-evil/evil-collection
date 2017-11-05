;;; evil-ansi-term.el --- Evil integration for ansi-term. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, ansi-term
;; HomePage: https://github.com/jojojames/evil-collection

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
;; Evil integration for `ansi-term' and `multi-term'.

;;; Code:
(require 'term)
(require 'evil-collection-util)

(defun +evil-escape-stay ()
  "Go back to Normal State but don't move cursor backwards.
Moving cursor backwards when going back to Normal is default Vim behavior but
is not good default behavior in some cases. (Like Terminal)"
  (interactive)
  (evil-normal-state)
  (evil-forward-char 1))

(defun +evil-term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

(defun evil-ansi-term-set-keys ()
  (evil-define-key 'normal term-raw-map
    (kbd "p") 'term-paste
    (kbd "C-h") 'help-command)

  (evil-define-key 'insert term-raw-map
    (kbd "C-a") 'term-send-raw
    (kbd "C-b") 'term-send-raw
    (kbd "C-d") 'term-send-raw
    (kbd "C-f") 'term-send-raw
    (kbd "C-e") 'term-send-raw
    (kbd "C-h") 'help-command
    (kbd "C-k") 'term-send-raw
    (kbd "C-l") 'term-send-raw
    (kbd "C-r") 'term-send-raw
    (kbd "C-u") 'term-send-raw
    (kbd "C-w") 'term-send-raw
    (kbd "C-y") 'term-send-raw
    (kbd "<escape>") #'+evil-escape-stay
    (kbd "ESC") #'+evil-escape-stay
    (kbd "C-c C-d") 'term-send-eof
    (kbd "C-c C-z") 'term-stop-subjob
    (kbd "<tab>") '+evil-term-send-tab))

(provide 'evil-ansi-term)
;;; evil-ansi-term.el ends here
