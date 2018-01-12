;;; evil-collection-info.el --- Evil bindings for Info-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, info, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; The default bindings in motion state override the standard
;; movement keys.  This package restores them.

;;; Code:
(require 'evil)
(require 'info)

(defun evil-collection-info-setup ()
  "Set up `evil' bindings for `info-mode'."
  (evil-define-key 'motion Info-mode-map
    ;; motion: Restore some Evil keys that got overriden.
    "w" 'evil-forward-word-begin
    "e" 'evil-forward-word-end
    "ge" 'evil-backward-word-end
    "gE" 'evil-backward-WORD-end
    "b" 'evil-backward-word-begin
    "gg" 'evil-goto-first-line
    "t" 'evil-find-char-to
    "T" 'evil-find-char-to-backward
    "f" 'evil-find-char

    "?" evil-collection-evil-search-backward
    "/" evil-collection-evil-search-forward
    "n" evil-collection-evil-search-next
    "N" evil-collection-evil-search-previous

    (kbd "<tab>") 'Info-next-reference
    (kbd "S-<tab>") 'Info-prev-reference

    ;; TODO: Should search with "n"/"N" cover the full manual like "C-s"/"C-r" does?

    ;; goto
    "gd" 'Info-goto-node ; TODO: "gd" does not match the rationale of "go to definition". Change?
    "gt" 'Info-top-node
    "gT" 'Info-toc
    "gf" 'Info-follow-reference
    (kbd "C-o") 'Info-history-back
    (kbd "C-i") 'Info-history-forward
    ;; TODO: "[" and "]" are Emacs default for fine-grained browsing.
    ;; We usually use "C-j"/"C-k" for that.
    (kbd "C-j") 'Info-next
    (kbd "C-k") 'Info-prev
    "gj" 'Info-next
    "gk" 'Info-prev

    (kbd "M-w") 'Info-copy-current-node-name ; TODO: Use yn?
    "p" nil

    ;; quit
    "q" 'Info-exit
    "ZQ" 'evil-quit
    "ZZ" 'Info-exit))

(provide 'evil-collection-info)
;;; evil-collection-info.el ends here
