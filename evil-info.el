;;; evil-info.el --- Add Evil bindings to Info

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Package-Requires: ((evil "1.2.3"))
;; Package-Version: 20170724.1223
;; Homepage: https://github.com/Ambrevar/evil-special-modes
;; Version: 0

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

;;; Commentary: The default bindings in motion state override the standard
;;; movement keys.  This package restores them.

;;; Code:

(require 'evil)
(require 'info)

;;;###autoload
(defun evil-info-setup ()
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
    "n" 'evil-search-next
    "?" 'evil-search-backward
    (kbd "<tab>") 'Info-next-reference
    (kbd "S-<tab>") 'Info-prev-reference

    ;; TODO: Should search with "n"/"N" cover the full manual like "C-s"/"C-r" does?

    ;; goto
    "gd" 'Info-goto-node
    "gt" 'Info-top-node
    "gT" 'Info-toc
    "gf" 'Info-follow-reference
    (kbd "C-o") 'Info-history-back
    (kbd "C-i") 'Info-history-forward
    ;; TODO: "[" and "]" are Emacs default for fine-grained browsing.
    ;; We usually use "C-j"/"C-k" for that.
    (kbd "C-j") 'Info-next
    (kbd "C-k") 'Info-prev

    (kbd "M-w") 'Info-copy-current-node-name ; TODO: Use yn?
    "p" nil

    ;; quit
    "q" 'Info-exit
    "ZQ" 'evil-quit
    "ZZ" 'Info-exit))

(provide 'evil-info)
;;; evil-info.el ends here
