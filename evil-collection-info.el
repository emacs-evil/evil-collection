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
;; The default bindings in motion state override the standard movement keys.
;; This package uses normal state and redefines everything.

;;; Code:
(require 'evil)
(require 'evil-collection-evil-search)
(require 'info)

(defun evil-collection-info-setup ()
  "Set up `evil' bindings for `info-mode'."
  (evil-collection-inhibit-insert-state Info-mode-map)
  (evil-set-initial-state 'Info-mode 'normal)
  (evil-define-key 'normal Info-mode-map
    (kbd "<tab>") 'Info-next-reference
    (kbd "S-<tab>") 'Info-prev-reference

    ;; From evil-integration.el.
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    (kbd "M-h") 'Info-help              ; "h"
    (kbd "C-t") 'Info-history-back      ; "l"
    (kbd "C-o") 'Info-history-back
    " " 'Info-scroll-up
    (kbd "C-]") 'Info-follow-nearest-node
    (kbd "DEL") 'Info-scroll-down
    ;; Add "C-i" for consistency.
    (kbd "C-i") 'Info-history-forward

    "d" 'Info-directory
    "u" 'Info-up
    "L" 'Info-history
    "s" 'Info-search
    "S" 'Info-search-case-sensitively
    "i" 'Info-index
    "I" 'Info-virtual-index
    "a" 'info-apropos

    "gg" 'evil-goto-first-line

    ;; TODO: Restore digit arguments?  Use g[n] instead.

    ;; TODO: Should search with "n"/"N" cover the full manual like "C-s"/"C-r" does?
    ;; TODO: Directions?
    "n" 'isearch-repeat-forward
    "N" 'isearch-repeat-backward

    ;; goto
    "gd" 'Info-goto-node ; TODO: "gd" does not match the rationale of "go to definition". Change?
    "gm" 'Info-menu
    "gt" 'Info-top-node
    "gT" 'Info-toc
    "gf" 'Info-follow-reference
    ;; TODO: "[" and "]" are Emacs default for fine-grained browsing.
    ;; We usually use "C-j"/"C-k" for that.
    (kbd "C-j") 'Info-next
    (kbd "C-k") 'Info-prev
    "gj" 'Info-next
    "gk" 'Info-prev

    (kbd "M-w") 'Info-copy-current-node-name ; TODO: Use yn?

    ;; quit
    "q" 'Info-exit
    "ZQ" 'evil-quit
    "ZZ" 'Info-exit))

(provide 'evil-collection-info)
;;; evil-collection-info.el ends here
