;;; evil-collection-info.el --- Evil bindings for Info-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
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
(require 'evil-collection)
(require 'info)

(defconst evil-collection-info-maps '(Info-mode-map))

(defun evil-collection-info-setup ()
  "Set up `evil' bindings for `info-mode'."
  (evil-collection-inhibit-insert-state 'Info-mode-map)
  (evil-set-initial-state 'Info-mode 'normal)
  (evil-collection-define-key 'normal 'Info-mode-map
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
    "m" 'Info-menu
    "w" 'evil-forward-word-begin
    "b" 'evil-backward-word-begin

    "gg" 'evil-goto-first-line

    ;; TODO: Restore digit arguments?  Use g[n] instead.

    ;; TODO: Should search with "n"/"N" cover the full manual like "C-s"/"C-r" does?
    ;; TODO: Directions?
    "n" (if (evil-collection-evil-search-enabled)
            evil-collection-evil-search-next
          'isearch-repeat-forward)
    "N" (if (evil-collection-evil-search-enabled)
            evil-collection-evil-search-previous
          'isearch-repeat-backward)

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


    ;; quit
    "q" 'Info-exit
    "ZQ" 'evil-quit
    "ZZ" 'Info-exit)

  (evil-collection-define-key 'operator 'Info-mode-map
    "u" '(menu-item                     ; Like eww.
          ""
          nil
          :filter (lambda (&optional _)
                    (when (memq evil-this-operator
                                evil-collection-yank-operators)
                      (setq evil-inhibit-operator t)
                      #'Info-copy-current-node-name)))))

(provide 'evil-collection-info)
;;; evil-collection-info.el ends here
