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

;;;###autoload
(defun evil-collection-info-setup ()
  "Set up `evil' bindings for `info-mode'."
  (evil-collection-set-readonly-bindings 'Info-mode-map)
  (evil-set-initial-state 'Info-mode 'normal)
  (evil-collection-define-key 'normal 'Info-mode-map
    "l" 'evil-forward-char
    "h" 'evil-backward-char
    (kbd "<tab>") 'Info-next-reference
    (kbd "S-<tab>") 'Info-prev-reference
    ;; This exists because <tab> is recognized as C-i on terminals.
    (kbd "g TAB") 'Info-next-reference
    "g]" 'Info-next-reference
    "g[" 'Info-prev-reference

    ;; From evil-integration.el.
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    (kbd "M-h") 'Info-help              ; "h"
    (kbd "C-t") 'Info-history-back      ; "l"
    (kbd "C-o") 'Info-history-back
    " " 'Info-scroll-up
    (kbd "C-]") 'Info-follow-nearest-node
    (kbd "DEL") 'Info-scroll-down
    (kbd "C-i") 'Info-history-forward

    "d" 'Info-directory
    "u" 'Info-up
    "L" 'Info-history
    "s" 'Info-search
    "S" 'Info-search-case-sensitively
    "i" 'Info-index
    "I" 'Info-virtual-index
    "a" 'info-apropos
    "w" 'evil-forward-word-begin
    "b" 'evil-backward-word-begin
    "e" 'evil-forward-word-end

    "gg" 'evil-goto-first-line

    ;; mouse integration
    [mouse-2]     'Info-mouse-follow-nearest-node
    [follow-link] 'mouse-face
    ;; make mac user happy
    [double-wheel-left]  'Info-history-back
    [double-wheel-right] 'Info-history-forward

    ;; digit arguments
    "g1" 'Info-nth-menu-item
    "g2" 'Info-nth-menu-item
    "g3" 'Info-nth-menu-item
    "g4" 'Info-nth-menu-item
    "g5" 'Info-nth-menu-item
    "g6" 'Info-nth-menu-item
    "g7" 'Info-nth-menu-item
    "g8" 'Info-nth-menu-item
    "g9" 'Info-nth-menu-item

    ;; NOTE: We do NOT search the entire buffer, only the narrowed buffer.
    "n" evil-collection-evil-search-next
    "N" evil-collection-evil-search-previous

    ;; goto
    "gd" 'Info-goto-node ; TODO: "gd" does not match the rationale of "go to definition". Change?
    "gm" 'Info-menu
    "m" 'evil-set-marker                ; Else this would be `Info-menu'.
    "gt" 'Info-top-node
    "t" 'evil-find-char-to              ; Else this would be `Info-top-node'.
    "gT" 'Info-toc
    "T" 'evil-find-char-to-backward     ; Else this would be `Info-toc'.
    "gf" 'Info-follow-reference
    "f" 'evil-find-char                 ; Else this would be `Info-follow-reference'.
    ;; TODO: "[" and "]" are Emacs default for fine-grained browsing.
    ;; We usually use "C-j"/"C-k" for that.
    (kbd "C-j") 'Info-forward-node
    (kbd "C-k") 'Info-backward-node
    "gj" 'Info-next
    "gk" 'Info-prev

    "g?" 'Info-summary
    "?" evil-collection-evil-search-backward)  ; Else this would be `Info-summary'.

  (evil-collection-define-key 'visual 'Info-mode-map
    "l" 'evil-forward-char
    "h" 'evil-backward-char

    "w" 'evil-forward-word-begin
    "b" 'evil-backward-word-begin
    "e" 'evil-forward-word-end

    "f" 'evil-find-char
    "t" 'evil-find-char-to
    "T" 'evil-find-char-to-backward

    "0" 'evil-digit-argument-or-evil-beginning-of-line
    "gg" 'evil-goto-first-line)

  ;; yu, Like eww.
  (evil-collection-define-operator-key 'yank 'Info-mode-map
    "u" 'Info-copy-current-node-name))

(provide 'evil-collection-info)
;;; evil-collection-info.el ends here
