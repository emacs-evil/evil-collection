;;; evil-collection-info.el --- Evil bindings for Info-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
    ;; This exists because <tab> is recognized as C-i on terminals.
    (kbd "g TAB") 'Info-next-reference
    "g]" 'Info-next-reference
    "g[" 'Info-prev-reference

    ;; From evil-integration.el.
    (kbd "M-h") 'Info-help              ; "h"
    (kbd "C-t") 'Info-history-back      ; "l"
    (kbd "C-o") 'Info-history-back
    " " 'Info-scroll-up
    (kbd "C-]") 'Info-follow-nearest-node
    (kbd "DEL") 'Info-scroll-down
    (kbd "C-i") 'Info-history-forward

    "d" 'Info-directory
    "u" 'Info-up
    "gL" 'Info-history ; "L"
    "S" 'Info-search-case-sensitively
    "i" 'Info-index
    "I" 'Info-virtual-index
    "a" 'info-apropos

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

    ;; goto
    "J" 'Info-menu

    "gG" 'Info-goto-node
    "gm" 'Info-menu
    "gt" 'Info-top-node
    "gT" 'Info-toc
    "g," 'Info-index-next)

  ;; yu, Like eww.
  (evil-collection-define-operator-key 'yank 'Info-mode-map
    "u" 'Info-copy-current-node-name)

  (evil-collection-bind 'Info-mode-map
                        'action 'Info-follow-nearest-node
                        'next-item 'Info-next
                        'prev-item 'Info-prev
                        'next-section 'Info-forward-node
                        'prev-section 'Info-backward-node
                        'find-file 'Info-follow-reference
                        'describe-mode 'Info-summary
                        'search-or-filter 'Info-search
                        'cycle-next 'Info-next-reference
                        'cycle-previous 'Info-prev-reference))

(provide 'evil-collection-info)
;;; evil-collection-info.el ends here
