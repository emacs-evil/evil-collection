;;; evil-collection-mu4e-views.el --- Evil bindings for mu4e-views -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Joris Engbers
;; Copyright (C) 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Boris Glavic <lordpretzel@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.9
;; Package-Requires: ((emacs "24.4") (evil "1.2.10"))
;; Keywords: evil, mu4e-views, tools

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
;; Evil keybindings for mu4e-views that make sense for Evil users.  The following
;; keybindings are defined:
;;
;; General commands:
;; | Commmand                 | evil-mu4e | Alternative |
;; |--------------------------+-----------+-------------|
;; | Jump to maildir          | J         |             |
;; | Update                   | u         |             |
;; | Compose message          | cc        | C           |
;; | Kill update mail process | x         |             |

;; Commands for mu4e-headers view-mode:
;; | Command                         | evil-mu4e | Alternative |
;; |---------------------------------+-----------+-------------|
;; | Select viewing method           | v         |             |


;; Commands for mu4e-views view-mode (mu4e-views-view-actions-mode-map):
;; | Command                         | evil-mu4e | Alternative |
;; |---------------------------------+-----------+-------------|
;; | Next message                    | C-j       |             |
;; | Previous message                | C-k       |             |
;; | Mark the current thread as read | T         |             |
;; | Compose message                 | cc        | C           |
;; | Compose edit**                  | ce        | E           |
;; | Compose forward**               | cf        | F           |
;; | Compose reply                   | cr        | R           |
;; | Change sorting***               | o         | O           |
;; | Rerun search                    | gr        |             |
;; | Toggle include related          | zr        |             |
;; | Toggle threading                | zt        |             |
;; | Toggle hide cited               | za        |             |
;; | Skip duplicates                 | zd        |             |
;; | Show log                        | gl        |             |
;; | Select other view               | gv        |             |
;; | Save attachement(s)             | p         | P           |
;; | Save url                        | yu        |             |
;; | Go to url                       | gx        |             |
;; | Fetch url                       | gX        |             |
;;
;;  - * denotes only in header-mode
;;  - ** denotes Alternative only in header-mode
;;  - *** denotes Alternative only in view-mode
;;
;;; Code:

(require 'evil-collection)
(require 'mu4e-views nil t)
(require 'xwidget)

(defconst evil-collection-mu4e-views-maps
  '(mu4e-views-view-actions-mode-map))

;; only have a minor mode
;; (defun evil-collection-mu4e-views-set-state ()
;;   "Set the appropriate initial state of all mu4e modes."
;;   (dolist (mode '(mu4e-main-mode
;;                   mu4e-headers-mode
;;                   mu4e-view-mode
;;                   mu4e-org-mode))
;;     (evil-set-initial-state mode 'normal))
;;   (evil-set-initial-state 'mu4e-compose-mode 'insert))

;;; Define bindings

;; TODO: Inhibit insert-state functions as per Evil Collection.
(defvar evil-collection-mu4e-views-mode-map-bindings
  `((mu4e-headers-mode-map
     "v" mu4e-views-mu4e-select-view-msg-method
     ,(kbd "M-b") mu4e-views-cursor-msg-view-window-up
     ,(kbd "M-f") mu4e-views-cursor-msg-view-window-down)

    (mu4e-views-view-actions-mode-map
     " " mu4e-view-scroll-up-or-next
     [tab] shr-next-link
     [backtab] shr-previous-link
     "q" mu4e-views-mu4e-headers-windows-only
     "gx" mu4e-views-mu4e-view-go-to-url
     "gX" mu4e-views-mu4e-view-fetch-url
     "C" mu4e-compose-new
     ;; "H" mu4e-view-toggle-html
     ;; "E"               mu4e-compose-edit
     ;; "F"               mu4e-compose-forward
     "R" mu4e-compose-reply
     "cc" mu4e-compose-new
     "ce" mu4e-compose-edit
     "cf" mu4e-compose-forward
     "cr" mu4e-compose-reply
     "p" mu4e-views-mu4e-view-save-attachment
     ;; "P" mu4e-view-save-attachment-multi ; Since mu4e 1.0, -multi is same as normal.
     "O" mu4e-headers-change-sorting
     "o" mu4e-views-mu4e-view-open-attachment
     ;; "A" mu4e-view-attachment-action
     "a" mu4e-views-mu4e-view-action
     "J" mu4e~headers-jump-to-maildir
     ;; "[[" mu4e-view-headers-prev-unread
     ;; "]]" mu4e-view-headers-next-unread
     ;; "gk" mu4e-view-headers-prev-unread
     ;; "gj" mu4e-view-headers-next-unread
     "\C-j" mu4e-views-mu4e-headers-next
     "\C-k" mu4e-views-mu4e-headers-prev
     "x" mu4e-view-marked-execute
     "&" mu4e-view-mark-custom
     "*" mu4e-view-mark-for-something   ; TODO: Don't override "*".
     "m" mu4e-view-mark-for-move
     "r" mu4e-view-mark-for-refile
     "D" mu4e-view-mark-for-delete
     "d" mu4e-view-mark-for-trash
     "=" mu4e-view-mark-for-untrash
     "u" mu4e-view-unmark
     "U" mu4e-view-unmark-all
     "?" mu4e-view-mark-for-unread
     "!" mu4e-view-mark-for-read
     "%" mu4e-view-mark-pattern
     "+" mu4e-view-mark-for-flag
     "-" mu4e-view-mark-for-unflag
     "zr" mu4e-headers-toggle-include-related
     "zt" mu4e-headers-toggle-threading
     ;; "za" mu4e-view-toggle-hide-cited
     "gl" mu4e-show-log
     "s" mu4e-view-search-edit
     ;; "|" mu4e-view-pipe
     ;; "." mu4e-view-raw-message
     ,(kbd "C--") mu4e-headers-split-view-shrink
     ,(kbd "C-+") mu4e-headers-split-view-grow
     "T" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read)))
     ,@(when evil-want-C-u-scroll
         '("\C-u" evil-scroll-up))))
  "All evil-mu4e-views bindings.")

(defun evil-collection-mu4e-views-set-bindings ()
  "Set the bindings."
  ;; WARNING: With lexical binding, lambdas from `mapc' and `dolist' become
  ;; closures in which we must use `evil-define-key*' instead of
  ;; `evil-define-key'.
  (dolist (binding evil-collection-mu4e-views-mode-map-bindings)
    (apply #'evil-collection-define-key 'normal binding))
  ;; (evil-collection-define-key 'operator 'mu4e-view-mode-map
  ;;   "u" '(menu-item
  ;;         ""
  ;;         nil
  ;;         :filter (lambda (&optional _)
  ;;                   (when (memq evil-this-operator
  ;;                               '(evil-yank evil-cp-yank evil-sp-yank lispyville-yank))
  ;;                     (setq evil-inhibit-operator t)
  ;;                     #'mu4e-views-mu4e-view-save-url))))
  )

;;; Initialize evil-collection-mu4e-views

;;;###autoload
(defun evil-collection-mu4e-views-setup ()
  "Initialize evil-mu4e-views if necessary.
If mu4e-main-mode is in evil-state-motion-modes, initialization
is already done earlier."
;;    (evil-collection-mu4e-views-set-state)
    (evil-collection-mu4e-views-set-bindings))

(provide 'evil-collection-mu4e-views)
;;; evil-collection-mu4e-views.el ends here
