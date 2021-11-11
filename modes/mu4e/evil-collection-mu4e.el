;;; evil-collection-mu4e.el --- Evil bindings for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Joris Engbers
;; Copyright (C) 2018 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.9
;; Package-Requires: ((emacs "24.4") (evil "1.2.10"))
;; Keywords: evil, mu4e, tools

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
;; Evil keybindings for mu4e that make sense for Evil users.  The following
;; keybindings are defined:
;;
;; General commands:
;; | Commmand                 | evil-mu4e | Alternative |
;; |--------------------------+-----------+-------------|
;; | Jump to maildir          | J         |             |
;; | Update                   | u         |             |
;; | Compose message          | cc        | C           |
;; | Kill update mail process | x         |             |

;; Commands for header-mode and view-mode:
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
(require 'mu4e nil t)

(declare-function mu4e~main-action-str "mu4e-main")
(declare-function mu4e~main-view-queue "mu4e-main")
(defvar smtpmail-send-queued-mail)
(defvar smtpmail-queue-dir)

(defconst evil-collection-mu4e-maps '(mu4e-main-mode-map
                                      mu4e-headers-mode-map
                                      mu4e-view-mode-map
                                      mu4e-compose-mode-map))



(defun evil-collection-mu4e-set-state ()
  "Set the appropriate initial state of all mu4e modes."
  (dolist (mode '(mu4e-main-mode
                  mu4e-headers-mode
                  mu4e-view-mode
                  mu4e-org-mode))
    (evil-set-initial-state mode 'normal))
  (evil-set-initial-state 'mu4e-compose-mode 'insert))

;; When using org-mu4e, the above leads to an annoying behaviour, because
;; switching from message body to header activates mu4e-compose-mode, thus
;; putting the user into insert-state. The below code, together with the hooks
;; set in evil-collection-mu4e-setup fixes this issue.
(defun evil-collection-mu4e-org-set-header-to-normal-mode ()
  (evil-set-initial-state 'mu4e-compose-mode 'normal))

(defun evil-collection-mu4e-org-set-header-to-insert-mode ()
  (evil-set-initial-state 'mu4e-compose-mode 'insert))


;;; Define bindings

;; TODO: Inhibit insert-state functions as per Evil Collection.
(defvar evil-collection-mu4e-mode-map-bindings
  `((mu4e-main-mode-map
     "J" mu4e~headers-jump-to-maildir
     "j" next-line
     "k" previous-line
     "u" mu4e-update-mail-and-index
     "gr" revert-buffer
     "b" mu4e-headers-search-bookmark
     "B" mu4e-headers-search-bookmark-edit
     "N" mu4e-news
     ";" mu4e-context-switch
     "H" mu4e-display-manual
     "C" mu4e-compose-new
     "cc" mu4e-compose-new
     "x" mu4e-kill-update-mail
     "A" mu4e-about
     "f" smtpmail-send-queued-mail
     "m" mu4e~main-toggle-mail-sending-mode
     "s" mu4e-headers-search
     "q" mu4e-quit)

    (mu4e-headers-mode-map
     "q" mu4e~headers-quit-buffer
     "J" mu4e~headers-jump-to-maildir
     "C" mu4e-compose-new
     "E" mu4e-compose-edit
     "F" mu4e-compose-forward
     "R" mu4e-compose-reply
     "cc" mu4e-compose-new
     "ce" mu4e-compose-edit
     "cf" mu4e-compose-forward
     "cr" mu4e-compose-reply
     "o" mu4e-headers-change-sorting
     "j" mu4e-headers-next
     "k" mu4e-headers-prev
     "gr" mu4e-headers-rerun-search
     "b" mu4e-headers-search-bookmark
     "B" mu4e-headers-search-bookmark-edit
     ";" mu4e-context-switch
     ,(kbd "RET") mu4e-headers-view-message
     "/" mu4e-headers-search-narrow
     "s" mu4e-headers-search
     "S" mu4e-headers-search-edit
     "x" mu4e-mark-execute-all
     "a" mu4e-headers-action
     "*" mu4e-headers-mark-for-something ; TODO: Don't override evil-seach-word-forward?
     "&" mu4e-headers-mark-custom
     "A" mu4e-headers-mark-for-action
     "m" mu4e-headers-mark-for-move
     "r" mu4e-headers-mark-for-refile
     "D" mu4e-headers-mark-for-delete
     "d" mu4e-headers-mark-for-trash
     "=" mu4e-headers-mark-for-untrash
     "u" mu4e-headers-mark-for-unmark
     "U" mu4e-mark-unmark-all
     "?" mu4e-headers-mark-for-unread
     "!" mu4e-headers-mark-for-read
     "%" mu4e-headers-mark-pattern
     "+" mu4e-headers-mark-for-flag
     "-" mu4e-headers-mark-for-unflag
     "[[" mu4e-headers-prev-unread
     "]]" mu4e-headers-next-unread
     "gk" mu4e-headers-prev-unread
     "gj" mu4e-headers-next-unread
     "\C-j" mu4e-headers-next
     "\C-k" mu4e-headers-prev
     "zr" mu4e-headers-toggle-include-related
     "zt" mu4e-headers-toggle-threading
     "zd" mu4e-headers-toggle-skip-duplicates
     "gl" mu4e-show-log
     "gv" mu4e-select-other-view
     "T" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read))))

    (mu4e-compose-mode-map
     "gg" mu4e-compose-goto-top
     "G" mu4e-compose-goto-bottom
     "ZD" message-dont-send
     "ZF" mml-attach-file
     "ZQ" mu4e-message-kill-buffer
     "ZZ" message-send-and-exit)

    (mu4e-view-mode-map
     " " mu4e-view-scroll-up-or-next
     [tab] shr-next-link
     [backtab] shr-previous-link
     "q" mu4e~view-quit-buffer
     "gx" mu4e-view-go-to-url
     "gX" mu4e-view-fetch-url
     "C" mu4e-compose-new
     "H" mu4e-view-toggle-html
     ;; "E"               mu4e-compose-edit
     ;; "F"               mu4e-compose-forward
     "R" mu4e-compose-reply
     "cc" mu4e-compose-new
     "ce" mu4e-compose-edit
     "cf" mu4e-compose-forward
     "cr" mu4e-compose-reply
     "p" mu4e-view-save-attachments
     "P" mu4e-view-save-attachment-multi ; Since mu4e 1.0, -multi is same as normal.
     "O" mu4e-headers-change-sorting
     "A" mu4e-view-mime-part-action ; Since 1.6, uses gnus view by default
     "a" mu4e-view-action
     "J" mu4e~headers-jump-to-maildir
     "[[" mu4e-view-headers-prev-unread
     "]]" mu4e-view-headers-next-unread
     "gk" mu4e-view-headers-prev-unread
     "gj" mu4e-view-headers-next-unread
     "\C-j" mu4e-view-headers-next
     "\C-k" mu4e-view-headers-prev
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
     "za" mu4e-view-toggle-hide-cited
     "gl" mu4e-show-log
     "s" mu4e-view-search-edit
     "|" mu4e-view-pipe
     "." mu4e-view-raw-message
     ,(kbd "C--") mu4e-headers-split-view-shrink
     ,(kbd "C-+") mu4e-headers-split-view-grow
     "T" (lambda ()
           (interactive)
           (mu4e-headers-mark-thread nil '(read)))
     ,@(when evil-want-C-u-scroll
         '("\C-u" evil-scroll-up))))
  ;; TODO: Add mu4e-headers-search-bookmark?
  "All evil-mu4e bindings.")

(defun evil-collection-mu4e-set-bindings ()
  "Set the bindings."
  ;; WARNING: With lexical binding, lambdas from `mapc' and `dolist' become
  ;; closures in which we must use `evil-define-key*' instead of
  ;; `evil-define-key'.
  (dolist (binding evil-collection-mu4e-mode-map-bindings)
    (apply #'evil-collection-define-key 'normal binding))
  (evil-collection-define-key 'visual 'mu4e-compose-mode-map
    "gg" 'mu4e-compose-goto-top
    "G" 'mu4e-compose-goto-bottom)
  (evil-set-command-property 'mu4e-compose-goto-bottom :keep-visual t)
  (evil-set-command-property 'mu4e-compose-goto-top :keep-visual t)

  ;; yu
  (evil-collection-define-operator-key 'yank 'mu4e-view-mode-map
    "u" 'mu4e-view-save-url))


;;; Update mu4e-main-view
;;; To avoid confusion the main-view is updated to show the keys that are in use
;;; for evil-mu4e.

(defvar evil-collection-mu4e-begin-region-basic "\n  Basics"
  "The place where to start overriding Basic section.")

(defvar evil-collection-mu4e-end-region-basic "a new message\n"
  "The place where to end overriding Basic section.")

(defvar evil-collection-mu4e-new-region-basic
  (concat (mu4e~main-action-str "\t* [J]ump to some maildir\n" 'mu4e-jump-to-maildir)
          (mu4e~main-action-str "\t* enter a [s]earch query\n" 'mu4e-search)
          (mu4e~main-action-str "\t* [C]ompose a new message\n" 'mu4e-compose-new))
  "Define the evil-mu4e Basic region.")

(defvar evil-collection-mu4e-begin-region-misc "\n  Misc"
  "The place where to start overriding Misc section.")

(defvar evil-collection-mu4e-end-region-misc "q]uit"
  "The place where to end overriding Misc section.")

(defun evil-collection-mu4e-new-region-misc ()
  "Define the evil-mu4e Misc region."
  (concat
   (mu4e~main-action-str "\t* [;]Switch focus\n" 'mu4e-context-switch)
   (mu4e~main-action-str "\t* [u]pdate email & database (Alternatively: gr)\n"
                         'mu4e-update-mail-and-index)

   ;; show the queue functions if `smtpmail-queue-dir' is defined
   (if (file-directory-p smtpmail-queue-dir)
       (mu4e~main-view-queue)
     "")
   "\n"

   (mu4e~main-action-str "\t* [N]ews\n" 'mu4e-news)
   (mu4e~main-action-str "\t* [A]bout mu4e\n" 'mu4e-about)
   (mu4e~main-action-str "\t* [H]elp\n" 'mu4e-display-manual)
   (mu4e~main-action-str "\t* [q]uit\n" 'mu4e-quit)))

(defun evil-collection-mu4e-replace-region (new-region start end)
  "Replace region between START and END with NEW-REGION.
START end END end are regular expressions."
  ;; move to start of region
  (goto-char (point-min))
  (re-search-forward start)

  ;; insert new headings
  (insert "\n\n")
  (insert new-region)
  ;; Delete text until end of region.
  (let ((start-point (point))
        (end-point (re-search-forward end)))
    (delete-region start-point end-point)))

(defun evil-collection-mu4e-update-main-view ()
  "Update 'Basic' and 'Misc' regions to reflect the new
keybindings."
  (evil-collection-mu4e-replace-region evil-collection-mu4e-new-region-basic
                                       evil-collection-mu4e-begin-region-basic
                                       evil-collection-mu4e-end-region-basic)
  (evil-collection-mu4e-replace-region (evil-collection-mu4e-new-region-misc)
                                       evil-collection-mu4e-begin-region-misc
                                       evil-collection-mu4e-end-region-misc))


;;; Initialize evil-collection-mu4e

;;;###autoload
(defun evil-collection-mu4e-setup ()
  "Initialize evil-mu4e if necessary.
If mu4e-main-mode is in evil-state-motion-modes, initialization
is already done earlier."
    (evil-collection-mu4e-set-state)
    (evil-collection-mu4e-set-bindings)
    (add-hook 'mu4e-main-mode-hook 'evil-collection-mu4e-update-main-view)
    (add-hook 'org-mode-hook #'evil-collection-mu4e-org-set-header-to-normal-mode)
    (add-hook 'mu4e-compose-pre-hook #'evil-collection-mu4e-org-set-header-to-insert-mode))

(provide 'evil-collection-mu4e)
;;; evil-collection-mu4e.el ends here
