;;; evil-collection-gnus.el --- Bindings for `gnus' -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, tools, evil

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
;;; Bindings for `gnus'.

;;; Code:
(require 'gnus nil t)
(require 'evil-collection)

(defconst evil-collection-gnus-maps '(gnus-article-mode-map
                                      gnus-bookmark-bmenu-mode-map
                                      gnus-browse-mode-map
                                      gnus-group-mode-map
                                      gnus-server-mode-map
                                      gnus-summary-mode-map))

;;;###autoload
(defun evil-collection-gnus-setup ()
  "Set up `evil' bindings for `gnus'."
  (evil-set-initial-state 'gnus-summary-mode 'normal)
  (evil-collection-define-key 'normal 'gnus-summary-mode-map
    ;; motion
    (kbd "<tab>") 'gnus-summary-widget-forward
    (kbd "<backtab>") 'gnus-summary-widget-backward
    (kbd "<delete>") 'gnus-summary-prev-page
    (kbd "S-SPC") 'gnus-summary-prev-page
    (kbd "SPC") 'gnus-summary-next-page
    (kbd "RET") 'gnus-summary-scroll-up
    "]]" 'gnus-summary-next-unread-article
    "[[" 'gnus-summary-prev-unread-article
    (kbd "C-j") 'gnus-summary-next-article
    (kbd "C-k") 'gnus-summary-prev-article


    ;; Marking
    "m" 'gnus-summary-mark-as-processable
    "M" 'gnus-summary-put-mark-as-read
    "u" 'gnus-summary-clear-mark-forward
    "U" 'gnus-summary-clear-mark-backward

    "!" 'gnus-summary-execute-command
    "|" 'gnus-summary-pipe-output

    "gu" 'gnus-summary-first-unread-article
    "gU" 'gnus-summary-best-unread-article

    "^" 'gnus-summary-refer-parent-article
    (kbd "M-^") 'gnus-summary-refer-article

    "zz" 'gnus-recenter
    "z/" 'gnus-summary-limit-map
    "zd" 'gnus-summary-mark-as-dormant
    "zt" 'gnus-summary-toggle-header
    "x" 'gnus-summary-limit-to-unread

    "J" 'gnus-summary-goto-article

    "r" 'gnus-summary-reply
    "R" 'gnus-summary-reply-with-original
    ;; TODO: Should it be very-wide?
    ;; "r" 'gnus-summary-very-wide-reply
    ;; "R" 'gnus-summary-very-wide-reply-with-original

    "gO" 'gnus-summary-save-map
    "gS" 'gnus-summary-send-map
    "gT" 'gnus-summary-thread-map
    "gV" 'gnus-summary-score-map
    "gW" 'gnus-summary-wash-map
    "X" 'gnus-uu-extract-map
    "gY" 'gnus-summary-buffer-map
    "gZ" 'gnus-summary-exit-map

    ;; filter
    "s" 'gnus-summary-isearch-article

    ;; search
    (kbd "M-s") 'gnus-summary-search-article-forward
    (kbd "M-r") 'gnus-summary-search-article-backward
    (kbd "M-S") 'gnus-summary-repeat-search-article-forward
    (kbd "M-R") 'gnus-summary-repeat-search-article-backward

    ;; sort
    "oa" 'gnus-summary-sort-by-author
    "oc" 'gnus-summary-sort-by-chars
    "od" 'gnus-summary-sort-by-date
    "oi" 'gnus-summary-sort-by-score
    "ol" 'gnus-summary-sort-by-lines
    "omd" 'gnus-summary-sort-by-most-recent-date
    "omm" 'gnus-summary-sort-by-marks
    "omn" 'gnus-summary-sort-by-most-recent-number
    "on" 'gnus-summary-sort-by-number
    "oo" 'gnus-summary-sort-by-original
    "or" 'gnus-summary-sort-by-random
    "os" 'gnus-summary-sort-by-subject
    "ot" 'gnus-summary-sort-by-recipient

    [mouse-2] 'gnus-mouse-pick-article
    [follow-link] 'mouse-face

    "gr" 'gnus-summary-rescan-group

    ;; Rest of the bindings "as is".
    "d" 'gnus-summary-mark-as-read-forward
    "D" 'gnus-summary-mark-as-read-backward
    "E" 'gnus-summary-mark-as-expirable
    (kbd "M-u") 'gnus-summary-clear-mark-forward
    (kbd "M-U") 'gnus-summary-clear-mark-backward
    (kbd "M-C-k") 'gnus-summary-kill-thread
    (kbd "M-C-l") 'gnus-summary-lower-thread
    "e" 'gnus-summary-edit-article
    (kbd "M-C-t") 'gnus-summary-toggle-threads
    "zs" 'gnus-summary-show-thread
    "zh" 'gnus-summary-hide-thread
    (kbd "M-C-f") 'gnus-summary-next-thread
    (kbd "M-C-b") 'gnus-summary-prev-thread
    (kbd "<M-down>") 'gnus-summary-next-thread
    (kbd "<M-up>") 'gnus-summary-prev-thread
    (kbd "M-C-u") 'gnus-summary-up-thread
    (kbd "M-C-d") 'gnus-summary-down-thread
    "c" 'gnus-summary-catchup-and-exit
    (kbd "C-t") 'toggle-truncate-lines
    (kbd "C-c M-C-s") 'gnus-summary-limit-include-expunged
    "=" 'gnus-summary-expand-window
    (kbd "C-x C-s") 'gnus-summary-reselect-current-group
    (kbd "C-c C-r") 'gnus-summary-caesar-message
    "f" 'gnus-summary-followup
    "F" 'gnus-summary-followup-with-original
    "C" 'gnus-summary-cancel-article
    (kbd "C-c C-f") 'gnus-summary-mail-forward
    ".s" 'gnus-summary-save-article     ; Like notmuch?
    (kbd "C-o") 'gnus-summary-save-article-mail
    (kbd "M-k") 'gnus-summary-edit-local-kill
    (kbd "M-K") 'gnus-summary-edit-global-kill
    ;; "V" 'gnus-version
    (kbd "C-c C-d") 'gnus-summary-describe-group
    "zm" 'gnus-summary-mail-other-window
    "a" 'gnus-summary-post-news
    ;; "g" 'gnus-summary-show-article
    "gG" 'gnus-summary-goto-last-article
    (kbd "C-c C-v C-v") 'gnus-uu-decode-uu-view
    ;; "\C-d" 'gnus-summary-enter-digest-group
    ;; "\M-\C-d" 'gnus-summary-read-document
    (kbd "M-C-e") 'gnus-summary-edit-parameters
    (kbd "M-C-a") 'gnus-summary-customize-parameters
    (kbd "C-c C-b") 'gnus-bug
    "*" 'gnus-cache-enter-article
    (kbd "M-*") 'gnus-cache-remove-article
    (kbd "M-&") 'gnus-summary-universal-argument
    (kbd "M-i") 'gnus-symbolic-argument
    "I" 'gnus-summary-increase-score
    "L" 'gnus-summary-lower-score
    ;; "h" 'gnus-summary-select-article-buffer

    "K" 'gnus-info-find-node
    "zv" 'gnus-article-view-part
    (kbd "M-t") 'gnus-summary-toggle-display-buttonized

    ;; quit
    "Q" 'gnus-summary-exit-no-update
    "q" 'gnus-summary-exit
    "ZQ" 'gnus-summary-exit-no-update
    "ZZ" 'gnus-summary-exit)

  (evil-set-initial-state 'gnus-article-mode 'normal)
  (evil-collection-define-key 'motion 'gnus-article-mode-map
    "F"         'gnus-article-followup-with-original
    "R"         'gnus-article-reply-with-original
    "W"         'gnus-article-wide-reply-with-original)
  (evil-collection-define-key 'normal 'gnus-article-mode-map
    ;; quit
    "q"         'evil-window-delete
    "ZQ"        'evil-window-delete
    "ZZ"        'evil-window-delete

    ;; Movement
    (kbd "TAB") 'forward-button
    (kbd "<backtab>") 'backward-button
    (kbd "SPC") 'gnus-article-goto-next-page
    (kbd "DEL") 'gnus-article-goto-prev-page
    (kbd "S-SPC") 'gnus-article-goto-prev-page

    ;; Reply
    "r"         'gnus-summary-reply

    ;; Composing
    "C"         'gnus-article-mail
    "cc"        'gnus-article-mail

    ;; Actions
    (kbd "C-]") 'gnus-article-refer-article
    "s"         'gnus-article-show-summary
    "E"         'gnus-article-read-summary-keys
    (kbd "C-c C-f") 'gnus-summary-mail-forward)

  (evil-set-initial-state 'gnus-group-mode 'normal)
  (evil-collection-define-key 'normal 'gnus-group-mode-map
    ;; quit
    "q"         'gnus-group-exit
    "ZZ"        'gnus-group-exit
    "ZQ"        'gnus-group-quit

    ;; Movement
    "k"         'gnus-group-prev-group
    "j"         'gnus-group-next-group
    "[["        'gnus-group-prev-unread-group
    "]]"        'gnus-group-next-unread-group
    "gk"        'gnus-group-prev-unread-group
    "gj"        'gnus-group-next-unread-group

    ;; Composing, like mu4e
    "C"         'gnus-group-mail
    "cc"        'gnus-group-mail
    "ci"        'gnus-group-news

    ;; Actions
    "."         'gnus-group-first-unread-group
    "A"         'gnus-activate-all-groups
    "B"         'gnus-group-browse-foreign-server
    "E"         'gnus-group-edit-group
    "F"         'gnus-group-find-new-groups
    "J"         'gnus-group-jump-to-group
    "R"         'gnus-group-rename-group
    "X"         'gnus-group-expunge-group
    (kbd "RET") 'gnus-group-select-group
    (kbd "SPC") 'gnus-group-read-group
    "gr"        'gnus-group-get-new-news-this-group
    "gR"        'gnus-group-get-new-news
    "gu"        'gnus-group-unsubscribe-current-group
    "gU"        'gnus-group-unsubscribe-group
    "gc"        'gnus-group-catchup-current
    "gC"        'gnus-group-catchup-current-all
    "ge"        'gnus-group-expire-articles
    "gE"        'gnus-group-expire-all-groups

    ;; Deleting & Pasting
    "dd"        'gnus-group-kill-group
    "D"         'gnus-group-kill-group
    "p"         'gnus-group-yank-group
    "P"         'gnus-group-yank-group

    ;; Marking
    "m"         'gnus-group-mark-group
    "u"         'gnus-group-unmark-group
    "U"         'gnus-group-unmark-all-groups
    "M"         'gnus-group-mark-buffer
    "*"         'gnus-group-mark-buffer
    "%"         'gnus-group-mark-regexp

    ;; Searching
    "s"         'gnus-group-apropos
    "S"         'gnus-group-description-apropos

    ;; Sorting
    "oa"        'gnus-group-sort-groups-by-alphabet
    "ol"        'gnus-group-sort-groups-by-level
    "om"        'gnus-group-sort-groups-by-method
    "on"        'gnus-group-sort-groups-by-real-name
    "or"        'gnus-group-sort-groups-by-rank
    "os"        'gnus-group-sort-groups
    "ou"        'gnus-group-sort-groups-by-unread
    "ov"        'gnus-group-sort-groups-by-score

    ;; Listing
    "L!"        'gnus-group-list-ticked
    "L/"        'gnus-group-list-limit-map
    "L?"        'gnus-group-list-dormant
    "La"        'gnus-group-list-active        ;; was A A
    "Lc"        'gnus-group-list-cached
    "Lf"        'gnus-group-list-flush-map
    "Lk"        'gnus-group-list-killed
    "Ll"        'gnus-group-list-level
    "Lm"        'gnus-group-list-matching
    "LM"        'gnus-group-list-all-matching
    "Lp"        'gnus-group-list-plus-map
    "Ls"        'gnus-group-list-groups
    "Lu"        'gnus-group-list-all-groups
    "Lz"        'gnus-group-list-zombies

    "^"         'gnus-group-enter-server-mode

    (kbd "DEL") 'gnus-group-prev-unread-group
    [mouse-2]   'gnus-mouse-pick-group
    "g?"        'gnus-group-help-map)

  (evil-set-initial-state 'gnus-server-mode 'normal)
  (evil-collection-define-key 'normal 'gnus-server-mode-map
    (kbd "RET") 'gnus-server-read-server
    (kbd "SPC") 'gnus-server-read-server-in-server-buffer
    "C"         'gnus-server-close-server
    "D"         'gnus-server-deny-server
    "G"         'gnus-group-make-nnir-group
    "I"         'gnus-server-set-cloud-method-server
    "L"         'gnus-server-offline-server
    "O"         'gnus-server-open-server
    "R"         'gnus-server-remove-denials
    "S"         'gnus-server-show-server
    "a"         'gnus-server-add-server
    "y"         'gnus-server-copy-server
    "e"         'gnus-server-edit-server
    "gr"        'gnus-server-regenerate-server
    "g?"        'describe-mode
    "i"         'gnus-server-toggle-cloud-server
    "d"         'gnus-server-kill-server
    "L"         'gnus-server-list-servers
    "s"         'gnus-server-scan-server
    "p"         'gnus-server-yank-server
    "z"         'gnus-server-compact-server
    "M-c"       'gnus-server-close-all-servers
    "M-o"       'gnus-server-open-all-servers
    "q"         'gnus-server-exit
    "ZZ"        'gnus-server-exit
    "ZQ"        'gnus-server-exit)

  (evil-set-initial-state 'gnus-browse-mode 'normal)
  (evil-collection-define-key 'normal 'gnus-browse-mode-map
    "u" 'gnus-browse-unsubscribe-current-group
    (kbd "SPC") 'gnus-browse-read-group
    (kbd "RET") 'gnus-browse-select-group
    "q"         'gnus-browse-exit
    "ZZ"        'gnus-browse-exit
    "ZQ"        'gnus-browse-exit)

  (evil-set-initial-state 'gnus-bookmark-bmenu-mode 'normal)
  (evil-collection-define-key 'normal 'gnus-bookmark-bmenu-mode-map
    ;; quit
    "q"         'quit-window
    "ZZ"        'quit-window
    "ZQ"        'quit-window

    "g?"        'describe-mode

    ;; mark and execution
    "m"         'gnus-bookmark-bmenu-mark
    "u"         'gnus-bookmark-bmenu-unmark
    (kbd "DEL") 'gnus-bookmark-bmenu-backup-unmark
    "d"         'gnus-bookmark-bmenu-delete
    "x"         'gnus-bookmark-bmenu-execute-deletions

    (kbd "RET") 'gnus-bookmark-bmenu-select
    [mouse-2]   'gnus-bookmark-bmenu-select-by-mouse
    "L"         'gnus-bookmark-bmenu-load
    "s"         'gnus-bookmark-bmenu-save
    "t"         'gnus-bookmark-bmenu-toggle-infos
    "a"         'gnus-bookmark-bmenu-show-details
    ;; not implemented yet
    "A"         'gnus-bookmark-bmenu-show-all-annotations
    "E"         'gnus-bookmark-bmenu-edit-annotation
    "R"         'gnus-bookmark-bmenu-rename))

(provide 'evil-collection-gnus)
;;; evil-collection-gnus.el ends here
