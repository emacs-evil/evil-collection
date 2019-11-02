;;; evil-collection-gnus.el --- Bindings for `gnus'. -*- lexical-binding: t -*-

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

(defconst evil-collection-gnus-maps '(gnus-summary-mode-map
                                      gnus-article-mode-map))

;;;###autoload
(defun evil-collection-gnus-setup ()
  "Set up `evil' bindings for `gnus'."
  (evil-set-initial-state 'gnus-summary-mode 'normal)
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'gnus-summary-mode-map
      (kbd "C-u") 'gnus-summary-scroll-up))
  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'gnus-summary-mode-map
      (kbd "C-d") 'gnus-summary-scroll-down))
  (evil-define-key 'normal gnus-summary-mode-map
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

    ;; TODO: Is this getting in the way of regular "hjkl"?
    ;; "j" 'gnus-summary-next-unread-article
    ;; "k" 'gnus-summary-prev-unread-article

    "m" 'gnus-summary-mark-as-processable
    "M" 'gnus-summary-unmark-as-processable
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
    "u" 'gnus-summary-tick-article-forward
    "U" 'gnus-summary-tick-article-backward
    "x" 'gnus-summary-limit-to-unread

    "gg" 'gnus-summary-beginning-of-article
    "G" 'gnus-summary-end-of-article
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
    "k" 'gnus-summary-kill-same-subject-and-select
    (kbd "C-k") 'gnus-summary-kill-same-subject
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
    (kbd "C-w") 'gnus-summary-mark-region-as-read
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
  (evil-define-key 'normal gnus-article-mode-map
    "q" 'evil-window-delete))

(provide 'evil-collection-gnus)
;;; evil-collection-gnus.el ends here
