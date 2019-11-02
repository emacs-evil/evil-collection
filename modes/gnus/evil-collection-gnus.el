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

    "m" 'gnus-summary-mark-as-processable
    "!" 'gnus-summary-execute-command
    "p" 'gnus-summary-pipe-output

    "zz" 'gnus-recenter
    "zc" 'gnus-cache-enter-article
    "gb" 'gnus-summary-best-unread-article
    "gf" 'gnus-summary-first-unread-article
    "z/" 'gnus-summary-limit-map
    "zd" 'gnus-summary-mark-as-dormant
    "z^" 'gnus-summary-refer-parent-article
    "zt" 'gnus-summary-toggle-header
    "u" 'gnus-summary-tick-article-forward
    "U" 'gnus-summary-tick-article-backward
    "x" 'gnus-summary-limit-to-unread

    ;; TODO: Bind the following?
    ;; "<" "=" ">" ...

    "r" 'gnus-summary-very-wide-reply
    "R" 'gnus-summary-very-wide-reply-with-original

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
