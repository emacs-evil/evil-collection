;;; evil-collection-xref.el --- Evil bindings for xref -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, xref, tools

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
;; Evil bindings for `xref'.

;;; Code:
(require 'evil-collection)
(require 'xref)

(defconst evil-collection-xref-maps `(xref--xref-buffer-mode-map
                                      ,@(when (>= emacs-major-version 27) '(xref--transient-buffer-mode-map))))

;;;###autoload
(defun evil-collection-xref-setup ()
  "Set up `evil' bindings for `xref'."
  (evil-set-initial-state 'xref--xref-buffer-mode 'normal)
  (evil-collection-set-readonly-bindings 'xref--xref-buffer-mode-map)
  (evil-collection-define-key 'normal 'xref--xref-buffer-mode-map
    (kbd "C-n") 'xref-next-line
    (kbd "C-p") 'xref-prev-line
    "r" 'xref-query-replace-in-results

    ;; Match `dired''s `dired-do-find-regexp-and-replace'.
    "Q" 'xref-query-replace-in-results

    "o" 'xref-show-location-at-point)
  ;; In Emacs mode map, TAB binds to `xref-quit-and-goto-xref'.
  (evil-collection-bind 'xref--xref-buffer-mode-map
                        'action 'xref-goto-xref
                        'action-other 'xref-quit-and-goto-xref
                        'action-stay 'xref-show-location-at-point
                        'next-item 'xref-next-line
                        'prev-item 'xref-prev-line
                        'next-section-2 'xref-next-line
                        'prev-section-2 'xref-prev-line)

  (when (>= emacs-major-version 27)
    (evil-collection-bind 'xref--xref-buffer-mode-map 'refresh 'xref-revert-buffer))

  (when (>= emacs-major-version 28)
    ;; `]]'/`[[' navigate by group; line navigation lives on `gj'/`gk'
    ;; and `C-j'/`C-k' via the `next-item' / `next-section-2' themes.
    (evil-collection-bind 'xref--xref-buffer-mode-map
                          'next-section 'xref-next-group
                          'prev-section 'xref-prev-group))

  (when (>= emacs-major-version 27)
    (evil-set-initial-state 'xref--transient-buffer-mode 'normal)
    (evil-collection-bind 'xref--transient-buffer-mode-map 'action 'xref-quit-and-goto-xref)))

(provide 'evil-collection-xref)
;;; evil-collection-xref.el ends here
