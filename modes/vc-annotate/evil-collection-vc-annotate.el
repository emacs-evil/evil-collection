;;; evil-collection-vc-annotate.el --- Bindings for `vc-annotate' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools

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
;; Bindings for `vc-annotate'

;;; Code:
(require 'evil-collection)
(require 'vc-annotate)

(defconst evil-collection-vc-annotate-maps '(vc-annotate-mode-map))

;;;###autoload
(defun evil-collection-vc-annotate-setup ()
  "Set up `evil' bindings for `vc-annotate'."
  (evil-set-initial-state 'vc-annotate-mode 'normal)
  (evil-collection-bind 'vc-annotate-mode-map 'quit 'quit-window)
  (evil-collection-define-key 'normal 'vc-annotate-mode-map
    "a" 'vc-annotate-revision-previous-to-line
    "d" 'vc-annotate-show-diff-revision-at-line
    "=" 'vc-annotate-show-diff-revision-at-line
    "D" 'vc-annotate-show-changeset-diff-revision-at-line
    "F" 'vc-annotate-find-revision-at-line
    "J" 'vc-annotate-revision-at-line
    "L" 'vc-annotate-show-log-revision-at-line
    "W" 'vc-annotate-working-revision
    "A" 'vc-annotate-toggle-annotation-visibility)
  (evil-collection-bind 'vc-annotate-mode-map
                        'action 'vc-annotate-goto-line
                        'next-item 'vc-annotate-next-revision
                        'prev-item 'vc-annotate-prev-revision
                        'next-section 'vc-annotate-next-revision
                        'prev-section 'vc-annotate-prev-revision))

(provide 'evil-collection-vc-annotate)
;;; evil-collection-vc-annotate.el ends here
