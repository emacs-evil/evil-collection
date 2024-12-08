;;; evil-collection-p-search.el --- Bindings for `p-search' -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, convenience, tools

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
;;; Bindings for p-search.

;;; Code:
(require 'evil-collection)
(require 'p-search nil t)

(defvar p-search-mode-map)
(defconst evil-collection-p-search-maps '(p-search-mode-map))

(defvar evil-collection-p-search-mode-map (make-sparse-keymap))

;; The normal `p-search-mode-map' calls (suppress-keymap map t)
;; which makes some of the evil bindings get to to 'ignore too
;; for some reason. Use a separate minor mode to avoid all of that.
(define-minor-mode evil-collection-p-search-mode
  "A minor mode to attach to `p-search' results"
  :group 'evil-collection-p-search-mode
  :keymap evil-collection-p-search-mode-map
  :lighter nil)

(defun evil-collection-p-search-setup ()
  "Set up `evil' bindings for p-search."
  (add-hook 'p-search-mode-hook 'evil-collection-p-search-mode)

  (evil-collection-define-key 'normal 'evil-collection-p-search-mode-map
    "a" 'p-search-add-dwim
    "E" 'p-search-edit-dwim
    "C" 'p-search-add-candidate-generator
    "gr" 'p-search-refresh-buffer
    "gR" 'p-search-hard-refresh-buffer
    ;; (keymap-set map "i" #'p-search-importance)
    "x" 'p-search-kill-entity-at-point
    "gj" 'p-search-next-item
    (kbd "C-j") 'p-search-next-item
    "]]" 'p-search-next-item
    "o" 'p-search-observe
    "gk" 'p-search-prev-item
    (kbd "C-k") 'p-search-prev-item
    "[[" 'p-search-prev-item
    ;; (keymap-set map "r" #'p-search-reinstantiate-prior)
    "P" 'p-search-add-prior
    "+" 'p-search-increase-preview-size
    "-" 'p-search-decrease-preview-size
    (kbd "<tab>") 'p-search-toggle-section
    (kbd "<return>") 'p-search-find-document
    "gv" 'p-search-view-document
    "go" 'p-search-view-document
    (kbd "C-o") 'p-search-display-document
    "gO" 'p-search-display-document
    "q" 'p-search-quit
    ;; (keymap-set map "C-o" #'p-search-display-file)
    ;; (keymap-set map "1" #'p-search-show-level-1)
    ;; (keymap-set map "2" #'p-search-show-level-2)
    ;; (keymap-set map "3" #'p-search-show-level-3)
    "Jg" 'p-search-jump-candidate-generators
    "Jp" 'p-search-jump-priors
    "Jr" 'p-search-jump-results))

(provide 'evil-collection-p-search)
;;; evil-collection-p-search.el ends here
