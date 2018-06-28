;;; evil-collection-ztree.el --- Evil bindings for ztree -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, ztree, tools

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
;; Evil bindings for ztree.

;;; Code:
(require 'evil-collection)
(require 'ztree nil t)

(defvar ztree-mode-map)
(defvar ztreediff-mode-map)

(defconst evil-collection-ztree-maps '(ztree-mode-map ztreediff-mode-map))

(defun evil-collection-ztree-setup ()
  "Set up `evil' bindings for `ztree'."

  (evil-collection-inhibit-insert-state 'ztree-mode-map)
  (evil-set-initial-state 'ztree-mode 'normal)
  (evil-collection-define-key 'normal 'ztree-mode-map
    (kbd "<tab>") 'ztree-jump-side
    (kbd "<return>") 'ztree-perform-action
    (kbd "SPC") 'ztree-perform-soft-action

    "x" 'ztree-toggle-expand-subtree

    ;; refresh
    "gr" 'ztree-refresh-buffer

    ;; quit
    "q" 'quit-window
    "ZQ" 'quit-window
    "ZZ" 'quit-window)

  (evil-collection-inhibit-insert-state 'ztreediff-mode-map)
  (evil-set-initial-state 'ztree-mode 'normal)
  (evil-define-minor-mode-key 'normal 'ztreediff-mode
    "C" 'ztree-diff-copy
    "D" 'ztree-diff-delete-file
    "zH" 'ztree-diff-toggle-show-filtered-files
    "d" 'ztree-diff-simple-diff-files
    "zh" 'ztree-diff-toggle-show-equal-files
    "gf" 'ztree-diff-view-file

    ;; refresh
    "gr" 'ztree-diff-partial-rescan
    "gR" 'ztree-diff-full-rescan))

(provide 'evil-collection-ztree)
;;; evil-collection-ztree.el ends here
