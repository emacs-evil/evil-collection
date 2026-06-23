;;; evil-collection-ztree.el --- Evil bindings for ztree -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

;;;###autoload
(defun evil-collection-ztree-setup ()
  "Set up `evil' bindings for `ztree'."

  (evil-collection-set-readonly-bindings 'ztree-mode-map)
  (evil-set-initial-state 'ztree-mode 'normal)
  (evil-collection-define-key 'normal 'ztree-mode-map
    (kbd "<tab>") 'ztree-jump-side
    (kbd "SPC") 'ztree-perform-soft-action

    "x" 'ztree-toggle-expand-subtree)
  (evil-collection-bind 'ztree-mode-map
                        'action 'ztree-perform-action
                        'refresh 'ztree-refresh-buffer)

  (evil-collection-set-readonly-bindings 'ztreediff-mode-map)
  (evil-set-initial-state 'ztree-mode 'normal)
  (evil-collection-define-minor-mode-key 'normal 'ztreediff-mode
    "C" 'ztree-diff-copy
    "D" 'ztree-diff-simple-diff-files
    "zH" 'ztree-diff-toggle-show-filtered-files
    "zh" 'ztree-diff-toggle-show-equal-files)

  (evil-collection-bind-minor-mode 'find-file    'ztreediff-mode 'ztree-diff-view-file)
  (evil-collection-bind-minor-mode 'refresh      'ztreediff-mode 'ztree-diff-partial-rescan)
  (evil-collection-bind-minor-mode 'refresh-all  'ztreediff-mode 'ztree-diff-full-rescan)
  (evil-collection-bind-minor-mode 'delete       'ztreediff-mode 'ztree-diff-delete-file))

(provide 'evil-collection-ztree)
;;; evil-collection-ztree.el ends here
