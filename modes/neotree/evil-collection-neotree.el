;;; evil-collection-neotree.el --- Evil bindings for neotree -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Maximiliano Sandoval <msandova@protonmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, neotree, tools

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
;; Evil bindings for `neotree'

;;; Code:

(require 'evil-collection)
(require 'neotree nil t)

(declare-function neotree-make-executor "neotree.el")
(defconst evil-collection-neotree-maps '(neotree-mode-map))

;;;###autoload
(defun evil-collection-neotree-setup ()
  "Set up `evil' bindings for `neotree'."

  (evil-set-initial-state 'neotree-mode 'normal) ;; Neotree start in normal by default.

  (evil-collection-bind 'neotree-mode-map 'rename 'neotree-rename-node)

  (evil-collection-define-key 'normal 'neotree-mode-map

    (kbd "<tab>") (neotree-make-executor
                   :dir-fn 'neo-open-dir)
    "z" (neotree-make-executor
         :dir-fn 'neo-open-dir)
    "gd" (neotree-make-executor
          :dir-fn 'neo-open-dired)
    "gD" (neotree-make-executor
          :dir-fn 'neo-open-dired)
    "H" 'neotree-hidden-file-toggle
    "gh" 'neotree-hidden-file-toggle
    "gv" 'neotree-open-file-in-system-application
    "c" 'neotree-create-node
    "y" 'neotree-copy-node
    "r" 'neotree-change-root
    "J" 'neotree-dir
    "+" 'neotree-stretch-toggle
    "=" 'neotree-stretch-toggle
    "ge" 'neotree-enter
    "j" 'neotree-next-line
    "k" 'neotree-previous-line

    ;; Unchanged keybings.
    "a" (neotree-make-executor
         :file-fn 'neo-open-file-ace-window)
    "|" (neotree-make-executor
         :file-fn 'neo-open-file-vertical-split)
    "-" (neotree-make-executor
         :file-fn 'neo-open-file-horizontal-split)
    "S" 'neotree-select-previous-sibling-node
    "s" 'neotree-select-next-sibling-node
    (kbd "C-c C-c") 'neotree-change-root
    (kbd "C-x 1") 'neotree-empty-fn
    (kbd "C-x 2") 'neotree-empty-fn
    (kbd "C-x 3") 'neotree-empty-fn
    (kbd "C-x C-f") 'find-file-other-window
    (kbd "C-c C-f") 'find-file-other-window)
  (evil-collection-bind 'neotree-mode-map
                        'action (neotree-make-executor
                                 :file-fn 'neo-open-file
                                 :dir-fn 'neo-open-dir)
                        'action-other (neotree-make-executor
                                       :file-fn 'neo-open-file
                                       :dir-fn 'neo-open-dir)
                        'action-stay 'neotree-quick-look
                        'next-item 'neotree-select-down-node
                        'prev-item 'neotree-select-up-node
                        'next-section 'neotree-select-down-node
                        'prev-section 'neotree-select-up-node
                        'quit 'neotree-hide
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window
                        'refresh 'neotree-refresh
                        'delete 'neotree-delete-node))

(provide 'evil-collection-neotree)
;;; evil-collection-neotree.el ends here
