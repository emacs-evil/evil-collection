;;; evil-collection-difftastic.el --- Bindings for `difftastic' -*- lexical-binding: t -*-

;; Copyright (C) 2025 hiecaq

;; Author: hiecaq <soc.github@hiecaq.org>
;; Maintainer: hiecaq <soc.github@hiecaq.org>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
;;; Bindings for difftastic.

;;; Code:
(require 'evil-collection)
(require 'difftastic nil t)

(defvar difftastic-mode-map)

(defconst evil-collection-difftastic-maps '(difftastic-mode-map))

;;;###autoload
(defun evil-collection-difftastic-setup ()
  "Set up `evil' bindings for `difftastic'."
  (evil-collection-define-key 'normal 'difftastic-mode-map
    (kbd "[[") 'difftastic-previous-file
    (kbd "]]") 'difftastic-next-file
    (kbd "C-j") 'difftastic-next-chunk
    (kbd "C-k") 'difftastic-previous-chunk
    "gj" 'difftastic-next-chunk
    "gk" 'difftastic-previous-chunk
    "zc" 'difftastic-hide-chunk
    "zo" 'difftastic-show-chunk
    (kbd "TAB") 'difftastic-toggle-chunk
    (kbd "RET") 'difftastic-diff-visit-file
    "o" 'difftastic-diff-visit-worktree-file
    "gr" 'difftastic-rerun
    "q" 'difftastic-leave
    "ZQ" 'difftastic-quit))

(provide 'evil-collection-difftastic)

;;; evil-collection-difftastic.el ends here
