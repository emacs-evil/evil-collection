;;; evil-collection-magit-repos.el --- Bindings for magit-repos -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016, 2021 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: Justin Burkett <justin@burkett.cc>
;; James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; Package-Requires: ((emacs "26.3") (evil "1.2.3") (magit "2.6.0"))
;; Homepage: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1

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
;;; Bindings for magit-repos.

;;; Code:
(require 'evil-collection)
(require 'magit-repos nil t)

(defvar magit-repolist-mode-map)
(defconst evil-collection-magit-repos-maps '(magit-repolist-mode-map))

;;;###autoload
(defun evil-collection-magit-repos-setup ()
  "Set up `evil' bindings for `magit-repos'."
  (evil-set-initial-state 'magit-repolist-mode 'normal)
  (evil-collection-define-key 'normal 'magit-repolist-mode-map
    "m" 'magit-repolist-mark
    "u" 'magit-repolist-unmark
    "f" 'magit-repolist-fetch
    (kbd "RET") 'magit-repolist-status
    (kbd "gr")  'magit-list-repositories)
  (add-hook 'magit-repolist-mode-hook 'evil-normalize-keymaps))

(provide 'evil-collection-magit-repos)
;;; evil-collection-magit-repos.el ends here
