;;; evil-slime.el --- Evil bindings for `slime' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, slime, tools

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
;; Evil bindings for `slime-mode'.

;;; Code:
(require 'evil-collection-util)
(require 'slime nil t)

(defvar sldb-mode-map)
(defvar slime-inspector-mode-map)

(defun evil-slime-setup ()
  "Set up `evil' bindings for `slime'."
  (evil-collection-util-evilify-map
   sldb-mode-map
   :mode sldb-mode
   :bindings
   "H" 'describe-mode
   "\C-j" 'sldb-down
   "\C-k" 'sldb-up
   "\M-j" 'sldb-details-down
   "\M-k" 'sldb-details-up
   "gb" 'sldb-break-on-return
   "gB" 'sldb-break-with-default-debugger)

  (evil-collection-util-evilify-map
   slime-inspector-mode-map
   :mode slime-inspector-mode
   :bindings
   "gr" 'slime-inspector-reinspect)

  (evil-define-key 'normal slime-mode-map
    (kbd "C-t") #'slime-pop-find-definition-stack
    "gd" #'slime-edit-definition)

  (evil-define-key 'normal slime-popup-buffer-mode-map
    "q" 'quit-window
    (kbd "C-t") #'slime-pop-find-definition-stack
    "gd" #'slime-edit-definition)

  (add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-slime)
;;; evil-slime.el ends here
