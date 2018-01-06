;;; evil-collection-free-keys.el --- Evil bindings for free-keys -*- lexical-binding: t -*-

;; Copyright (C) 2017 Tal Wrii

;; Author: Tal Wrii <talwrii@gmail.com>
;; Maintainer: James Nguyen <talwrii@gmail.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, free-keys, tools

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
;; Evil bindings for free-keys.
;; https://github.com/Fuco1/free-keys

;;; Code:
(require 'free-keys nil t)
(require 'evil)

(defun evil-collection-free-keys-setup ()
  "Set up `evil' bindings for `free-keys'."
  (evil-set-initial-state 'free-keys-mode 'motion)
  (evil-define-key 'motion free-keys-mode-map
    "b" 'free-keys-change-buffer
    "p" 'free-keys-set-prefix
    "q" 'quit-window))

(provide 'evil-collection-free-keys)
;;; evil-collection-free-keys ends here
