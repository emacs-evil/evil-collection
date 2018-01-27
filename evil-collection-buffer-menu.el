;;; evil-collection-buffer-menu.el --- Evil bindings for Buffer-menu -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Maximiliano Sandoval <msandova@protonmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, Buffer-menu, tools

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
;; Evil bindings for `Buffer-menu'

;;; Code:

(require 'evil)

(defun evil-collection-buffer-menu-setup ()
  "Set up `evil' bindings for `Buffer-menu'."
  
  (evil-define-key '(motion normal) Buffer-menu-mode-map
    "ZQ" 'evil-quit
    "ZZ" 'quit-window
    "j"  'evil-next-line
    "k"  'evil-previous-line
    "h"  'evil-backward-char
    "l"  'evil-forward-char))

(provide 'evil-collection-buffer-menu)
;;; evil-collection-buffer-menu.el ends here
