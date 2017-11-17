;;; evil-man.el --- Evil bindings for Man-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, man, tools

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
;; Evil bindings for `man'.

;;; Code:
(require 'evil)
(require 'man)

(defun evil-man-setup ()
  "Set up `evil' bindings for `man'."
  (evil-define-key 'motion Man-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<tab>") 'forward-button
    (kbd "<backtab>") 'backward-button

    (kbd "]") 'Man-next-manpage
    (kbd "[") 'Man-previous-manpage
    (kbd "C-j") 'Man-next-section
    (kbd "C-k") 'Man-previous-section

    ;; goto
    "gm" 'man
    "gd" 'Man-goto-section
    "gR" 'Man-follow-manual-reference
    "gs" 'Man-goto-see-also-section

    ;; update
    "gr" 'Man-update-manpage

    ;; quit
    "q" 'Man-quit
    "ZQ" 'evil-quit
    "ZZ" 'Man-quit))

(provide 'evil-man)
;;; evil-man.el ends here
