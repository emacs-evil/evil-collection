;;; evil-woman.el --- Add Evil bindings to WoMan

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Package-Requires: ((evil "1.2.3"))
;; Package-Version: 20170724.1223
;; Homepage: https://github.com/jojojames/evil-collection
;; Version: 0

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

;;; Code:

(require 'woman)
(require 'evil)
(require 'evil-man) ; WoMan's keymap inherits from Man.

(defun evil-woman-setup ()
  (evil-define-key 'motion woman-mode-map
    (kbd "]") 'WoMan-next-manpage
    (kbd "[") 'WoMan-previous-manpage

    ;; goto
    ;; "gm" 'woman

    ;; update
    "gr" 'woman-reformat-last-file))

(provide 'evil-woman)
;;; evil-woman.el ends here
