;;; evil-macrostep.el --- Evil Integration for Macrostep -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, macrostep, tools

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
;; Evil bindings for `macrostep-mode'.

;;; Code:
(require 'evil-collection-util)
(require 'macrostep)

(defun evil-macrostep-setup ()
  ;; Keymaps don't seem to be populated on first try.
  ;; Force `evil' to normalize keymaps.
  (add-hook 'macrostep-mode-hook #'evil-normalize-keymaps)

  (evil-define-key 'normal macrostep-keymap
    "q" 'macrostep-collapse-all
    "e" 'macrostep-expand
    "u" 'macrostep-collapse
    "gj" 'macrostep-next-macro
    "gk" 'macrostep-prev-macro
    "C-j" 'macrostep-next-macro
    "C-k" 'macrostep-prev-macro))

(provide 'evil-macrostep)
;;; evil-macrostep.el ends here
