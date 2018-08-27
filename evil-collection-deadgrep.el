;;; evil-collection-deadgrep.el --- Bindings for deadgrep. -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for deadgrep.

;;; Code:
(require 'evil-collection)
(require 'deadgrep nil t)

(defconst evil-collection-deadgrep-maps '(deadgrep-mode-map))

(defun evil-collection-deadgrep-setup ()
  "Set up `evil' bindings for deadgrep.."
  (evil-collection-define-key 'normal 'deadgrep-mode-map
    (kbd "RET") 'deadgrep-visit-result
    "gr" 'deadgrep-restart
    (kbd "C-j") 'deadgrep-forward
    (kbd "C-k") 'deadgrep-backward
    (kbd "TAB") 'deadgrep-toggle-file-results
    ;; Quit
    "q" 'quit-window
    "ZZ" 'quit-window
    "ZQ" 'evil-quit
    ))

(provide 'evil-collection-deadgrep)
;;; evil-collection-deadgrep.el ends here
