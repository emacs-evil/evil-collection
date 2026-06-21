;;; evil-collection-shell-maker.el --- Bindings for `shell-maker' -*- lexical-binding: t -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, shell-maker, tools

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
;;; Bindings for `shell-maker'.
;;;
;;; These bindings apply to `shell-maker-mode' and any derived mode that
;;; doesn't override RET / S-RET in its own evil state map (for example,
;;; `agent-shell-mode' inherits from `shell-maker-mode-map').

;;; Code:
(require 'evil-collection)
(require 'shell-maker nil t)

(defgroup evil-collection-shell-maker nil
  "Evil bindings for `shell-maker'."
  :group 'evil-collection)

(make-obsolete-variable 'evil-collection-shell-maker-submit-state
                        'evil-collection-repl-submit-state
                        "evil-collection 0.0.2")

(defconst evil-collection-shell-maker-maps '(shell-maker-mode-map))


(defvar shell-maker-mode-map)

;;;###autoload
(defun evil-collection-shell-maker-setup ()
  "Set up `evil' bindings for `shell-maker'."
  (evil-collection-theme-bind 'repl-submit        'shell-maker-mode-map 'shell-maker-submit)
  (evil-collection-theme-bind 'repl-newline       'shell-maker-mode-map 'newline)
  (evil-collection-theme-bind 'repl-force-newline 'shell-maker-mode-map 'newline))

(provide 'evil-collection-shell-maker)
;;; evil-collection-shell-maker.el ends here
