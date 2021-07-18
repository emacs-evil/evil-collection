;;; evil-collection-evil-mc.el --- Bindings for evil-mc -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
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
;;; Bindings for evil-mc.

;;; Code:
(require 'evil-collection)
(require 'evil-mc nil t)
(eval-when-compile (require 'subr-x)) ; `if-let*' and `when-let*'

(defvar evil-mc-map)
(defconst evil-collection-evil-mc-maps '(evil-mc-mode-map))

(defun evil-collection-evil-mc-clear-keymap (&rest _args)
  "Brute force remove `evil-mc-key-map' from `evil-mode-map-alist'."
  (when-let ((evil-mc-map (assq 'evil-mc-mode evil-mode-map-alist)))
    (setq evil-mode-map-alist
          (delq evil-mc-map evil-mode-map-alist))))

;;;###autoload
(defun evil-collection-evil-mc-setup ()
  "Set up `evil' bindings for evil-mc."
  ;; `evil-mc''s default keybindings conflict with `evil-collection' using the
  ;; 'gr' prefix. We brute force remove the keymap so that packages will play
  ;; nice. Open to other suggestions on how to work with `evil-mc'.
  ;; See https://github.com/emacs-evil/evil-collection/issues/184 for more
  ;; details.
  (advice-add 'evil-normalize-keymaps
              :after 'evil-collection-evil-mc-clear-keymap)

  ;; https://github.com/gabesoft/evil-mc/issues/70
  (add-hook 'evil-mc-after-cursors-deleted
            (lambda ()
              (setq evil-was-yanked-without-register t))))

(provide 'evil-collection-evil-mc)
;;; evil-collection-evil-mc.el ends here
