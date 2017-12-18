;;; evil-collection-anaconda-mode.el --- Bindings for `anaconda-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, python, tools

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
;;; Bindings for `anaconda-mode'.

;;; Code:
(require 'anaconda-mode nil t)
(require 'evil)

(defconst evil-collection-anaconda-mode-maps '(anaconda-mode-view-mode-map
                                               anaconda-mode-map))

(defun evil-collection-anaconda-mode-setup ()
  "Set up `evil' bindings for `anaconda-mode'."
  ;; Bindings don't seem to be set the first time.
  (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps)

  ;; latest anaconda uses `anaconda-view-mode-map'; anaconda stable
  ;; uses `anaconda-mode-view-mode-map'
  (evil-define-key 'normal (if (boundp 'anaconda-mode-view-mode-map)
                               anaconda-mode-view-mode-map
                             anaconda-view-mode-map)
    "gj" 'next-error-no-select
    "gk" 'previous-error-no-select
    (kbd "C-j") 'next-error-no-select
    (kbd "C-k") 'previous-error-no-select
    "]" 'next-error-no-select
    "[" 'previous-error-no-select
    "q" 'quit-window)

  (evil-define-key 'normal anaconda-mode-map
    ;; Would be nice to support these too.
    ;; 'anaconda-mode-find-assignments
    ;; 'anaconda-mode-find-references
    "gd" 'anaconda-mode-find-definitions
    (kbd "C-t") 'anaconda-mode-go-back
    "K" 'anaconda-mode-show-doc
    "gf" 'anaconda-mode-find-file))

(provide 'evil-collection-anaconda-mode)
;;; evil-collection-anaconda-mode.el ends here
