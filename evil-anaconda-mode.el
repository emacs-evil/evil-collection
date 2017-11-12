;;; evil-anaconda-mode.el --- Bindings for `anaconda-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords:

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
(require 'anaconda-mode)
(require 'evil)

(defun evil-anaconda-mode-setup ()
  ;; Bindings don't seem to be set the first time.
  (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps)

  (evil-define-key 'normal anaconda-mode-view-mode-map
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
    "C-t" 'anaconda-mode-go-back
    "K" 'anaconda-mode-show-doc
    "gf" 'anaconda-mode-find-file))

(provide 'evil-anaconda-mode)
;;; evil-anaconda-mode.el ends here
