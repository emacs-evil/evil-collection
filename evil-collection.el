;;; evil-collection.el --- A set of keybindings for evil-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs
;; HomePage: https://github.com/jojojames/evil-collection

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
;;; A set of keybindings for evil-mode.

;;; Code:

;;;###autoload
(defun evil-collection-builtin-modes-init ()
  ""
  (interactive)
  (with-eval-after-load 'bookmark
    (require 'evil-bookmark)
    (evil-bookmark-set-keys))
  (with-eval-after-load 'compile
    (require 'evil-compile)
    (evil-compile-set-keys))
  (with-eval-after-load 'dired
    (require 'evil-dired)
    (evil-dired-set-keys))
  (with-eval-after-load 'edebug
    (require 'evil-edebug)
    (evil-edebug-set-keys)))

;;;###autoload
(defun evil-collection-extra-modes-init ()
  ""
  (interactive)
  (with-eval-after-load 'ag
    (require 'evil-ag)
    (evil-ag-set-keys))
  (with-eval-after-load 'cider
    (require 'evil-cider)
    (evil-cider-set-keys))
  (with-eval-after-load 'elisp-refs
    (require 'evil-elisp-refs)
    (evil-elisp-refs-set-keys))
  (with-eval-after-load 'flycheck
    (require 'evil-flycheck)
    (evil-flycheck-set-keys)))

;;;###autoload
(defun evil-collection-all-modes-init ()
  "Register Evil bindings for all supported modes."
  (interactive)
  (evil-collection-builtin-modes-init)
  (evil-collection-extra-modes-init))

(provide 'evil-collection)
;;; evil-collection.el ends here
