;;; evil-collection.el --- A set of keybindings for Evil mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (evil "1.2.13"))
;; Keywords: evil, tools

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
;; A set of keybindings for Evil mode.
;;
;; If you want to use Evil in the minibuffer, you'll have to enable it by
;; setting `evil-collection-setup-minibuffer' to t before loading this package.
;; This is so because many users find it confusing.

;;; Code:
(defgroup evil-collection nil
  "A set of keybindings for Evil mode"
  :group 'evil)

(defcustom evil-collection-mode-list
  `((ag t)
    (alchemist t)
    (anaconda-mode t)
    (arc-mode t)
    (bookmark t)
    (calendar t)
    (cider t)
    (comint t)
    (company t)
    (compile t)
    (custom t)
    (cus-theme t)
    (debbugs t)
    (debug t)
    (diff-mode t)
    (dired t)
    (doc-view t)
    (edebug t)
    (elfeed t)
    (elisp-mode t)
    (elisp-refs t)
    (emms t)
    (eshell t)
    (eval-sexp-fu t)
    (flycheck t)
    (geiser t)
    (ggtags t)
    (help t)
    (helm t)
    (ibuffer t)
    (image t)
    (image+ t)
    (info t)
    (ivy t)
    (macrostep t)
    (man t)
    (minibuffer nil)
    ;; occur is in replace.el which was built-in before Emacs 26.
    (occur ,(if (<= emacs-major-version 25) "replace" 'replace))
    (outline t)
    (p4 t)
    (package-menu package)
    (pass t)
    (pdf pdf-view)
    (proced t)
    (prodigy t)
    (profiler t)
    (rtags t)
    (slime t)
    (term t ansi-term multi-term)
    (tide t)
    (transmission t)
    (vlf t)
    (woman t)
    (xref t)
    (ztree ztree-diff))
  "The alist of modes which will be evilified by `evil-collection-init'.
 The values are list of target modes.  If t, then the target
name is the same as the key.

By default, `minibuffer' is not activated by default because many
users find this confusing.  It can be activated with

  (setcar (alist-get 'minibuffer evil-collection-mode-list) t)"
  :type '(alist)
  :group 'evil-collection)

;;;###autoload
(defun evil-collection-init ()
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (require 'evil-collection-calendar)
    (evil-collection-calendar-setup))"
  (interactive)
  (dolist (mode evil-collection-mode-list)
    (let ((name (car mode))
          (reqs (cdr mode)))
      (dolist (req reqs)
        (when (eq req t)
          (setq req name))
        (with-eval-after-load req
          (require (intern (concat "evil-collection-" (symbol-name name))))
          (funcall (intern (concat "evil-collection-" (symbol-name name) "-setup"))))))))

(provide 'evil-collection)
;;; evil-collection.el ends here
