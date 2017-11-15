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
;; If you want to use Evil in the minibuffer, you'll have to enable it manually.
;; This is so because many users find it confusing.
;;
;;      (require 'evil-minibuffer)
;;      (evil-minibuffer-init)

;;; Code:
(defvar evil-collection-mode-list
  `(ag
    anaconda-mode
    arc-mode
    bookmark
    calendar
    cider
    comint
    company
    compile
    custom
    cus-theme
    debbugs
    debug
    diff-mode
    dired
    doc-view
    edebug
    elfeed
    elisp-mode
    elisp-refs
    emms
    eshell
    flycheck
    ggtags
    help
    helm
    ibuffer
    image
    image+
    info
    ivy
    macrostep
    man
    ;; occur is in replace.el which was built-in before Emacs 26.
    (occur ,(if (<= emacs-major-version 25) "replace" 'replace))
    outline
    p4
    (package-menu package)
    pass
    (pdf pdf-view)
    proced
    prodigy
    profiler
    slime
    (term term ansi-term multi-term)
    tide
    transmission
    vlf
    woman
    xref
    (ztree ztree-diff))
  "The list of modes which will be evilified by `evil-collection-init'.
Elements are either target mode symbols or lists which `car' is the
mode symbol and `cdr' the packages to register.

By default, `minibuffer' is not included because many users find
this confusing.")

;;;###autoload
(defun evil-collection-init ()
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (require 'evil-calendar)
    (evil-calendar-setup))"
  (interactive)
  (dolist (mode evil-collection-mode-list)
    (let ((m mode)
          (reqs (list mode)))
      (when (listp mode)
        (setq m (car mode)
              reqs (cdr mode)))
      (dolist (req reqs)
        (with-eval-after-load req
          (require (intern (concat "evil-" (symbol-name m))))
          (funcall (intern (concat "evil-" (symbol-name m) "-setup"))))))))

(provide 'evil-collection)
;;; evil-collection.el ends here
