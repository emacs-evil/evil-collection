;;; evil-collection.el --- A set of keybindings for Evil mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>, Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <ambrevar@gmail.com>
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
;; A set of keybindings for Evil mode.
;;
;; If you want to use Evil in the minibuffer, you'll have to enable it manually.
;; This is so because many users find it confusing.
;;
;;      (require 'evil-minibuffer)
;;      (evil-minibuffer-init)

;;; Code:

(defvar evil-collection-mode-list
  '(ag
    bookmark
    calendar
    cider
    compile
    custom
    debbugs
    debugger
    diff-mode
    dired
    edebug
    elisps-refs
    eshell
    flycheck
    ggtags
    help
    ibuffer
    image
    info
    ivy
    macrostep
    man
    occur
    outline
    p4
    (package-menu package)
    pass
    proced
    prodigy
    profiler
    slime
    (term term ansi-term multi-term)
    vlf
    woman
    xref)
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
    (evil-calendar-set-keys))"
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
          (funcall (intern (concat "evil-" (symbol-name m) "-set-keys"))))))))

(provide 'evil-collection)
;;; evil-collection.el ends here
