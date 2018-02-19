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

(require 'evil)

(defvar evil-want-integration)
(if (featurep 'evil-integration)
    (if evil-want-integration
        (display-warning
         '(evil-collection)
         "Make sure to set `evil-want-integration' to nil before loading evil \
or evil-collection.")
      (display-warning
       '(evil-collection)
       "`evil-want-integration' was set to nil but not before loading evil."))
  (require 'evil-collection-integration))

(defgroup evil-collection nil
  "A set of keybindings for Evil mode"
  :group 'evil)

(defcustom evil-collection-setup-minibuffer nil
  "Whether to setup Evil bindings in the minibuffer."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-mode-list
  `(ace-jump-mode
    ag
    alchemist
    anaconda-mode
    arc-mode
    avy
    bookmark
    (buff-menu "buff-menu")
    calendar
    cider
    cmake-mode
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
    epa
    eshell
    eval-sexp-fu
    etags-select
    eww
    flycheck
    free-keys
    geiser
    ggtags
    go-mode
    help
    helm
    ibuffer
    image
    image+
    indium
    info
    ivy
    js2-mode
    log-view
    lua-mode
    kotlin-mode
    macrostep
    man
    ,@(when evil-collection-setup-minibuffer '(minibuffer))
    neotree
    notmuch
    nov
    ;; occur is in replace.el which was built-in before Emacs 26.
    (occur ,(if (<= emacs-major-version 25) "replace" 'replace))
    outline
    p4
    (package-menu package)
    paren
    pass
    (pdf pdf-view)
    popup
    proced
    prodigy
    profiler
    python
    quickrun
    racer
    realgud
    reftex
    rjsx-mode
    robe
    ruby-mode
    rtags
    simple
    slime
    (term term ansi-term multi-term)
    tide
    transmission
    typescript-mode
    vc-annotate
    vdiff
    vlf
    which-key
    woman
    xref
    (ztree ztree-diff))
  "The list of modes which will be evilified by `evil-collection-init'.
Elements are either target mode symbols or lists which `car' is the
mode symbol and `cdr' the packages to register.

By default, `minibuffer' is not included because many users find
this confusing. It will be included if
`evil-collection-setup-minibuffer' is set to t."
  :type '(repeat (choice symbol sexp))
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
    (let ((m mode)
          (reqs (list mode)))
      (when (listp mode)
        (setq m (car mode)
              reqs (cdr mode)))
      (dolist (req reqs)
        (with-eval-after-load req
          (require (intern (concat "evil-collection-" (symbol-name m))))
          (funcall (intern (concat "evil-collection-" (symbol-name m) "-setup"))))))))

(defvar evil-collection-delete-operators '(evil-delete
                                           evil-cp-delete
                                           vil-sp-delete
                                           lispyville-delete)
  "List of delete operators.")

(provide 'evil-collection)
;;; evil-collection.el ends here
