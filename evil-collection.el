;;; evil-collection.el --- A set of keybindings for Evil mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (evil "1.2.13"))
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
(require 'cl-lib)
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
    calc
    calendar
    cider
    cmake-mode
    comint
    company
    compile
    custom
    cus-theme
    daemons
    debbugs
    debug
    diff-mode
    dired
    doc-view
    edebug
    eldoc
    elfeed
    elisp-mode
    elisp-refs
    emms
    epa
    ert
    eshell
    eval-sexp-fu
    etags-select
    eww
    flycheck
    free-keys
    geiser
    ggtags
    git-timemachine
    go-mode
    help
    guix
    helm
    ibuffer
    image
    image+
    indium
    info
    ivy
    js2-mode
    log-view
    lsp-ui-imenu
    lua-mode
    kotlin-mode
    macrostep
    man
    magit
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
    view
    vlf
    which-key
    wdired
    wgrep
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

(defcustom evil-collection-key-whitelist '()
  "List of keys that may be used by evil-collection.
This is a list of strings that are suitable for input to
`kbd'. If there are no keys in the list, the whitelist will be ignored."
  :type '(repeat string)
  :group 'evil-collection)

(defcustom evil-collection-key-blacklist '()
  "List of keys that may not be used by evil-collection.
This is a list of strings that are suitable for input to `kbd'."
  :type '(repeat string)
  :group 'evil-collection)

(defvar evil-collection-bindings-record '()
  "Record of bindings currently made by evil-collection. This is
an alist of the form ((PACKAGE . BINDINGS)), where bindings is an
alist of the form ((KEY . FUNCTION)).")

(defvar evil-collection-setup-hook nil
  "Hook run by `evil-collection-init' for each mode that is evilified.
This hook runs after all setup (including keybindings) for a mode has already
taken place. The arguments passed to functions for this hook are the name of the
mode and a list of keymap names (i.e. symbols, not actual keymaps) customized by
Evil Collection for that mode. More arguments may be added in the future, so
functions added to this hook should include a \"&rest _rest\" for forward
compatibility.")

(defun evil-collection-define-key (state package map-sym &rest bindings)
  "Wrapper for `evil-define-key*' with additional features.
Filter keys on the basis of `evil-collection-key-whitelist' and
`evil-collection-key-blacklist'. Store bindings in
`evil-collection-bindings-record'."
  (declare (indent defun))
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         (record (cdr-safe (assoc package evil-collection-bindings-record))))
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings)))
        (when (or (and whitelist (member key whitelist))
                  (not (member key blacklist)))
          (push (cons key def) record)
          (eval-after-load package
            `(condition-case err
                 (evil-define-key* ',state ,map-sym ,key ',def)
               (error
                (message "evil-collection: Bad binding %s"
                         '(evil-define-key* ',state ,map-sym ,key ',def))))))))
    (setq record (nreverse record))
    (if (assoc package evil-collection-bindings-record)
        (setcdr (assoc package evil-collection-bindings-record) record)
      (push (cons package record) evil-collection-bindings-record))
    nil))

(defun evil-collection--translate-key (state keymap-symbol
                                             translations
                                             destructive)
  "Helper function for `evil-collection-translate-key'.
In the keymap corresponding to STATE and KEYMAP-SYMBOL, make the key
TRANSLATIONS. When DESTRUCTIVE is non-nil, make the TRANSLATIONS destructively
without creating/referencing a backup keymap."
  (let* ((backup-keymap-symbol (intern (format "evil-collection-%s%s-backup-map"
                                               keymap-symbol
                                               (if state
                                                   (format "-%s-state" state)
                                                 ""))))
         (keymap (symbol-value keymap-symbol))
         (lookup-keymap (if (and (not destructive)
                                 (boundp backup-keymap-symbol))
                            (symbol-value backup-keymap-symbol)
                          (copy-keymap
                           (if state
                               (evil-get-auxiliary-keymap keymap state t t)
                             keymap))))
         (maps (cl-loop for (key replacement) on translations by 'cddr
                        ;; :destructive can be in TRANSLATIONS
                        unless (keywordp key)
                        collect key
                        and collect (when replacement
                                      (lookup-key lookup-keymap replacement)))))
    (unless (or destructive
                (boundp backup-keymap-symbol))
      (set backup-keymap-symbol lookup-keymap))
    (apply #'evil-define-key* state keymap maps)))

;;;###autoload
(cl-defun evil-collection-translate-key (states keymaps
                                                &rest translations
                                                &key destructive
                                                &allow-other-keys)
  "Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap symbols. Like `evil-define-key', when a keymap does not exist,
the keybindings will be deferred until the keymap is defined, so
`with-eval-after-load' is not neccessary. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation."
  (declare (indent defun))
  (unless (listp keymaps)
    (setq keymaps (list keymaps)))
  (unless (and (listp states)
               (not (null states)))
    (setq states (list states)))
  (dolist (keymap-symbol keymaps)
    (dolist (state states)
      (evil-delay `(and (boundp ',keymap-symbol)
                        (keymapp ,keymap-symbol))
          `(evil-collection--translate-key ',state ',keymap-symbol
                                           ',translations ,destructive)
        'after-load-functions t nil
        (symbol-name (cl-gensym (format "evil-collection-translate-key-in-%s"
                                        keymap-symbol)))))))

;;;###autoload
(defmacro evil-collection-swap-key (states keymaps &rest args)
  "Wrapper around `evil-collection-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `evil-collection-translate-key'. ARGS
should consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\"
\"a\" with `evil-collection-translate-key') and optionally keyword arguments for
`evil-collection-translate-key'."
  (declare (indent defun))
  (setq args (cl-loop for (key replacement) on args by 'cddr
                      collect key and collect replacement
                      and unless (keywordp key)
                      collect replacement and collect key))
  `(evil-collection-translate-key ,states ,keymaps ,@args))

;;;###autoload
(defun evil-collection-init (&optional modes)
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (require 'evil-collection-calendar)
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'."
  (interactive)
  (if modes
      (or (listp modes) (setq modes (list modes)))
    (setq modes evil-collection-mode-list))
  (dolist (mode modes)
    (let ((m mode)
          (reqs (list mode)))
      (when (listp mode)
        (setq m (car mode)
              reqs (cdr mode)))
      (dolist (req reqs)
        (with-eval-after-load req
          (require (intern (concat "evil-collection-" (symbol-name m))))
          (funcall (intern (concat "evil-collection-" (symbol-name m)
                                   "-setup")))
          (let ((mode-keymaps
                 (ignore-errors
                   (symbol-value
                    (intern (format "evil-collection-%s-maps" m))))))
            (run-hook-with-args 'evil-collection-setup-hook
                                m mode-keymaps)))))))

(defvar evil-collection-delete-operators '(evil-delete
                                           evil-cp-delete
                                           evil-sp-delete
                                           lispyville-delete)
  "List of delete operators.")

(defvar evil-collection-yank-operators '(evil-yank
                                         evil-cp-yank
                                         evil-sp-yank
                                         lispyville-yank)
  "List of yank operators.")

(provide 'evil-collection)
;;; evil-collection.el ends here
