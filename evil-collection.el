;;; evil-collection.el --- A set of keybindings for Evil mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
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
;; Some minibuffer-related packages such as Helm rely on this option.

;;; Code:
(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'evil)

(defvar evil-want-integration)
(defvar evil-want-keybinding)
(if (featurep 'evil-keybindings)
    (if evil-want-keybinding
        (display-warning
         '(evil-collection)
         "Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")
      (display-warning
       '(evil-collection)
       "`evil-want-keybinding' was set to nil but not before loading evil.\
\n
Make sure to set `evil-want-keybinding' to nil before loading evil \
or evil-collection.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")))

(unless (featurep 'evil-integration)
  (message "Requiring evil-integration. Set evil-want-integration to t to\
 remove this message.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")
  (require 'evil-integration))

;; Compatibility

(eval-and-compile
  (with-no-warnings
    (if (version< emacs-version "26")
        (progn
          (defalias 'evil-collection-if-let* #'if-let)
          (defalias 'evil-collection-when-let* #'when-let)
          (function-put #'evil-collection-if-let* 'lisp-indent-function 2)
          (function-put #'evil-collection-when-let* 'lisp-indent-function 1))
      (defalias 'evil-collection-if-let* #'if-let*)
      (defalias 'evil-collection-when-let* #'when-let*))))

;; Compatibility

(declare-function org-table-align "org-table.el" nil)

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
    deadgrep
    debbugs
    debug
    diff-mode
    dired
    doc-view
    edebug
    ediff
    eglot
    elfeed
    elisp-mode
    elisp-refs
    emms
    epa
    ert
    eshell
    eval-sexp-fu
    evil-mc
    eww
    flycheck
    flymake
    free-keys
    geiser
    ggtags
    git-timemachine
    go-mode
    grep
    help
    guix
    helm
    ibuffer
    image
    image-dired
    image+
    imenu-list
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
    magit-todos
    ,@(when evil-collection-setup-minibuffer '(minibuffer))
    mu4e
    mu4e-conversation
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
    vc-dir
    vc-git
    vdiff
    view
    vlf
    which-key
    wdired
    wgrep
    woman
    xref
    youtube-dl
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
  "List of keys that may be used by Evil Collection.
This is a list of strings that are suitable for input to
`kbd'.  If there are no keys in the list, the whitelist will be ignored."
  :type '(repeat string)
  :group 'evil-collection)

(defcustom evil-collection-key-blacklist '()
  "List of keys that may not be used by Evil Collection.
This is a list of strings that are suitable for input to `kbd'."
  :type '(repeat string)
  :group 'evil-collection)

(defvar evil-collection--bindings-record (make-hash-table :test 'eq)
  "Record of bindings currently made by Evil Collection. This is
a hash-table with the package symbol as a key.  The associated
values are the package's bindings which are stored as a list of
the form ((STATE KEY BINDING)).")

(defvar evil-collection-setup-hook nil
  "Hook run by `evil-collection-init' for each mode that is evilified.
This hook runs after all setup (including keybindings) for a mode has already
taken place. The arguments passed to functions for this hook are the name of the
mode and a list of keymap names (i.e. symbols, not actual keymaps) customized by
Evil Collection for that mode. More arguments may be added in the future, so
functions added to this hook should include a \"&rest _rest\" for forward
compatibility.")

(defvar evil-collection-describe-buffer "*evil-collection*"
  "Name for Evil Collection buffer used to describe bindings.")

(defun evil-collection-define-key (state map-sym &rest bindings)
  "Wrapper for `evil-define-key*' with additional features.
Unlike `evil-define-key*' MAP-SYM should be a quoted keymap other
than the unquoted keymap required for `evil-define-key*'.  This
function adds the ability to filter keys on the basis of
`evil-collection-key-whitelist' and
`evil-collection-key-blacklist'. It also stores bindings in
`evil-collection--bindings-record'."
  (declare (indent defun))
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         (record (gethash map-sym evil-collection--bindings-record))
         filtered-bindings)
    (while bindings
      (let ((key (pop bindings))
            (def (pop bindings)))
        (when (or (and whitelist (member key whitelist))
                  (not (member key blacklist)))
          (if (consp state)
              (dolist (st state)
                (push (list (if st st 'all) (key-description key) def)
                      record))
            (push (list (if state state 'all) (key-description key) def)
                  record))
          (push key filtered-bindings)
          (push def filtered-bindings))))
    (puthash map-sym record evil-collection--bindings-record)
    (setq filtered-bindings (nreverse filtered-bindings))
    (cond ((null filtered-bindings))
          ((and (boundp map-sym) (keymapp (symbol-value map-sym)))
           (apply #'evil-define-key*
                  state (symbol-value map-sym) filtered-bindings))
          ((boundp map-sym)
           (user-error "evil-collection: %s is not a keymap" map-sym))
          (t
           (let* ((fname (format "evil-collection-define-key-in-%s" map-sym))
                  (fun (make-symbol fname)))
             (fset fun `(lambda (&rest args)
                          (when (and (boundp ',map-sym) (keymapp ,map-sym))
                            (remove-hook 'after-load-functions #',fun)
                            (apply #'evil-define-key*
                                   ',state ,map-sym ',filtered-bindings))))
             (add-hook 'after-load-functions fun t))))))

(defun evil-collection-inhibit-insert-state (map-sym)
  "Unmap insertion keys from normal state.
This is particularly useful for read-only modes."
  (evil-collection-define-key 'normal map-sym
    [remap evil-append] #'ignore
    [remap evil-append-line] #'ignore
    [remap evil-insert] #'ignore
    [remap evil-insert-line] #'ignore
    [remap evil-change] #'ignore
    [remap evil-change-line] #'ignore
    [remap evil-substitute] #'ignore
    [remap evil-change-whole-line] #'ignore
    [remap evil-delete] #'ignore
    [remap evil-delete-line] #'ignore
    [remap evil-delete-char] #'ignore
    [remap evil-delete-backward-char] #'ignore
    [remap evil-replace] #'ignore
    [remap evil-replace-state] #'ignore
    [remap evil-open-below] #'ignore
    [remap evil-open-above] #'ignore
    [remap evil-paste-after] #'ignore
    [remap evil-paste-before] #'ignore
    [remap evil-join] #'ignore
    [remap evil-indent] #'ignore
    [remap evil-shift-left] #'ignore
    [remap evil-shift-right] #'ignore
    [remap evil-invert-char] #'ignore))

(defun evil-collection--binding-lessp (a b)
  "Comparison function used to sort bindings of the form (state key def)."
  (let ((a-state (symbol-name (nth 0 a)))
        (b-state (symbol-name (nth 0 b)))
        (a-key (nth 1 a))
        (b-key (nth 1 b)))
    (if (not (string= a-state b-state))
        (string-lessp a-state b-state)
      (string-lessp a-key b-key))))

(defun evil-collection-describe-bindings (&optional arg)
  "Print bindings made by Evil Collection to separate buffer.

With non-nil ARG, restrict to bindings corresponding to active
modes in the current buffer."
  (interactive "P")
  (let ((orig-buf (current-buffer))
        (desc-buf (get-buffer-create evil-collection-describe-buffer)))
    (switch-to-buffer-other-window desc-buf)
    (with-current-buffer desc-buf
      (erase-buffer)
      (org-mode)
      (dolist (keymap
               (sort (hash-table-keys evil-collection--bindings-record)
                     (lambda (a b)
                       (string-lessp (symbol-name a)
                                     (symbol-name b)))))
        (when (or (null arg)
                  (with-current-buffer orig-buf
                    (and (boundp keymap)
                         (memq (symbol-value keymap) (current-active-maps)))))
          (insert "\n\n* " (symbol-name keymap) "\n")
          (insert "
| State | Key | Definition |
|-------|-----|------------|
")
          (cl-loop
           for (state key def) in
           (sort (copy-sequence
                  (gethash keymap evil-collection--bindings-record))
                 #'evil-collection--binding-lessp)
           do
           (when (and def (not (eq def 'ignore)))
             (insert (format "| %s | %s | %s |\n"
                             state
                             (replace-regexp-in-string "|" "¦" key)
                             (cond ((symbolp def) def)
                                   ((functionp def) "(lambda ...)")
                                   ((consp def)
                                    (format "(%s ...)" (car def)))
                                   (t "??"))))))
          (org-table-align)))
      (goto-char (point-min)))))

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

;;* Search

(defun evil-collection-evil-search-enabled ()
  (eq evil-search-module 'evil-search))

(defvar evil-collection-evil-search-forward
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-forward
                                 #'evil-search-forward))))

(defvar evil-collection-evil-search-backward
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-backward
                                 #'evil-search-backward))))

(defvar evil-collection-evil-search-next
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-next
                                 #'evil-search-next))))

(defvar evil-collection-evil-search-previous
  '(menu-item "" nil :filter (lambda (&optional _)
                               (if (eq evil-search-module 'evil-search)
                                   #'evil-ex-search-previous
                                 #'evil-search-previous))))

(provide 'evil-collection)
;;; evil-collection.el ends here
