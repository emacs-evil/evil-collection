;;; evil-collection.el --- A set of keybindings for Evil mode -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2023 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3") (evil "1.2.13"))
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

;; `evil' requires `seq-into'?
;; This require on `seq' before loading `evil 'prevents `evil' from erroring
;; out with the below message on Emacs 29.
;; Symbol's function definition is void: seq-into
;; Looks like this error can be traced through evil ->
;; Look at the commit that moved this line above `evil' to see the error message.
;; evil -> evil-vars -> read-kbd-macro -> seq-into -> error.
;; https://github.com/emacs-evil/evil/issues/1627
(require 'seq)
(require 'cl-lib)
(require 'evil)
;; `annalist' is optional; only used to record and describe bindings via
;; `evil-collection-describe-bindings'. Install it to enable that feature.
(require 'annalist nil t)

(declare-function annalist-record "annalist")
(declare-function annalist-describe "annalist")
(declare-function annalist-define-view "annalist")
(declare-function annalist-string-< "annalist")

(defvar evil-collection-base-dir (file-name-directory load-file-name)
  "Store the directory evil-collection.el was loaded from.")

(defvar evil-want-integration)
(defvar evil-want-keybinding)

(defvar evil-collection--startup-warning-flag nil)

(defun evil-collection-maybe-display-startup-warning ()
  "Warn if `evil-want-keybinding' was not set before loading Evil.
Runs at most once."
  (unless evil-collection--startup-warning-flag
    (setq evil-collection--startup-warning-flag t)
    (when (featurep 'evil-keybindings)
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
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")))))

(unless (featurep 'evil-integration)
  (message "Requiring evil-integration. Set evil-want-integration to t to\
 remove this message.\
\n
See https://github.com/emacs-evil/evil-collection/issues/60 for more details.")
  (require 'evil-integration))

(defgroup evil-collection nil
  "A set of keybindings for Evil mode."
  :group 'evil)

(defcustom evil-collection-setup-minibuffer nil
  "Whether to setup Evil bindings in the minibuffer."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-calendar-want-org-bindings nil
  "Whether to bind Org functions in calendar keymap."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-setup-debugger-keys t
  "Whether to bind debugger keys when debugger is active.

Debugger in this case is dependent on mode.

This is only relevant for debug modes that are part of another mode,

e.g. `indium'. Modes like `edebug' or `realgud' needs to be explicitly disabled

through removing their entry from `evil-collection-mode-list'.

This variable is obsolete; new customization should use
`evil-collection-binding-overrides' instead:

  (setq evil-collection-binding-overrides
        \\='((debug-breakpoint :enabled nil)))"
  :type 'boolean
  :group 'evil-collection)

(make-obsolete-variable 'evil-collection-setup-debugger-keys
                        "use `evil-collection-binding-overrides': `debug-continue', `debug-step-over', `debug-step-into', `debug-step-out', `debug-breakpoint'."
                        "evil-collection 0.0.3")

(defcustom evil-collection-want-unimpaired-p t
  "Whether to enable unimpaired style bindings globally."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-want-find-usages-bindings t
  "Whether to bind `xref-find-references'-like bindings.

This will bind additional find-* type commands, e.g. usages, assignments, etc..

This variable is obsolete; New customization
should use `evil-collection-binding-overrides' instead:

  (setq evil-collection-binding-overrides
        \\='((find-usages :enabled nil)))"
  :type 'boolean
  :group 'evil-collection)

(make-obsolete-variable 'evil-collection-want-find-usages-bindings
                        "use `evil-collection-binding-overrides': `find-usages'."
                        "evil-collection 0.0.3")

(defcustom evil-collection-want-g-bindings t
  "Whether to bind g* bindings."
  :type 'boolean
  :group 'evil-collection)

(defvar evil-collection--modes-with-delayed-setup
  `(emms
    eshell)
  "List of modes whose keybinds aren't completely set up after the mode is
loaded. This can be a problem for cases where we're doing key translations
using `evil-collection-setup-hook' which would result in an empty keymap.

Normally we run `evil-collection-setup-hook' right away after the mode
is loaded in `with-eval-after-load' (see `evil-collection-init') but for these
modes, we skip running that hook and let the corresponding `evil-collection'
package handle running `evil-collection-setup-hook'.

Elements in this list either match a target mode symbol or the car of a list in
`evil-collection--supported-modes'.

If `evil-collection-always-run-setup-hook-after-load' is t, this list isn't
read and `evil-collection-setup-hook' will be ran in the
`with-eval-after-load' block in `evil-collection-init'.")

(defcustom evil-collection-always-run-setup-hook-after-load nil
  "Whether to always run `evil-collection-setup-hook' after mode is loaded.

See `evil-collection-init' and `evil-collection--modes-with-delayed-setup'."
  :type 'boolean
  :group 'evil-collection)

(defcustom evil-collection-defer-delay 3
  "Default idle delay in seconds used when deferring mode initialization.

Used as a fallback when no explicit delay is specified for a mode in
`evil-collection-defer'."
  :type 'number
  :group 'evil-collection)

(defvar evil-collection--supported-modes
  `(2048-game
    ag
    agent-shell
    alchemist
    anaconda-mode
    apropos
    arc-mode
    atomic-chrome
    auto-package-update
    beginend
    bluetooth
    bm
    bookmark
    (buff-menu "buff-menu")
    bufler
    calc
    calendar
    cider
    citre
    cmake-mode
    color-rg
    comint
    company
    compile
    consult
    corfu
    crdt
    (csv "csv-mode")
    (custom cus-edit)
    cus-theme
    dape
    dashboard
    daemons
    deadgrep
    debbugs
    debug
    devdocs
    dictionary
    diff-hl
    diff-mode
    difftastic
    dired
    dired-sidebar
    disk-usage
    distel
    doc-view
    docker
    eat
    ebib
    ebuku
    eca
    edbi
    edebug
    ediff
    eglot
    elpaca
    ement
    explain-pause-mode
    eldoc
    elfeed
    elisp-mode
    elisp-refs
    elisp-slime-nav
    embark
    emms
    ,@(when (>= emacs-major-version 29) '(emoji))
    epa
    ert
    eshell
    eval-sexp-fu
    evil-ghostel
    evil-mc
    eww
    fanyi
    finder
    flycheck
    flymake
    fj
    forge
    free-keys
    fzfa
    geiser
    ggtags
    git-timemachine
    gited
    gnus
    go-mode
    gptel
    grep
    guix
    hackernews
    helm
    help
    helpful
    hg-histedit
    hideshow
    hungry-delete
    hyrolo
    ibuffer
    ielm
    (image image-mode)
    image-dired
    image+
    imenu
    imenu-list
    (indent "indent")
    indium
    info
    ivy
    js2-mode
    ,@(when (>= emacs-major-version 30) '(kmacro))
    leetcode
    lispy
    lms
    log-edit
    log-view
    lsp-ui-imenu
    lua-mode
    kotlin-mode
    macrostep
    man
    (magit magit-submodule) ;; See https://github.com/emacs-evil/evil-collection/issues/637
    magit-repos
    magit-section
    magit-todos
    markdown-mode
    message
    minesweeper
    ,@(when evil-collection-setup-minibuffer '(minibuffer))
    monky
    mpc
    mpdel
    mpdired
    mu4e
    mu4e-conversation
    neotree
    newsticker
    notmuch
    nov
    omnisharp
    org
    org-agenda
    org-present
    org-roam
    osx-dictionary
    outline
    ovpn-mode
    p4
    (package-menu package)
    pass
    (pdf pdf-view)
    popup
    proced
    (process-menu simple)
    prodigy
    profiler
    p-search
    python
    quickrun
    racer
    racket-describe
    reader
    realgud
    reftex
    replace ;; For `occur'.
    restclient
    rg
    ripgrep
    rjsx-mode
    robe
    rtags
    ruby-mode
    scheme
    scroll-lock
    selectrum
    sh-script
    shell-maker
    ,@(when (>= emacs-major-version 28) '(shortdoc))
    simple
    simple-mpc
    slime
    sly
    smerge-mode
    snake
    so-long
    speedbar
    ,@(when (>= emacs-major-version 27) '(tab-bar))
    tablist
    tabulated-list
    tar-mode
    telega
    (term term ansi-term multi-term)
    tetris
    ,@(when (>= emacs-major-version 27) '(thread))
    tide
    timer-list
    transmission
    trashed
    tuareg
    typescript-mode
    ultra-scroll
    vc-annotate
    vc-dir
    vc-git
    vdiff
    vertico
    view
    vlf
    vterm
    vundo
    w3m
    wdired
    wgrep
    which-key
    with-editor
    woman
    xref
    xwidget
    yaml-mode
    youtube-dl
    zmusic
    (ztree ztree-diff ztree-dir))
  "List of modes supported by evil-collection. Elements are
either target mode symbols or lists which `car' is the mode
symbol and `cdr' the packages to register.")

(dolist (mode evil-collection--supported-modes)
  (let ((ec-mode-name (if (listp mode) (car mode) mode)))
    (autoload
      (intern (format "evil-collection-%s-setup" ec-mode-name))
      (expand-file-name
       (format "modes/%s/evil-collection-%s" ec-mode-name ec-mode-name)
       evil-collection-base-dir))))

(defcustom evil-collection-mode-list evil-collection--supported-modes
  "The list of modes which will be evilified by `evil-collection-init'.
Elements are either target mode symbols or lists which `car' is the
mode symbol and `cdr' the packages to register.

By default, `minibuffer' is not included because many users find
this confusing. It will be included if
`evil-collection-setup-minibuffer' is set to t."
  :type '(repeat (choice symbol sexp))
  :group 'evil-collection)

(defcustom evil-collection-config
  '((buff-menu :defer t)
    (calc :defer t)
    (comint :defer t)
    (debug :defer t)
    (diff-mode :defer t)
    (dired :hook dired-mode)
    (edebug :defer t)
    (eldoc :defer t)
    (help :defer t)
    (image :defer t)
    (indent :defer t)
    (info :defer t)
    (replace :defer t)
    (outline :defer t)
    (package-menu :defer t)
    (process-menu :defer t)
    (simple :defer t)
    (tab-bar :defer t)
    (tabulated-list :defer t)
    (xref :defer t))
  "The list of modes with special configuration.

These modes should match entries within `evil-collection-mode-list'.

This variable is consumed only by `evil-collection-setup'.


NOTE: The API of this variable may change drastically.

Currently supported keys:

:defer t or TIME in seconds to defer loading mode.

:hook HOOK-NAME to initialize the mode when a hook fires.  The hook
name does not need the \"-hook\" suffix; it will be appended
automatically.  When :hook is present, :defer is ignored."
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

(defcustom evil-collection-state-passlist '()
  "List of evil states that may be used by Evil Collection.
This is a list of symbols that are suitable for input to
 `evil-define-key'. Ignore when there are no states in the list."
  :type '(repeat symbol)
  :group 'evil-collection)

(defcustom evil-collection-state-denylist
  (if (bound-and-true-p evil-disable-insert-state-bindings)
      '(insert)
    '())
  "List of evil states that may not be used by Evil Collection.
This is a list of symbols that are suitable for input to
 `evil-define-key'."
  :type '(repeat symbol)
  :group 'evil-collection)

(defcustom evil-collection-repl-submit-state 'normal
  "Evil state in which RET submits the prompt in REPL-like buffers.

The other state gets RET bound to `newline'.  S-RET always inserts a
newline regardless of state.

New customization should use
`evil-collection-binding-overrides' instead:

  `repl-submit', `repl-newline', `repl-force-newline'

  (setq evil-collection-binding-overrides
        \\='((repl-submit  :state insert)
          (repl-newline :state normal)))"
  :type '(choice (const :tag "Submit in normal state" normal)
                 (const :tag "Submit in insert state" insert))
  :group 'evil-collection)

(make-obsolete-variable 'evil-collection-repl-submit-state
                        "use `evil-collection-binding-overrides': `repl-submit', `repl-newline'."
                        "evil-collection 0.0.3")

(defvar evil-collection-binding-defaults
  `((term-toggle-escape :enabled t
                        :state (normal insert)
                        :key "C-c C-z")
    (repl-submit :enabled t
                 :state ,(lambda () evil-collection-repl-submit-state)
                 :key ("RET" "<return>" "C-m"))
    (repl-newline :enabled t
                  :state ,(lambda ()
                            (if (eq evil-collection-repl-submit-state 'normal)
                                'insert
                              'normal))
                  :key ("RET" "<return>" "C-m"))
    (repl-force-newline :enabled t
                        :state (normal insert)
                        :key ("S-<return>" "S-RET"))
    (find-usages :enabled ,(lambda (_map-sym _id _keys _command)
                             evil-collection-want-find-usages-bindings)
                 :state normal
                 :key "gr")
    (find-definition :enabled t :state normal :key "gd")
    (pop-definition :enabled t :state normal :key "C-t")
    (lookup-doc :enabled t :state normal :key "K")
    (goto-repl :enabled t :state normal :key "gz")
    (find-file :enabled t :state normal :key "gf")
    (quit :enabled t :state normal :key "q")
    (quit-save :enabled t :state normal :key "ZZ")
    (quit-cancel :enabled t :state normal :key "ZQ")
    (rename :enabled t :state normal :key ("r" "R"))
    (edit :enabled t :state normal :key ("e" "E"))
    (browse-url :enabled t :state normal :key "gx")
    (describe-mode :enabled t :state normal :key "g?")
    (refresh :enabled t :state normal :key "gr")
    (refresh-all :enabled t :state normal :key "gR")
    (action :enabled t :state normal :key ("RET" "<return>"))
    (action-other :enabled t :state normal :key ("S-<return>" "S-RET" "go"))
    (action-stay :enabled t :state normal :key ("M-<return>" "M-RET" "gO"))
    (debug-continue    :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key ("c" "<f5>"))
    (debug-step-over   :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key ("n" "<f10>"))
    (debug-step-into   :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key ("i" "s" "<f11>"))
    (debug-step-out    :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key ("o" "<S-f11>"))
    (debug-breakpoint  :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key ("b" "<f9>"))
    (debug-eval        :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key "e")
    (debug-locals      :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key "L")
    (debug-restart     :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key "R")
    (debug-frame-up    :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key "<")
    (debug-frame-down  :enabled ,(lambda (_map-sym _id _keys _command)
                                    evil-collection-setup-debugger-keys)
                       :state normal
                       :key ">")
    (next-button :enabled t :state normal :key ("<tab>" "TAB"))
    (previous-button :enabled t :state normal :key ("<backtab>" "<S-tab>"))
    (cycle-next :enabled t :state normal :key ("<tab>" "TAB"))
    (cycle-previous :enabled t :state normal :key ("<backtab>" "<S-tab>"))
    (section-toggle :enabled t :state normal :key ("<tab>" "TAB"))
    (next-item :enabled t :state normal :key "gj")
    (prev-item :enabled t :state normal :key "gk")
    (next-section :enabled t :state normal :key ("]]"))
    (prev-section :enabled t :state normal :key ("[["))
    (next-section-2 :enabled t :state normal :key "C-j")
    (prev-section-2 :enabled t :state normal :key "C-k")
    (scroll-down :enabled t :state normal :key "SPC")
    (scroll-up :enabled t :state normal :key ("S-SPC" "S-<space>"))
    (history-previous :enabled t :state normal :key "C-p")
    (history-next :enabled t :state normal :key "C-n")
    (completion-previous :enabled t :state insert :key "C-p")
    (completion-next :enabled t :state insert :key "C-n")
    (zoom-in :enabled t :state normal :key ("+" "="))
    (zoom-out :enabled t :state normal :key "-")
    (zoom-reset :enabled t :state normal :key "0")
    (jump :enabled t :state normal :key "J")
    (mark :enabled t :state normal :key "m")
    (mark-all :enabled t :state normal :key "M")
    (mark-toggle-all :enabled t :state normal :key "~")
    (unmark :enabled t :state normal :key "u")
    (unmark-all :enabled t :state normal :key "U")
    (mark-delete :enabled t :state normal :key "d")
    (execute-marks :enabled t :state normal :key "x")
    (delete :enabled t :state normal :key "d"))
  "Built-in entries for the evil-collection theme system.

Each entry has the form (ID . PLIST) and may use:

  :enabled  nil, t, or a function with the signature
              (lambda (map-sym id keys command) ...)
            called at lookup time.  Missing entries are treated as t.
            Any of the four arguments may be nil when
            `evil-collection-binding-enabled-p' is called outside the
            bind helpers (e.g. as a pure feature toggle from a
            consumer-side setup function).  Override examples:

              ;; Skip in one specific keymap.
              (find-usages :enabled
                ,(lambda (map-sym _id _keys _cmd)
                   (not (eq map-sym \\='dired-sidebar-mode-map))))

              ;; Skip when bound to a particular key.
              (find-usages :enabled
                ,(lambda (_map-sym _id keys _cmd)
                   (not (member \"gr\" keys))))

  :state    Evil state symbol, list of state symbols, or a function
            of no args returning either.  A singular value is wrapped
            to a list at lookup.  Optional.
  :key      Key string (suitable for `kbd'), list of key strings, or
            a function of no args returning either.  Optional.

:state and :key are optional.  An entry with only :enabled acts as a
pure feature toggle: the consumer-side code does the binding itself
and asks the resolver via `evil-collection-binding-enabled-p' whether
the feature is on.

Function values for :state and :key are funcalled at lookup time with
no arguments, which is useful for delegating to legacy defcustoms
while features migrate to the theme system.  Plain symbols are never
funcalled even when they happen to have a function binding (e.g. the
state symbol `insert' is also the `insert' function); that is why
anonymous lambdas are required when a property's value should be
computed dynamically.

Users customize via `evil-collection-binding-overrides'; do not
modify this variable directly.")

(defcustom evil-collection-binding-overrides nil
  "User overrides merged onto `evil-collection-binding-defaults'.

Same shape as `evil-collection-binding-defaults'.  Properties present
here win per-property; properties omitted here fall through to the
defaults.

  (setq evil-collection-binding-overrides
        \\='((term-toggle-escape :state normal :key \"C-c j\")))

Use `:enabled nil' to disable a feature.  An explicit nil counts as
\"set\", not \"absent\"."
  :type '(alist :key-type symbol :value-type plist)
  :group 'evil-collection)

(defun evil-collection-binding--get (id prop)
  "Resolve PROP for theme entry ID.

Overrides win over defaults per-property.  Returns nil if neither
side sets PROP."
  (let ((over (cdr (assq id evil-collection-binding-overrides)))
        (def  (cdr (assq id evil-collection-binding-defaults))))
    (cond ((plist-member over prop) (plist-get over prop))
          ((plist-member def prop)  (plist-get def prop)))))

(defun evil-collection-binding--resolve (v)
  "Funcall V if it is an anonymous function; otherwise return V.
Plain symbols are returned as-is even when they have a function
binding — they are data (state names, command names) in this
context, never callables."
  (if (and (functionp v) (not (symbolp v)))
      (funcall v)
    v))

(defun evil-collection-binding--listify (v)
  "Wrap V in a list unless it already is one.  Nil stays nil."
  (cond ((null v) nil)
        ((listp v) v)
        (t (list v))))

(defun evil-collection-binding-enabled-p (id &optional map-sym keys command)
  "Return non-nil if theme entry ID is enabled.

:enabled may be nil, t, or a function with the signature
\(lambda (MAP-SYM ID KEYS COMMAND) ...).  Missing -> t.

The optional MAP-SYM, KEYS, and COMMAND are forwarded to the function
when present at the call site, and otherwise nil — this lets callers
that resolve `:enabled' as a pure feature toggle (no concrete keymap
in scope) keep their existing call shape."
  (let* ((over (cdr (assq id evil-collection-binding-overrides)))
         (def  (cdr (assq id evil-collection-binding-defaults)))
         (v (cond ((plist-member over :enabled) (plist-get over :enabled))
                  ((plist-member def :enabled)  (plist-get def :enabled))
                  (t t))))
    (if (and (functionp v) (not (symbolp v)))
        (funcall v map-sym id keys command)
      v)))

(defun evil-collection-binding-states (id)
  "Return list of evil states configured for theme entry ID."
  (evil-collection-binding--listify
   (evil-collection-binding--resolve (evil-collection-binding--get id :state))))

(defun evil-collection-binding-keys (id)
  "Return list of key strings configured for theme entry ID."
  (evil-collection-binding--listify
   (evil-collection-binding--resolve (evil-collection-binding--get id :key))))

(defun evil-collection-bind (map-sym &rest id-command-pairs)
  "Bind one or more theme entries in MAP-SYM.

ID-COMMAND-PAIRS is an even-length list alternating theme IDs and
commands:

  (evil-collection-bind \\='foo-mode-map
    \\='action \\='foo-do
    \\='quit   \\='foo-quit)

Each ID is resolved through `evil-collection-binding-keys'; entries with
`:enabled' nil are skipped.  Pairs that share the same `:state'
value are batched into a single `evil-collection-define-key' call
so a deferred map installs one `after-load-functions' hook per
state-group instead of one per pair."
  (let ((groups nil))
    (while id-command-pairs
      (let* ((id (pop id-command-pairs))
             (cmd (pop id-command-pairs))
             (keys (evil-collection-binding-keys id)))
        (when (evil-collection-binding-enabled-p id map-sym keys cmd)
          (let ((states (evil-collection-binding-states id)))
            (when keys
              (let ((pairs (cl-mapcan (lambda (k) (list (kbd k) cmd))
                                      keys))
                    (group (assoc states groups)))
                (if group
                    (setcdr group (nconc (cdr group) pairs))
                  (push (cons states pairs) groups))))))))
    (dolist (group (nreverse groups))
      (apply #'evil-collection-define-key
             (car group) map-sym (cdr group)))))

(defun evil-collection-bind-minor-mode (id mode command)
  "Bind theme entry ID to COMMAND in minor-mode MODE.

Like `evil-collection-bind' but routes through
`evil-collection-define-minor-mode-key'."
  (let ((keys (evil-collection-binding-keys id)))
    (when (evil-collection-binding-enabled-p id mode keys command)
      (let ((states (evil-collection-binding-states id)))
        (when keys
          (apply #'evil-collection-define-minor-mode-key states mode
                 (cl-mapcan (lambda (key) (list (kbd key) command))
                            keys)))))))

(defun evil-collection-bind-local (id command)
  "Bind theme entry ID to COMMAND in the current buffer.

Like `evil-collection-bind' but uses `evil-local-set-key'."
  (let ((keys (evil-collection-binding-keys id)))
    (when (evil-collection-binding-enabled-p id nil keys command)
      (let ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
            (blacklist (mapcar 'kbd evil-collection-key-blacklist))
            (states (evil-collection--filter-states
                     (evil-collection-binding-states id))))
        (when states
          (dolist (key keys)
            (let ((kbd-key (kbd key)))
              (when (evil-collection--can-bind-key kbd-key whitelist blacklist)
                (when (featurep 'annalist)
                  (annalist-record 'evil-collection 'keybindings
                                   (list 'local (car states) kbd-key command)
                                   :local t))
                (dolist (state states)
                  (evil-local-set-key state kbd-key command))))))))))

(defvar evil-collection-setup-hook nil
  "Hook run by `evil-collection-init' for each mode that is evilified.
This hook runs after all setup (including keybindings) for a mode has already
taken place. The arguments passed to functions for this hook are the name of the
mode and a list of keymap names (i.e. symbols, not actual keymaps) customized by
Evil Collection for that mode. More arguments may be added in the future, so
functions added to this hook should include a \"&rest _rest\" for forward
compatibility.")

(defun evil-collection-define-operator-key (operator map-sym &rest bindings)
  "Define a key on a specific OPERATOR e.g. yank or delete.

This function is useful for adding specific binds to operator maps
\(e.g. `evil-yank' or `evil-delete') without erasing the original bind.

For example, say one wants to bind \"yf\" to something but also wants to keep
\"yy\".

This function takes care of checking the whitelist/blacklist against the full
binding.

For example:
\(evil-collection-define-operator-key \='yank
  \='pass-mode-map \"f\" \='pass-copy-field)

This will check \"yf\" against a user's white/blacklist and also record the
binding in `annalist' as so."
  (declare (indent defun))
  (let* ((prefix (if (eq operator 'yank) "y" "d"))
         (operators (if (eq operator 'yank)
                        'evil-collection-yank-operators
                      'evil-collection-delete-operators))
         (remap (if (eq operator 'yank) [remap evil-yank] [remap evil-delete]))
         (whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         filtered-bindings)
    (while bindings
      (let* ((key (pop bindings))
             (key-with-prefix (concat prefix key))
             (def (pop bindings))
             (def-with-menu-item
              `(menu-item
                ""
                nil
                :filter
                (lambda (&optional _)
                  (when (or
                         (eq evil-this-operator (key-binding ,remap))
                         (memq evil-this-operator ,operators))
                    (setq evil-inhibit-operator t)
                    ',def)))))
        (when (or (and whitelist (member key-with-prefix whitelist))
                  (not (member key-with-prefix blacklist)))
          (when (featurep 'annalist)
            (annalist-record 'evil-collection 'keybindings
                             ;; Record the binding as if it was in 'normal mode
                             ;; instead of 'operator mode as the user would be
                             ;; in normal mode when triggering the operator.
                             (list map-sym 'normal key-with-prefix def)
                             :local (or (eq map-sym 'local)
                                        (local-variable-p map-sym))))
          ;; Use the original key declared when actually setting the binding.
          (push key filtered-bindings)
          ;; Use the definition attached to the menu-item when setting the
          ;; binding.
          (push def-with-menu-item filtered-bindings))))
    (setq filtered-bindings (nreverse filtered-bindings))
    (evil-collection--define-key 'operator map-sym filtered-bindings)))

(defun evil-collection--filter-states (state)
  "Return a list states after filtering STATE (a single symbol or list of symbols).
The return value adheres to `evil-collection-state-passlist' and
`evil-collection-state-denylist'. When the STATE is nil, which
means all states for `evil-define-key', return nil."
  (let ((states (if (listp state) state (list state))))
    (seq-difference
     (if evil-collection-state-passlist
         (seq-intersection states evil-collection-state-passlist)
       states)
     evil-collection-state-denylist)))

(defun evil-collection--keys-conflict (key-a key-b)
  "Return t if KEY-A and KEY-B conflict.
Keys conflict if they're equal or if one is a prefix of the other."
  (setq key-a (vconcat key-a)
        key-b (vconcat key-b))
  (let ((len (min (length key-a) (length key-b))))
    (equal (substring key-a 0 len) (substring key-b 0 len))))

(defsubst evil-collection--can-bind-key (key whitelist blacklist)
  "Return t if KEY can be bound.
Return nil if KEY conflicts with a key in BLACKLIST and is not
a member of WHITELIST.

Both WHITELIST and BLACKLIST must be lists of keys in Emacs'
internal key representation (i.e., after calling `kbd' on the key
description."
  (or (member key whitelist)
      (not (cl-member key blacklist
                      :test #'evil-collection--keys-conflict))))

(defun evil-collection-define-key (state map-sym &rest bindings)
  "Wrapper for `evil-define-key*' with additional features.
Unlike `evil-define-key*' MAP-SYM should be a quoted keymap other than the
unquoted keymap required for `evil-define-key*'. This function adds the ability
to filter keys on the basis of `evil-collection-key-whitelist' and
`evil-collection-key-blacklist'. It also records bindings with annalist.el."
  (declare (indent defun))
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         (states-to-bind (evil-collection--filter-states state))
         filtered-bindings)
    (when (or states-to-bind (null state))
      (while bindings
        (let ((key (pop bindings))
              (def (pop bindings)))
          (when (evil-collection--can-bind-key key whitelist blacklist)
            (when (featurep 'annalist)
              (annalist-record 'evil-collection 'keybindings
                               (list map-sym state key def)
                               :local (or (eq map-sym 'local)
                                          (local-variable-p map-sym))))
            (push key filtered-bindings)
            (push def filtered-bindings))))
      (setq filtered-bindings (nreverse filtered-bindings))
      (evil-collection--define-key states-to-bind map-sym filtered-bindings))))

(defun evil-collection-define-minor-mode-key (state mode &rest bindings)
  "Wrapper for `evil-define-minor-mode-key' with additional features.
Like `evil-collection-define-key' but binds to MODE (a minor-mode
symbol) rather than a keymap.  Bindings auto-activate when MODE is
enabled without relying on `evil-normalize-keymaps'.

Filters keys via `evil-collection-key-whitelist' /
`evil-collection-key-blacklist' and records bindings with annalist."
  (declare (indent defun))
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist))
         (states-to-bind (evil-collection--filter-states state))
         filtered-bindings)
    (when (or states-to-bind (null state))
      (while bindings
        (let ((key (pop bindings))
              (def (pop bindings)))
          (when (evil-collection--can-bind-key key whitelist blacklist)
            (when (featurep 'annalist)
              (annalist-record 'evil-collection 'keybindings
                               (list mode state key def)))
            (push key filtered-bindings)
            (push def filtered-bindings))))
      (setq filtered-bindings (nreverse filtered-bindings))
      (when filtered-bindings
        (apply #'evil-define-minor-mode-key
               states-to-bind mode filtered-bindings)))))

(defun evil-collection-can-bind-key (key)
  "Return whether or not we should bind KEY."
  (let* ((whitelist (mapcar 'kbd evil-collection-key-whitelist))
         (blacklist (mapcar 'kbd evil-collection-key-blacklist)))
    (evil-collection--can-bind-key key whitelist blacklist)))

(defun evil-collection--define-key (state map-sym bindings)
  "Workhorse function for `evil-collection-define-key'.

See `evil-collection-define-key' docstring for more details."
  (cond ((null bindings))
        ((and (boundp map-sym) (keymapp (symbol-value map-sym)))
         (condition-case-unless-debug err
             (apply #'evil-define-key*
                    state (symbol-value map-sym) bindings)
           (error
            (message "evil-collection: error setting key in %s %S"
                     map-sym err))))
        ((boundp map-sym)
         (user-error "evil-collection: %s is not a keymap" map-sym))
        (t
         (let* ((fname (format "evil-collection-define-key-in-%s" map-sym))
                (fun (make-symbol fname)))
           (fset fun `(lambda (&rest args)
                        (when (and (boundp ',map-sym) (keymapp ,map-sym))
                          (remove-hook 'after-load-functions #',fun)
                          (condition-case-unless-debug err
                              (apply #'evil-define-key*
                                     ',state ,map-sym ',bindings)
                            (error
                             (message
                              ,(format
                                "evil-collection: error setting key in %s %%S"
                                map-sym)
                              err))))))
           (add-hook 'after-load-functions fun t)))))

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

(defun evil-collection-set-readonly-bindings (map-sym)
  "Unmap insertion keys from normal state. Additionally q can `quit-window'.
This is particularly useful for read-only modes. Make sure it's
called before setting up other evil bindings so that it can be
overriden."
  (evil-collection-inhibit-insert-state map-sym)
  (evil-collection-define-key 'normal map-sym
    "q"  #'quit-window
    "ZZ" #'quit-window
    "ZQ" #'evil-quit))

(defun evil-collection--binding-lessp (a b)
  "Comparison function used to sort bindings of the form (state key def)."
  (let ((a-state (symbol-name (nth 0 a)))
        (b-state (symbol-name (nth 0 b)))
        (a-key (nth 1 a))
        (b-key (nth 1 b)))
    (if (not (string= a-state b-state))
        (string-lessp a-state b-state)
      (string-lessp a-key b-key))))

(with-eval-after-load 'annalist
  (annalist-define-view 'keybindings 'evil-collection-valid
    (list (list 'keymap :sort #'annalist-string-<)
          (list 'state :sort #'annalist-string-<))
    :inherit 'valid)

  (annalist-define-view 'keybindings 'evil-collection-active
    (list (list 'keymap :sort #'annalist-string-<)
          (list 'state :sort #'annalist-string-<))
    :inherit 'active))

(defun evil-collection-describe-bindings (&optional arg)
  "Print bindings made by Evil Collection to separate buffer.

With non-nil ARG, restrict to bindings corresponding to active
modes in the current buffer.

Requires the optional `annalist' package."
  (interactive "P")
  (unless (require 'annalist nil t)
    (user-error
     "`evil-collection-describe-bindings' requires the `annalist' package"))
  (annalist-describe 'evil-collection 'keybindings
                     (if arg
                         'evil-collection-active
                       'evil-collection-valid)))

(defun evil-collection--mode-file (mode file)
  "Return path to FILE for MODE. Return nil if it doesn't exist."
  (let ((path (expand-file-name
               (format "modes/%s/%s" mode file) evil-collection-base-dir)))
    (when (file-exists-p path)
      path)))

(defun evil-collection-open-config-file (mode)
  "Open configuration file corresponding to MODE."
  (interactive
   (list
    (completing-read
     "Mode: "
     (cl-remove-if-not
      (lambda (mode)
        (evil-collection--mode-file mode (format "evil-collection-%s.el" mode)))
      (directory-files
       (expand-file-name "modes" evil-collection-base-dir)
       nil "^[^.]")))))
  (find-file (evil-collection--mode-file mode (format "evil-collection-%s.el" mode))))

(defun evil-collection-open-readme (mode)
  "Open README.org corresponding to MODE."
  (interactive
   (list
    (completing-read
     "Mode: "
     (cl-remove-if-not
      (lambda (mode)
        (evil-collection--mode-file mode "README.org"))
      (directory-files
       (expand-file-name "modes" evil-collection-base-dir)
       nil "^[^.]")))))
  (find-file (evil-collection--mode-file mode "README.org")))

(defun evil-collection--delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK.  If APPEND is
non-nil, the entry is appended to the hook.  If LOCAL is non-nil,
the buffer-local value of HOOK is modified.

This is a backport of `evil-delay' without the deprecation notice to deal with
CI until migration can be done.
Ref: https://github.com/emacs-evil/evil-collection/issues/750"
  (eval `(evil-with-delay ,condition (,hook ,append ,local ,name) ,form) t))

;;;###autoload
(cl-defun evil-collection-translate-minor-mode-key (states modes
                                                           &rest translations
                                                           &key destructive
                                                           &allow-other-keys)
  "Translate keys in the keymap(s) corresponding to STATES and MODES.

Similar to `evil-collection-translate-key' but for minor modes.
STATES should be the name of an evil state, a list of states, or nil. MODES
should be a symbol corresponding to minor-mode to make the translations in or a
list of minor-mode symbols. TRANSLATIONS corresponds to a list of
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
  (unless (listp modes)
    (setq modes (list modes)))
  (unless (and (listp states)
               (not (null states)))
    (setq states (list states)))
  (dolist (mode-symbol modes)
    (let ((keymap-symbol (intern (format "%S-map" mode-symbol))))
      (dolist (state states)
        (let ((hook-name
               (symbol-name
                (gensym
                 (format "evil-collection-translate-key-in-%s" keymap-symbol)))))
          (evil-collection--delay `(and (boundp ',keymap-symbol)
                                        (keymapp ,keymap-symbol))
                                  `(evil-collection--translate-minor-mode-key
                                    ',state
                                    ',mode-symbol
                                    ',translations
                                    ,destructive)
                                  'after-load-functions
                                  t
                                  nil
                                  hook-name))))))


(defun evil-collection--translate-minor-mode-key (state
                                                  mode-symbol
                                                  translations
                                                  destructive)
  "Helper function for `evil-collection-translate-minor-mode-key'.
In the minor mode keymap corresponding to STATE and MODE-SYMBOL, make the key
TRANSLATIONS. When DESTRUCTIVE is non-nil, make the TRANSLATIONS destructively
without creating/referencing a backup keymap."
  (let* ((keymap-symbol (intern (format "%S-map" mode-symbol)))
         (backup-keymap-symbol (intern (format "evil-collection-%s%s-backup-map"
                                               mode-symbol
                                               (if state
                                                   (format "-%s-state" state)
                                                 ""))))
         (keymap (symbol-value keymap-symbol))
         (lookup-keymap (if (and (not destructive)
                                 (boundp backup-keymap-symbol))
                            (symbol-value backup-keymap-symbol)
                          (copy-keymap
                           (if state
                               (evil-get-minor-mode-keymap state mode-symbol)
                             keymap))))
         (maps (cl-loop for (key replacement) on translations by 'cddr
                        ;; :destructive can be in TRANSLATIONS
                        unless (keywordp key)
                        collect key
                        and collect (when replacement
                                      (evil-lookup-key lookup-keymap replacement)))))
    (unless (or destructive
                (boundp backup-keymap-symbol))
      (set backup-keymap-symbol lookup-keymap))
    (apply #'evil-define-minor-mode-key state mode-symbol maps)))

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
                                      (evil-lookup-key lookup-keymap replacement)))))
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
`with-eval-after-load' is not necessary. TRANSLATIONS corresponds to a list of
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
      (let ((hook-name
             (symbol-name
              (gensym
               (format "evil-collection-translate-key-in-%s" keymap-symbol)))))
        (evil-collection--delay `(and (boundp ',keymap-symbol)
                                      (keymapp ,keymap-symbol))
                                `(evil-collection--translate-key
                                  ',state
                                  ',keymap-symbol
                                  ',translations
                                  ,destructive)
                                'after-load-functions
                                t
                                nil
                                hook-name)))))


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
(defmacro evil-collection-swap-minor-mode-key (states modes &rest args)
  "Wrapper around `evil-collection-translate-minor-mode-key' for swapping keys.
STATES, MODES, and ARGS are passed to
`evil-collection-translate-minor-mode-key'. ARGS should consist of key swaps
\(e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\" \"a\"
with `evil-collection-translate-minor-mode-key') and optionally keyword
arguments for `evil-collection-translate-minor-mode-key'."
  (declare (indent defun))
  (setq args (cl-loop for (key replacement) on args by 'cddr
                      collect key and collect replacement
                      and unless (keywordp key)
                      collect replacement and collect key))
  `(evil-collection-translate-minor-mode-key ,states ,modes ,@args))

;;;###autoload
(defun evil-collection-require (mode &optional noerror)
  "Require the evil-collection-MODE file, but do not activate it.

MODE should be a symbol. This requires the evil-collection-MODE
feature without needing to manipulate `load-path'. NOERROR is
forwarded to `require'."
  (let* ((mode-name (symbol-name mode))
         (feature (intern (format "evil-collection-%s" mode-name)))
         (file (expand-file-name
                (format "modes/%s/evil-collection-%s" mode-name mode-name)
                evil-collection-base-dir)))
    (require feature file noerror)))

(declare-function evil-collection-unimpaired-setup "evil-collection-unimpaired")

;;;###autoload
(defun evil-collection-init (&optional modes)
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load \='calendar
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'."
  (interactive)
  (evil-collection-maybe-display-startup-warning)
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
          ;; (message (format "Loaded %S..." req))
          (evil-collection-require m)
          (funcall (intern (concat "evil-collection-" (symbol-name m)
                                   "-setup")))
          (let ((mode-keymaps
                 (ignore-errors
                   (symbol-value
                    (intern (format "evil-collection-%s-maps" m))))))
            (when (or evil-collection-always-run-setup-hook-after-load
                      (not (memq m evil-collection--modes-with-delayed-setup)))
              (run-hook-with-args 'evil-collection-setup-hook
                                  m mode-keymaps)))))))
  (when evil-collection-want-unimpaired-p
    (evil-collection-require 'unimpaired)
    (evil-collection-unimpaired-setup)))

(defun evil-collection-setup (&optional modes)
  "Register the Evil bindings for all modes in `evil-collection-mode-list'.

----------------------EXPERIMENTAL------------------------------------------

This is a special wrapper over `evil-collection-init' that respects
configuration from `evil-collection-config'. This function is experimental,
so don't use if you don't want breakages or API changes.

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'.

----------------------EXPERIMENTAL------------------------------------------"
  (if modes
      (or (listp modes) (setq modes (list modes)))
    (setq modes evil-collection-mode-list))
  (let ((configs evil-collection-config)
        (deferred)  ; alist of (mode . delay)
        (hooked))   ; alist of (mode . hook)
    (dolist (config configs)
      (if-let* ((hook (plist-get (cdr config) :hook)))
          (push (cons (car config) hook) hooked)
        (when-let* ((defer (plist-get (cdr config) :defer)))
          (push (cons (car config) defer) deferred))))
    (let ((filtered-modes
           (cl-remove-if
            (lambda (mode)
              ;; `evil-collection-config' format is slightly different
              ;; than `evil-collection-mode-list', so use the mode
              ;; entry from the mode list instead.
              (cl-some
               (lambda (m)
                 (or (when-let* ((entry (assq m deferred)))
                       (setf (car entry) mode) t)
                     (when-let* ((entry (assq m hooked)))
                       (setf (car entry) mode) t)))
               (if (consp mode) mode (list mode))))
            modes)))
      (evil-collection-init filtered-modes))
    (message (format "Deferring: %S" (mapcar #'car deferred)))
    (dolist (entry deferred)
      (let ((mode (car entry))
            (delay (cdr entry)))
        (run-with-idle-timer
         (if (numberp delay) delay evil-collection-defer-delay) nil
         (apply-partially 'evil-collection-init (list mode)))))
    (dolist (entry hooked)
      (let* ((mode (car entry))
             (hook-sym (cdr entry))
             (hook-name (let ((name (symbol-name hook-sym)))
                          (if (string-suffix-p "-hook" name)
                              hook-sym
                            (intern (concat name "-hook")))))
             (mode-sym (if (consp mode) (car mode) mode))
             (fn-sym (intern (format "evil-collection--init-%s" mode-sym))))
        (defalias fn-sym
          (lambda ()
            (remove-hook hook-name fn-sym)
            (evil-collection-init (list mode))))
        (add-hook hook-name fn-sym)))))

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
;;* Unmap

(defvar evil-collection-unmap-skip-events
  '(menu-bar tool-bar tab-bar tab-line header-line mode-line)
  "Events under which `evil-collection-unmap' will not descend.
These are UI keymaps where removing entries is rarely the intent.")

;;;###autoload
(defun evil-collection-unmap (map-sym &rest things)
  "Completely unmap THINGS from MAP-SYM.

MAP-SYM is a symbol bound to a keymap. If MAP-SYM is not yet bound
to a keymap, the operation is deferred via `after-load-functions',
matching the deferral behaviour of `evil-collection-define-key'.

Each thing in THINGS is one of:

  - A command symbol — every binding pointing to that command
    anywhere in the keymap is removed (TAB, RET, C-c TAB,
    evil-state bindings, [remap ...] entries, etc.).

  - A key description string accepted by `kbd' (e.g. \"RET\",
    \"a\", \"<return>\", \"TAB\", \"C-c C-c\") — the binding at
    that exact key sequence is removed at the top level and
    inside every evil-state auxiliary keymap.

  - A key vector — same as a key description, already parsed.

Removals use `define-key' with REMOVE set, so bindings are
deleted entirely, not just shadowed with nil.

Parent keymaps installed via `set-keymap-parent' are not modified,
and sub-keymaps stored under events in
`evil-collection-unmap-skip-events' (menu-bar, tool-bar, etc.) are
not descended into."
  (declare (indent defun))
  (cond
   ((and (boundp map-sym) (keymapp (symbol-value map-sym)))
    (evil-collection--unmap (symbol-value map-sym) things))
   ((boundp map-sym)
    (user-error "evil-collection-unmap: %s is not a keymap" map-sym))
   (t
    (let* ((fname (format "evil-collection-unmap-in-%s" map-sym))
           (fun (make-symbol fname)))
      (fset fun
            `(lambda (&rest _)
               (when (and (boundp ',map-sym) (keymapp ,map-sym))
                 (remove-hook 'after-load-functions #',fun)
                 (condition-case-unless-debug err
                     (evil-collection--unmap
                      (symbol-value ',map-sym) ',things)
                   (error
                    (message
                     ,(format
                       "evil-collection-unmap: error unmapping in %s %%S"
                       map-sym)
                     err))))))
      (add-hook 'after-load-functions fun t)))))

(defun evil-collection--unmap (keymap things)
  "Workhorse for `evil-collection-unmap'."
  (let (commands keys)
    (dolist (thing things)
      (cond
       ((null thing))
       ((symbolp thing) (push thing commands))
       ((vectorp thing) (push thing keys))
       ((stringp thing) (push (kbd thing) keys))
       (t (user-error "evil-collection-unmap: cannot unmap %S" thing))))
    ;; --- Remove by command (deep walk) -----------------------------------
    (when commands
      (let (paths)
        (evil-collection--walk-keymap-shallow
         keymap []
         (lambda (key-vec binding)
           (let ((real (if (and (consp binding)
                                (eq (car-safe binding) 'menu-item))
                           (nth 2 binding)
                         binding)))
             (when (memq real commands)
               (push (copy-sequence key-vec) paths)))))
        ;; Remove deeper paths first so we don't yank a prefix from under
        ;; entries we still want to remove.
        (setq paths (sort paths (lambda (a b) (> (length a) (length b)))))
        (dolist (path paths)
          (ignore-errors (define-key keymap path nil t)))))
    ;; --- Remove by key (top-level + evil state aux maps) -----------------
    (dolist (key keys)
      (ignore-errors (define-key keymap key nil t))
      (dolist (entry (cdr keymap))
        (when (and (consp entry)
                   (symbolp (car entry))
                   (keymapp (cdr entry))
                   (string-suffix-p "-state" (symbol-name (car entry))))
          (ignore-errors (define-key (cdr entry) key nil t)))))))

(defun evil-collection--walk-keymap-shallow (keymap prefix fn)
  "Walk KEYMAP under PREFIX, calling FN with (KEY-VEC BINDING).

Stops at the parent-keymap boundary (does NOT follow inherited
keymaps). Skips sub-keymaps whose event is in
`evil-collection-unmap-skip-events'."
  (let ((tail (cdr keymap)))
    (while (and tail (not (eq (car-safe tail) 'keymap)))
      (let ((entry (car tail)))
        (cond
         ((stringp entry))                  ; prompt — skip
         ((char-table-p entry)
          (map-char-table
           (lambda (ev binding)
             (when binding
               (cond
                ((characterp ev)
                 (evil-collection--walk-keymap-entry ev binding prefix fn))
                ((and (consp ev)
                      (characterp (car ev))
                      (characterp (cdr ev)))
                 (let ((c (car ev)) (end (cdr ev)))
                   (while (<= c end)
                     (evil-collection--walk-keymap-entry c binding prefix fn)
                     (setq c (1+ c))))))))
           entry))
         ((vectorp entry)
          (let ((i 0) (len (length entry)))
            (while (< i len)
              (let ((b (aref entry i)))
                (when b
                  (evil-collection--walk-keymap-entry i b prefix fn)))
              (setq i (1+ i)))))
         ((consp entry)
          (let* ((ev (car entry))
                 (rest (cdr entry))
                 ;; Strip optional item-string in (EVENT ITEM-STRING . BINDING)
                 (binding (if (and (consp rest) (stringp (car rest)))
                              (cdr rest)
                            rest)))
            (evil-collection--walk-keymap-entry ev binding prefix fn)))))
      (setq tail (cdr tail)))))

(defun evil-collection--walk-keymap-entry (event binding prefix fn)
  "Dispatch on BINDING type for `evil-collection--walk-keymap-shallow'."
  (let ((key (vconcat prefix (vector event))))
    (cond
     ((memq event evil-collection-unmap-skip-events))
     ((keymapp binding)
      (evil-collection--walk-keymap-shallow binding key fn))
     ((and (consp binding)
           (eq (car-safe binding) 'menu-item)
           (keymapp (nth 2 binding)))
      (evil-collection--walk-keymap-shallow (nth 2 binding) key fn))
     (binding
      (funcall fn key binding)))))

(provide 'evil-collection)
;;; evil-collection.el ends here
