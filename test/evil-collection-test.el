;;; evil-collection-test.el --- Tests for evil-collection -*- lexical-binding: t -*-
(require 'evil-collection)

(ert-deftest evil-collection-zero-test ()
  "Zero check blank test."
  (should (equal 0 0)))

(ert-deftest evil-collection-filtering-states-test ()
  "Test `evil-collection--filter-states'."
  (let ((evil-collection-state-denylist '())
        (evil-collection-state-passlist '()))
    (should
     (equal nil
            (evil-collection--filter-states nil)))
    (should
     (equal '(normal)
            (evil-collection--filter-states 'normal)))
    (should
     (equal '(normal)
            (evil-collection--filter-states '(normal)))))
  (let ((evil-collection-state-denylist '(insert))
        (evil-collection-state-passlist '()))
    (should
     (equal '()
            (evil-collection--filter-states 'insert)))
    (should
     (equal '(visual)
            (evil-collection--filter-states '(visual insert)))))
  (let ((evil-collection-state-denylist '(insert))
        (evil-collection-state-passlist '(normal visual)))
    (should
     (equal '()
            (evil-collection--filter-states '())))
    (should
     (equal '(visual)
            (evil-collection--filter-states '(insert visual))))
    (should
     (seq-set-equal-p '(visual normal)
                      (evil-collection--filter-states '(motion normal visual insert))))))


;;; Theme mechanism

(ert-deftest evil-collection-binding-defaults-lookup ()
  "Resolver returns the default plist values when no override is set."
  (let ((evil-collection-binding-defaults
         '((demo :enabled t :state (normal insert) :key "C-c C-z")))
        (evil-collection-binding-overrides nil))
    (should (eq t (evil-collection-binding-enabled-p 'demo)))
    (should (equal '(normal insert) (evil-collection-binding-states 'demo)))
    (should (equal '("C-c C-z") (evil-collection-binding-keys 'demo)))))

(ert-deftest evil-collection-theme-override-wins-per-property ()
  "Override property replaces the default; unset properties fall through."
  (let ((evil-collection-binding-defaults
         '((demo :enabled t :state (normal insert) :key "C-c C-z")))
        (evil-collection-binding-overrides
         '((demo :state normal))))
    (should (eq t (evil-collection-binding-enabled-p 'demo)))
    (should (equal '(normal) (evil-collection-binding-states 'demo)))
    (should (equal '("C-c C-z") (evil-collection-binding-keys 'demo)))))

(ert-deftest evil-collection-theme-override-multiple-props ()
  "Overrides may replace multiple properties at once."
  (let ((evil-collection-binding-defaults
         '((demo :enabled t :state (normal insert) :key "C-c C-z")))
        (evil-collection-binding-overrides
         '((demo :state normal :key ("C-c j" "C-c k")))))
    (should (equal '(normal) (evil-collection-binding-states 'demo)))
    (should (equal '("C-c j" "C-c k") (evil-collection-binding-keys 'demo)))))

(ert-deftest evil-collection-theme-enabled-nil-disables ()
  "An explicit :enabled nil disables the feature and beats the default t."
  (let ((evil-collection-binding-defaults '((demo :enabled t)))
        (evil-collection-binding-overrides '((demo :enabled nil))))
    (should (null (evil-collection-binding-enabled-p 'demo)))))

(ert-deftest evil-collection-theme-enabled-function ()
  ":enabled may be a function; it is funcalled at lookup time."
  (let ((evil-collection-binding-defaults
         `((demo :enabled ,(lambda () (= 1 1)))
           (off  :enabled ,(lambda () nil))))
        (evil-collection-binding-overrides nil))
    (should (eq t (evil-collection-binding-enabled-p 'demo)))
    (should (null (evil-collection-binding-enabled-p 'off)))))

(ert-deftest evil-collection-theme-enabled-missing-defaults-to-t ()
  "Missing :enabled is treated as t."
  (let ((evil-collection-binding-defaults '((demo :state normal)))
        (evil-collection-binding-overrides nil))
    (should (eq t (evil-collection-binding-enabled-p 'demo))))
  (let ((evil-collection-binding-defaults nil)
        (evil-collection-binding-overrides nil))
    (should (eq t (evil-collection-binding-enabled-p 'never-declared)))))

(ert-deftest evil-collection-theme-state-normalization ()
  "Singular :state is wrapped to a list; list passes through; nil stays nil."
  (let ((evil-collection-binding-overrides nil))
    (let ((evil-collection-binding-defaults '((demo :state normal))))
      (should (equal '(normal) (evil-collection-binding-states 'demo))))
    (let ((evil-collection-binding-defaults '((demo :state (normal insert)))))
      (should (equal '(normal insert) (evil-collection-binding-states 'demo))))
    (let ((evil-collection-binding-defaults '((demo :state nil))))
      (should (null (evil-collection-binding-states 'demo))))
    (let ((evil-collection-binding-defaults '((demo))))
      (should (null (evil-collection-binding-states 'demo))))))

(ert-deftest evil-collection-theme-key-normalization ()
  "Singular :key is wrapped to a list; list passes through; nil stays nil."
  (let ((evil-collection-binding-overrides nil))
    (let ((evil-collection-binding-defaults '((demo :key "C-c C-z"))))
      (should (equal '("C-c C-z") (evil-collection-binding-keys 'demo))))
    (let ((evil-collection-binding-defaults '((demo :key ("C-c C-z" "C-c j")))))
      (should (equal '("C-c C-z" "C-c j") (evil-collection-binding-keys 'demo))))
    (let ((evil-collection-binding-defaults '((demo :key nil))))
      (should (null (evil-collection-binding-keys 'demo))))))

(ert-deftest evil-collection-theme-unknown-id ()
  "Unknown ids: enabled-p returns t (missing -> t); states/keys return nil."
  (let ((evil-collection-binding-defaults nil)
        (evil-collection-binding-overrides nil))
    (should (eq t (evil-collection-binding-enabled-p 'no-such-thing)))
    (should (null (evil-collection-binding-states 'no-such-thing)))
    (should (null (evil-collection-binding-keys 'no-such-thing)))))

(ert-deftest evil-collection-theme-override-introduces-new-id ()
  "An override may declare an id not present in the defaults."
  (let ((evil-collection-binding-defaults nil)
        (evil-collection-binding-overrides
         '((novel :enabled t :state normal :key "X"))))
    (should (eq t (evil-collection-binding-enabled-p 'novel)))
    (should (equal '(normal) (evil-collection-binding-states 'novel)))
    (should (equal '("X") (evil-collection-binding-keys 'novel)))))

(ert-deftest evil-collection-bind-skips-when-disabled ()
  "`evil-collection-bind' is a no-op when the entry is disabled."
  (let ((evil-collection-binding-defaults '((demo :enabled t :state normal :key "X")))
        (evil-collection-binding-overrides '((demo :enabled nil)))
        (test-map (make-sparse-keymap))
        (test-map-sym nil))
    (defvar evil-collection-theme-test--map nil)
    (setq evil-collection-theme-test--map test-map
          test-map-sym 'evil-collection-theme-test--map)
    (evil-collection-bind 'demo test-map-sym #'ignore)
    (should (null (lookup-key test-map (kbd "X"))))))

(ert-deftest evil-collection-bind-binds-all-keys ()
  "Every key in :key gets bound across every state in :state."
  (let ((evil-collection-binding-defaults
         '((demo :enabled t :state (normal insert) :key ("X" "Y"))))
        (evil-collection-binding-overrides nil))
    (defvar evil-collection-theme-test--map2 nil)
    (setq evil-collection-theme-test--map2 (make-sparse-keymap))
    (evil-collection-bind 'demo
                                'evil-collection-theme-test--map2
                                #'ignore)
    (dolist (state '(normal insert))
      (dolist (key '("X" "Y"))
        (should (eq #'ignore
                    (lookup-key (evil-get-auxiliary-keymap
                                 evil-collection-theme-test--map2 state)
                                (kbd key))))))))

(ert-deftest evil-collection-theme-shipped-term-toggle-escape ()
  "Sanity check the shipped default entry for `term-toggle-escape'."
  (should (eq t (evil-collection-binding-enabled-p 'term-toggle-escape)))
  (should (equal '(normal insert)
                 (evil-collection-binding-states 'term-toggle-escape)))
  (should (equal '("C-c C-z")
                 (evil-collection-binding-keys 'term-toggle-escape))))

(ert-deftest evil-collection-theme-shipped-action-keys ()
  "Sanity check the shipped default entries for action bindings."
  (dolist (id '(action action-other action-stay))
    (should (eq t (evil-collection-binding-enabled-p id)))
    (should (equal '(normal) (evil-collection-binding-states id))))
  (should (equal '("RET" "<return>")
                 (evil-collection-binding-keys 'action)))
  (should (equal '("S-<return>" "S-RET" "go")
                 (evil-collection-binding-keys 'action-other)))
  (should (equal '("M-<return>" "M-RET" "gO")
                 (evil-collection-binding-keys 'action-stay))))

(ert-deftest evil-collection-theme-function-valued-state ()
  ":state may be a function; it is funcalled at lookup time."
  (let* ((dynamic-state 'normal)
         (evil-collection-binding-defaults
          `((demo :state ,(lambda () dynamic-state))))
         (evil-collection-binding-overrides nil))
    (should (equal '(normal) (evil-collection-binding-states 'demo)))
    (setq dynamic-state 'insert)
    (should (equal '(insert) (evil-collection-binding-states 'demo)))
    (setq dynamic-state '(normal insert))
    (should (equal '(normal insert) (evil-collection-binding-states 'demo)))))

(ert-deftest evil-collection-theme-function-valued-key ()
  ":key may be a function; it is funcalled at lookup time."
  (let* ((dynamic-key "C-c j")
         (evil-collection-binding-defaults
          `((demo :key ,(lambda () dynamic-key))))
         (evil-collection-binding-overrides nil))
    (should (equal '("C-c j") (evil-collection-binding-keys 'demo)))
    (setq dynamic-key '("C-c j" "C-c k"))
    (should (equal '("C-c j" "C-c k") (evil-collection-binding-keys 'demo)))))

(ert-deftest evil-collection-theme-symbol-is-data-not-callable ()
  "Symbols with function bindings (e.g. `insert') are data, not funcalled.
This guards against the trap where `:state insert' would otherwise be
funcalled because `(functionp \\='insert)' returns t."
  (cl-assert (functionp 'insert) nil
             "Test premise: `insert' must be function-bound.")
  (let ((evil-collection-binding-defaults '((demo :state insert)))
        (evil-collection-binding-overrides nil))
    (should (equal '(insert) (evil-collection-binding-states 'demo)))))

(ert-deftest evil-collection-theme-shipped-repl-submit-tracks-legacy-var ()
  "Default `repl-submit' state follows `evil-collection-repl-submit-state'."
  (let ((evil-collection-binding-overrides nil))
    (let ((evil-collection-repl-submit-state 'normal))
      (should (equal '(normal) (evil-collection-binding-states 'repl-submit)))
      (should (equal '(insert) (evil-collection-binding-states 'repl-newline))))
    (let ((evil-collection-repl-submit-state 'insert))
      (should (equal '(insert) (evil-collection-binding-states 'repl-submit)))
      (should (equal '(normal) (evil-collection-binding-states 'repl-newline))))))

(ert-deftest evil-collection-theme-shipped-repl-key-set ()
  "Both `repl-submit' and `repl-newline' default to the unified key set."
  (should (equal '("RET" "<return>" "C-m")
                 (evil-collection-binding-keys 'repl-submit)))
  (should (equal '("RET" "<return>" "C-m")
                 (evil-collection-binding-keys 'repl-newline))))

(ert-deftest evil-collection-theme-repl-submit-overlap-both-states ()
  "Override allows submit to bind in both states simultaneously."
  (let ((evil-collection-binding-overrides
         '((repl-submit  :state (normal insert))
           (repl-newline :enabled nil))))
    (should (equal '(normal insert)
                   (evil-collection-binding-states 'repl-submit)))
    (should (null (evil-collection-binding-enabled-p 'repl-newline)))))

(ert-deftest evil-collection-theme-shipped-repl-force-newline-always-both-states ()
  "Default `repl-force-newline' is in both states regardless of `repl-submit'.
The contract inherited from `evil-collection-repl-submit-state' documents
\"S-RET always inserts a newline regardless of state\"."
  (let ((evil-collection-binding-overrides nil))
    (dolist (legacy '(normal insert))
      (let ((evil-collection-repl-submit-state legacy))
        (should (equal '(normal insert)
                       (evil-collection-binding-states 'repl-force-newline)))))))

(ert-deftest evil-collection-theme-repl-force-newline-user-state-wins ()
  "Explicit `repl-force-newline' :state replaces the default."
  (let ((evil-collection-binding-overrides
         '((repl-force-newline :state normal))))
    (should (equal '(normal)
                   (evil-collection-binding-states 'repl-force-newline)))))

(ert-deftest evil-collection-theme-shipped-repl-force-newline-keys ()
  "Default `repl-force-newline' key set."
  (should (equal '("S-<return>" "S-RET")
                 (evil-collection-binding-keys 'repl-force-newline))))

(ert-deftest evil-collection-bind-minor-mode-skips-when-disabled ()
  "`evil-collection-bind-minor-mode' is a no-op when disabled."
  (let ((evil-collection-binding-defaults '((demo :enabled t :state normal :key "X")))
        (evil-collection-binding-overrides '((demo :enabled nil))))
    ;; If the helper called through to `evil-define-minor-mode-key' with an
    ;; unbound mode it would error.  Confirm it stays silent when disabled.
    (should-not (evil-collection-bind-minor-mode
                 'demo 'evil-collection-theme-test--no-such-mode #'ignore))))

;;; evil-collection-test.el ends here
