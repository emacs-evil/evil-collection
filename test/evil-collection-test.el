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

(ert-deftest evil-collection-theme-defaults-lookup ()
  "Resolver returns the default plist values when no override is set."
  (let ((evil-collection-theme-defaults
         '((demo :enabled t :state (normal insert) :key "C-c C-z")))
        (evil-collection-theme-overrides nil))
    (should (eq t (evil-collection-theme-enabled-p 'demo)))
    (should (equal '(normal insert) (evil-collection-theme-states 'demo)))
    (should (equal '("C-c C-z") (evil-collection-theme-keys 'demo)))))

(ert-deftest evil-collection-theme-override-wins-per-property ()
  "Override property replaces the default; unset properties fall through."
  (let ((evil-collection-theme-defaults
         '((demo :enabled t :state (normal insert) :key "C-c C-z")))
        (evil-collection-theme-overrides
         '((demo :state normal))))
    (should (eq t (evil-collection-theme-enabled-p 'demo)))
    (should (equal '(normal) (evil-collection-theme-states 'demo)))
    (should (equal '("C-c C-z") (evil-collection-theme-keys 'demo)))))

(ert-deftest evil-collection-theme-override-multiple-props ()
  "Overrides may replace multiple properties at once."
  (let ((evil-collection-theme-defaults
         '((demo :enabled t :state (normal insert) :key "C-c C-z")))
        (evil-collection-theme-overrides
         '((demo :state normal :key ("C-c j" "C-c k")))))
    (should (equal '(normal) (evil-collection-theme-states 'demo)))
    (should (equal '("C-c j" "C-c k") (evil-collection-theme-keys 'demo)))))

(ert-deftest evil-collection-theme-enabled-nil-disables ()
  "An explicit :enabled nil disables the feature and beats the default t."
  (let ((evil-collection-theme-defaults '((demo :enabled t)))
        (evil-collection-theme-overrides '((demo :enabled nil))))
    (should (null (evil-collection-theme-enabled-p 'demo)))))

(ert-deftest evil-collection-theme-enabled-function ()
  ":enabled may be a function; it is funcalled at lookup time."
  (let ((evil-collection-theme-defaults
         `((demo :enabled ,(lambda () (= 1 1)))
           (off  :enabled ,(lambda () nil))))
        (evil-collection-theme-overrides nil))
    (should (eq t (evil-collection-theme-enabled-p 'demo)))
    (should (null (evil-collection-theme-enabled-p 'off)))))

(ert-deftest evil-collection-theme-enabled-missing-defaults-to-t ()
  "Missing :enabled is treated as t."
  (let ((evil-collection-theme-defaults '((demo :state normal)))
        (evil-collection-theme-overrides nil))
    (should (eq t (evil-collection-theme-enabled-p 'demo))))
  (let ((evil-collection-theme-defaults nil)
        (evil-collection-theme-overrides nil))
    (should (eq t (evil-collection-theme-enabled-p 'never-declared)))))

(ert-deftest evil-collection-theme-state-normalization ()
  "Singular :state is wrapped to a list; list passes through; nil stays nil."
  (let ((evil-collection-theme-overrides nil))
    (let ((evil-collection-theme-defaults '((demo :state normal))))
      (should (equal '(normal) (evil-collection-theme-states 'demo))))
    (let ((evil-collection-theme-defaults '((demo :state (normal insert)))))
      (should (equal '(normal insert) (evil-collection-theme-states 'demo))))
    (let ((evil-collection-theme-defaults '((demo :state nil))))
      (should (null (evil-collection-theme-states 'demo))))
    (let ((evil-collection-theme-defaults '((demo))))
      (should (null (evil-collection-theme-states 'demo))))))

(ert-deftest evil-collection-theme-key-normalization ()
  "Singular :key is wrapped to a list; list passes through; nil stays nil."
  (let ((evil-collection-theme-overrides nil))
    (let ((evil-collection-theme-defaults '((demo :key "C-c C-z"))))
      (should (equal '("C-c C-z") (evil-collection-theme-keys 'demo))))
    (let ((evil-collection-theme-defaults '((demo :key ("C-c C-z" "C-c j")))))
      (should (equal '("C-c C-z" "C-c j") (evil-collection-theme-keys 'demo))))
    (let ((evil-collection-theme-defaults '((demo :key nil))))
      (should (null (evil-collection-theme-keys 'demo))))))

(ert-deftest evil-collection-theme-unknown-id ()
  "Unknown ids: enabled-p returns t (missing -> t); states/keys return nil."
  (let ((evil-collection-theme-defaults nil)
        (evil-collection-theme-overrides nil))
    (should (eq t (evil-collection-theme-enabled-p 'no-such-thing)))
    (should (null (evil-collection-theme-states 'no-such-thing)))
    (should (null (evil-collection-theme-keys 'no-such-thing)))))

(ert-deftest evil-collection-theme-override-introduces-new-id ()
  "An override may declare an id not present in the defaults."
  (let ((evil-collection-theme-defaults nil)
        (evil-collection-theme-overrides
         '((novel :enabled t :state normal :key "X"))))
    (should (eq t (evil-collection-theme-enabled-p 'novel)))
    (should (equal '(normal) (evil-collection-theme-states 'novel)))
    (should (equal '("X") (evil-collection-theme-keys 'novel)))))

(ert-deftest evil-collection-theme-bind-skips-when-disabled ()
  "`evil-collection-theme-bind' is a no-op when the entry is disabled."
  (let ((evil-collection-theme-defaults '((demo :enabled t :state normal :key "X")))
        (evil-collection-theme-overrides '((demo :enabled nil)))
        (test-map (make-sparse-keymap))
        (test-map-sym nil))
    (defvar evil-collection-theme-test--map nil)
    (setq evil-collection-theme-test--map test-map
          test-map-sym 'evil-collection-theme-test--map)
    (evil-collection-theme-bind 'demo test-map-sym #'ignore)
    (should (null (lookup-key test-map (kbd "X"))))))

(ert-deftest evil-collection-theme-bind-binds-all-keys ()
  "Every key in :key gets bound across every state in :state."
  (let ((evil-collection-theme-defaults
         '((demo :enabled t :state (normal insert) :key ("X" "Y"))))
        (evil-collection-theme-overrides nil))
    (defvar evil-collection-theme-test--map2 nil)
    (setq evil-collection-theme-test--map2 (make-sparse-keymap))
    (evil-collection-theme-bind 'demo
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
  (should (eq t (evil-collection-theme-enabled-p 'term-toggle-escape)))
  (should (equal '(normal insert)
                 (evil-collection-theme-states 'term-toggle-escape)))
  (should (equal '("C-c C-z")
                 (evil-collection-theme-keys 'term-toggle-escape))))

;;; evil-collection-test.el ends here
