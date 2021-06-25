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

;;; evil-collection-test.el ends here
