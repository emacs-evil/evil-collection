;;; evil-collection-magit-tests.el --- evil-based key bindings for magit

;; Copyright (C) 2015-2016 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; Homepage: https://github.com/emacs-evil/evil-collection

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
(require 'evil-collection)
(evil-collection-require 'magit)

(ert-deftest evil-collection-magit-mode-map-tests ()
    "Test that original bindings in `evil-collection-magit-mode-map-bindings'
are correct."
  (dolist (binding evil-collection-magit-mode-map-bindings)
    (when (nth 4 binding)
      (should (eq (lookup-key (symbol-value (nth 1 binding)) (kbd (nth 4 binding)))
                  (nth 3 binding)))))
  (dolist (binding evil-collection-magit-minor-mode-map-bindings)
    (when (nth 4 binding)
      (should (eq (lookup-key (symbol-value (nth 1 binding)) (kbd (nth 4 binding)))
                  (nth 3 binding))))))

(ert-deftest evil-collection-magit-section-map-tests ()
  "Test that original bindings in
`evil-collection-magit-original-section-bindings' are correct."
  (dolist (binding evil-collection-magit-original-section-bindings)
    (should (eq (lookup-key (nth 0 binding) (nth 1 binding))
                (nth 2 binding)))))

;; (ert-deftest evil-collection-magit-popup-action-tests ()
;;   "Test that bindings are as expected in popups."
;;   (when evil-collection-magit-popup-keys-changed
;;     (dolist (change evil-collection-magit-popup-changes)
;;       (let ((alist (plist-get (symbol-value (nth 0 change)) (nth 1 change))))
;;         (should
;;          (eq (nth 2 (assoc (string-to-char (nth 3 change)) alist))
;;              (nth 4 change)))))))

(defun evil-collection-magit-collect-magit-section-maps ()
  (let (res)
    (mapatoms
     (lambda (sym)
       (when (string-match-p "^magit-.*-section-map$" (symbol-name sym))
                     (push sym res))))
    res))

(setq evil-collection-magit-section-maps-test (evil-collection-magit-collect-magit-section-maps))
;; (setq evil-collection-magit-commands-in-section-maps
;;       (let (res)
;;         (dolist (map evil-collection-magit-section-maps-test)
;;           (when (and (boundp map) (keymapp (symbol-value map)))
;;             (map-keymap
;;              (lambda (_ def)
;;                (when (commandp def)
;;                  (if res
;;                      (add-to-list 'res def)
;;                    (setq res (list def)))))
;;              (symbol-value map))))
;;         res))

(ert-deftest evil-collection-magit-section-maps-accounted-for ()
  "Check that `evil-collection-magit-section-maps' includes all section-maps
we can find."
  (dolist (map evil-collection-magit-section-maps-test)
    (when (and (boundp map) (keymapp (symbol-value map)))
      (should (memq map evil-collection-magit-section-maps)))))

(defun evil-collection-magit-collect-git-magit-modes ()
  (let (res)
    (mapatoms
     (lambda (sym)
       (when (and (or (boundp sym)
                      (fboundp sym))
                  (string-match-p "^\\(git\\|magit\\)-.*-mode$" (symbol-name sym)))
         (push sym res))))
    res))

(ert-deftest evil-collection-magit-all-modes-accounted-for ()
  "Check that mode lists include all modes we can find."
  (let ((modes (evil-collection-magit-collect-git-magit-modes))
        res)
    (dolist (mode modes)
      (when (boundp mode)
        (should (memq mode
                      (append
                       evil-collection-magit-emacs-to-default-state-modes
                       evil-collection-magit-emacs-to-evil-collection-magit-state-modes
                       evil-collection-magit-default-to-evil-collection-magit-state-modes
                       evil-collection-magit-untouched-modes
                       evil-collection-magit-ignored-modes)))))))

(ert-deftest evil-collection-magit-expand-region-arg-number ()
  "Check that the number of args accepted by
`evil-visual-expand-region' does not change."
  (should-not (evil-visual-expand-region))
  (should-not (evil-visual-expand-region t))
  (should-error (evil-visual-expand-region t t) :type
                'wrong-number-of-arguments))

;;; evil-collection-magit-tests.el ends here
