;;; evil-collection-cider.el --- Evil bindings for Cider -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, cider, tools

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
;; Evil bindings for Cider.

;;; Code:
(require 'cider nil t)
(require 'evil)

(defun evil-collection-cider-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(defun evil-collection-cider-setup ()
  "Set up `evil' bindings for `cider'."
  (unless evil-move-beyond-eol
    (advice-add 'cider-eval-last-sexp :around 'evil-collection-cider-last-sexp)
    (advice-add 'cider-eval-last-sexp-and-replace :around 'evil-collection-cider-last-sexp)
    (advice-add 'cider-eval-last-sexp-to-repl :around 'evil-collection-cider-last-sexp)
    (with-eval-after-load 'cider-eval-sexp-fu
      (advice-add 'cider-esf--bounds-of-last-sexp :around 'evil-collection-cider-last-sexp)))

  (evil-define-key '(normal visual) cider-mode-map
    "gd" 'cider-find-var
    (kbd "C-t") 'cider-pop-back
    "gz" 'cider-switch-to-repl-buffer
    "gf" 'cider-find-resource
    "K" 'cider-doc)

  (with-eval-after-load 'cider-repl
    (evil-define-key '(normal visual) cider-repl-mode-map
      "gz" 'cider-switch-to-last-clojure-buffer
      "gd" 'cider-find-var
      (kbd "C-t") 'cider-pop-back
      "gr" 'cider-refresh
      "gf" 'cider-find-resource
      "K" 'cider-doc))

  (with-eval-after-load 'cider-test
    (evil-define-key 'normal cider-test-report-mode-map
      (kbd "C-c ,") 'cider-test-commands-map
      (kbd "C-c C-t") 'cider-test-commands-map
      (kbd "M-p") 'cider-test-previous-result
      (kbd "M-n") 'cider-test-next-result

      ;; goto
      "gd" 'cider-test-jump

      (kbd "<backtab>") 'cider-test-previous-result
      (kbd "<tab>") 'cider-test-next-result
      (kbd "<return>") 'cider-test-jump
      "t" 'cider-test-jump
      "d" 'cider-test-ediff
      "e" 'cider-test-stacktrace
      "f" 'cider-test-rerun-failed-tests
      "n" 'cider-test-run-ns-tests
      "L" 'cider-test-run-loaded-tests
      "p" 'cider-test-run-project-tests
      "gr" 'cider-test-run-test
      "q" 'cider-popup-buffer-quit-function))

  (with-eval-after-load 'cider-macroexpansion
    (evil-define-key 'normal cider-macroexpansion-mode-map
      ;; quit
      "q" 'cider-popup-buffer-quit-function

      "r" 'cider-macroexpand-again
      "K" 'cider-doc ; Evil has `evil-lookup'.
      "J" 'cider-javadoc
      "." 'cider-find-var
      "m" 'cider-macroexpand-1-inplace
      "a" 'cider-macroexpand-all-inplace
      "u" 'cider-macroexpand-undo
      [remap undo] 'cider-macroexpand-undo))

  (with-eval-after-load 'cider-client
    (evil-define-key 'normal cider-connections-buffer-mode-map
      "d" 'cider-connections-make-default
      "c" 'cider-connection-browser
      "x" 'cider-connections-close-connection
      (kbd "<return>") 'cider-connections-goto-connection
      "g?" 'describe-mode)))

(provide 'evil-collection-cider)
;;; evil-collection-cider.el ends here
