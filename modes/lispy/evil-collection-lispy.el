;;; evil-collection-lispy.el --- Evil Bindings for Lispy -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, lispy, tools

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
;; Evil bindings for `lispy-mode'.

;;; Code:
(require 'lispy nil t)
(require 'evil-collection)

(defconst evil-collection-lispy-maps '(lispy-mode-map
                                       evil-collection-lispy-mode-map))

(defvar lispy-mode-map-base)
(defvar lispy-mode-map-special)
(defvar lispy-mode-map-lispy)
(defvar lispy-mode-map-paredit)
(defvar lispy-mode-map-parinfer)
(defvar lispy-mode-map-evilcp)
(defvar lispy-mode-map-c-digits)
(defvar lispy-mode-map-oleh)
(defvar lispy-mode-map)
(declare-function lispy-define-key "lispy")
(declare-function lispy-set-key-theme "lispy")

;; FIXME: Decouple this from `lispyville'.
(declare-function lispyville-insert-at-beginning-of-list "lispyville")
(declare-function lispyville-insert-at-end-of-list "lispyville")

(defun evil-collection-lispy-insert-at-end-of-list ()
  "Forward list and enter insert state."
  (interactive)
  (lispyville-insert-at-end-of-list 1))

(defun evil-collection-lispy-insert-at-beginning-of-list ()
  "Backward list and enter insert state."
  (interactive)
  (lispyville-insert-at-beginning-of-list 1))

(defun evil-collection-lispy-action-then-next-sexp (lispy-action)
  "Return function that triggers LISPY-ACTION and then moves to next sexp."
  (defalias (intern (format "%S-then-next-sexp" lispy-action))
    (lambda ()
      (interactive)
      (call-interactively lispy-action)
      (unless (or (lispy-left-p)
                  (lispy-right-p)
                  (and (lispy-bolp)
                       (or (looking-at lispy-outline-header)
                           (looking-at lispy-outline))))
        (call-interactively #'sp-next-sexp)))))

(defun evil-collection-lispy-delete (arg)
  "Copy and delete current sexp.
Passes ARG to `lispy-delete' or `lispy-delete-back'.

Copy of `noc:lispy-delete'."
  (interactive "p")
  (cond ((or (lispy-left-p)
             (region-active-p))
         (lispy-new-copy)
         (lispy-delete arg))
        ((lispy-right-p)
         (lispy-new-copy)
         (lispy-delete-backward arg))))

(defhydra g-knight (:color blue :hint nil :idle .3 :columns 3)
  "g knight"
  ("j" lispy-knight-down "Down")
  ("k" lispy-knight-up "Up")
  ("g" lispy-beginning-of-defun "Beginning")
  ("d" lispy-goto "Goto")
  ("l" lispy-goto-local "Goto Local"))

(defhydra lispy-tab-hydra (:color blue :hint nil :idle .3)
  "Tab"
  ("i" lispy-tab "Tab")
  ("s" lispy-shifttab "Shifttab"))

(defvar evil-collection-lispy-mode-map-special
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (lispy-define-key map "l" 'lispy-right)
    (lispy-define-key map "h" 'lispy-left)
    (lispy-define-key map "f" 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit)))) ;; `lispy-flow' -> w
    (lispy-define-key map "j" 'lispy-down)
    (lispy-define-key map "k" 'lispy-up)

    (lispy-define-key map
        "d" (evil-collection-lispy-action-then-next-sexp
             'evil-collection-lispy-delete)) ;; `lispy-different' -> o

    (lispy-define-key map "o" 'lispy-different) ;; `lispy-other-mode' -> Q
    (lispy-define-key map "p" 'lispy-paste) ;; `lispy-eval-other-window' -> P
    (lispy-define-key map "P" 'lispy-eval-other-window) ;; `lispy-paste' -> p
    (lispy-define-key map "y" 'lispy-new-copy)          ;; `lispy-occur' -> /
    (lispy-define-key map "z" 'lispy-view) ;; `lispy-mark-list' -> v

    ;; outline
    (lispy-define-key map "J" 'lispy-join)     ;; `lispy-outline-next'
    (lispy-define-key map "K" 'lispy-describe) ;; `lispy-outline-prev'
    (lispy-define-key map "L" 'lispy-outline-goto-child)

    ;; Paredit transformations
    (lispy-define-key map ">" 'lispy-slurp-or-barf-right) ;; `lispy-slurp'
    (lispy-define-key map "<" 'lispy-slurp-or-barf-left)  ;; `lispy-barf'

    ;; FIXME: This doesn't work for me for some reason...
    (lispy-define-key map "/" 'lispy-occur) ;; `lispy-x' -> q

    (lispy-define-key map "r" 'lispy-raise)
    (lispy-define-key map "R" 'lispy-raise-some)

    (lispy-define-key map "+" 'lispy-join) ;; Hmnn this can be something else.

    ;; more transformations
    (lispy-define-key map "C" 'lispy-convolute)
    (lispy-define-key map "X" 'lispy-convolute-left)
    (lispy-define-key map "w" 'lispy-move-up)
    (lispy-define-key map "s" 'lispy-move-down)
    (lispy-define-key map "O" 'lispy-oneline)
    (lispy-define-key map "M" 'lispy-alt-multiline)
    (lispy-define-key map "S" 'lispy-stringify)

    ;; marking
    (lispy-define-key map "a" 'lispy-ace-symbol
      :override '(cond ((looking-at lispy-outline)
                        (lispy-meta-return))))
    (lispy-define-key map "H" 'lispy-ace-symbol-replace)

    ;; dialect-specific
    (lispy-define-key map "e" 'lispy-eval)
    (lispy-define-key map "E" 'lispy-eval-and-insert)

    ;; Hmnn, not sure why there's no `lispy-end-of-defun'.
    ;; `end-of-defun' doesn't work quite right yet. It exits the list
    ;; which would exit lispy state.
    (lispy-define-key map "G" 'end-of-defun) ;; `lispy-goto-local' -> gl
    (lispy-define-key map "g" 'g-knight/body) ;; `lispy-goto' -> gd

    (lispy-define-key map "A" 'evil-collection-lispy-insert-at-end-of-list) ;; `lispy-beginning-of-defun' -> gg
    (lispy-define-key map "I" 'evil-collection-lispy-insert-at-beginning-of-list) ;; `lispy-shifttab' -> zs

    (lispy-define-key map "F" 'lispy-follow t)
    (lispy-define-key map "D" 'pop-tag-mark)
    (lispy-define-key map "_" 'lispy-underscore)

    ;; miscellanea
    (define-key map (kbd "SPC") 'lispy-space)
    (lispy-define-key map "TAB" 'lispy-tab-hydra/body) ;; `lh-knight/body'  -> g

    (lispy-define-key map "N" 'lispy-narrow)
    (lispy-define-key map "W" 'lispy-widen)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map "u" 'lispy-undo)

    (lispy-define-key map "q" 'lispy-x)          ;; `lispy-ace-paren' -> f
    (lispy-define-key map "Q" 'lispy-other-mode) ;; `lispy-ace-char' -> t
    (lispy-define-key map "v" 'lispy-mark-list)  ;; `lispy-view' -> z
    (lispy-define-key map "t" 'lispy-ace-char)   ;; `lispy-teleport' -> T
    (lispy-define-key map "n" 'lispy-flow)       ;; `lispy-new-copy' -> y
    (lispy-define-key map "b" 'lispy-back)

    (lispy-define-key map "B" 'lispy-ediff-regions)
    (lispy-define-key map "x" 'lispy-splice) ;; `lispy-x' -> q

    (lispy-define-key map "Z" 'lispy-edebug-stop)
    (lispy-define-key map "V" 'lispy-visit)
    (lispy-define-key map "-" 'lispy-ace-subword)
    (lispy-define-key map "." 'lispy-repeat)
    (lispy-define-key map "~" 'lispy-tilde)
    ;; digit argument
    (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))

    ;; additional
    (lispy-define-key map "T" 'lispy-teleport
      :override '(cond ((looking-at lispy-outline)
                        (end-of-line))))
    (lispy-define-key map "]" 'lispy-forward)
    (lispy-define-key map "[" 'lispy-backward)
    (lispy-define-key map "{" 'lispy-brackets)

    ;; Experimental
    (lispy-define-key map "C-J" 'lispy-outline-next)
    (lispy-define-key map "C-K" 'lispy-outline-prev)
    (lispy-define-key map "^" 'lispy-splice-sexp-killing-backward)
    (lispy-define-key map "$" 'lispy-splice-sexp-killing-forward)
    (lispy-define-key map "M-j" 'lispy-move-down) ;; `lispy-split'
    (lispy-define-key map "M-k" 'lispy-move-up) ;; `lispy-kill-sentence'
    map)
  "`evil' flavored `lispy' bindings when in special state.")

(defvar evil-collection-lispy-mode-map (make-sparse-keymap)
  "`evil' flavored `lispy-mode' bindings.")

(defun evil-collection-lispy-set-key-theme (theme)
  "Set `lispy-mode-map' for according to THEME.
THEME is a list of choices: 'special, 'lispy, 'paredit, 'evilcp,
'c-digits', 'special-evil', 'evil'.

This is an exact copy of `lispy-set-key-theme' except with the additions of
'special-evil' and 'evil' themes."
  (setq lispy-mode-map
        (make-composed-keymap
         (delq nil
               (list
                (when (memq 'special-evil theme) evil-collection-lispy-mode-map-special)
                (when (memq 'evil theme) evil-collection-lispy-mode-map)
                (when (memq 'special theme) lispy-mode-map-special)
                (when (memq 'lispy theme) lispy-mode-map-lispy)
                (when (memq 'paredit theme) lispy-mode-map-paredit)
                (when (memq 'parinfer theme) lispy-mode-map-parinfer)
                (when (memq 'evilcp theme) lispy-mode-map-evilcp)
                (when (memq 'c-digits theme) lispy-mode-map-c-digits)
                (when (memq 'oleh theme) lispy-mode-map-oleh)))))
  (setcdr
   (assq 'lispy-mode minor-mode-map-alist)
   lispy-mode-map))

;;;###autoload
(defun evil-collection-lispy-setup ()
  "Set up `evil' bindings for `lispy'."
  (advice-add 'lispy-set-key-theme :override 'evil-collection-lispy-set-key-theme)
  (lispy-set-key-theme '(special-evil evil))
  (evil-collection-define-key 'normal 'evil-collection-lispy-mode-map
    ;; Instead of inheriting, explicitly define keys from `lispy-mode-map-base'.
    ;; navigation.
    ;; Commented out define-keys are from the original map that we're not using.
    ;; -- Begin Inheritance
    ;; (define-key map (kbd "C-a") 'lispy-move-beginning-of-line)
    ;; (define-key map (kbd "C-e") 'lispy-move-end-of-line)
    (kbd "M-n") 'lispy-left
    ;; killing
    ;; (define-key map (kbd "C-k") 'lispy-kill)
    (kbd "M-d") 'lispy-kill-word
    (kbd "M-DEL") 'lispy-backward-kill-word
    ;; misc
    "(" 'lispy-parens
    ";" 'lispy-comment
    ;; (kbd "M-q") 'lispy-fill
    ;; (define-key map (kbd "C-j") 'lispy-newline-and-indent)
    ;; (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    ;; tags
    "gd" 'lispy-goto-symbol
    (kbd "C-t") 'pop-tag-mark
    ;; -- End Inheritance.

    (kbd "M-)") 'lispy-wrap-round
    (kbd "M-s") 'lispy-splice
    (kbd "M-<up>") 'lispy-splice-sexp-killing-backward
    (kbd "M-<down>") 'lispy-splice-sexp-killing-forward
    (kbd "M-r") 'lispy-raise-sexp
    (kbd "M-R") 'lispy-raise-some
    (kbd "M-C") 'lispy-convolute-sexp
    (kbd "M-S") 'lispy-split
    (kbd "M-J") 'lispy-join
    "]" 'lispy-forward
    "[" 'lispy-backward
    (kbd "M-(") 'lispy-wrap-round
    (kbd "M-{") 'lispy-wrap-braces
    (kbd "M-}") 'lispy-wrap-braces
    "<" 'lispy-slurp-or-barf-left
    ">" 'lispy-slurp-or-barf-right
    (kbd "M-y") 'lispy-new-copy
    (kbd "<C-return>") 'lispy-open-line
    (kbd "<M-return>") 'lispy-meta-return
    (kbd "M-k") 'lispy-move-up
    (kbd "M-j") 'lispy-move-down
    (kbd "M-o") 'lispy-string-oneline
    (kbd "M-p") 'lispy-clone
    (kbd "M-d") 'evil-collection-lispy-delete))

(provide 'evil-collection-lispy)
;;; evil-collection-lispy.el ends here
