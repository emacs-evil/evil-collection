;;; evil-collection-lispy.el --- Evil Bindings for Lispy -*- lexical-binding: t -*-

;; Copyright (C) 2018 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
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
;; This is not included in the default init list for `evil-collection'.
;; Add `lispy' to `evil-collection-mode-list' to use this package.
;; e.g. (push 'lispy evil-collection-mode-list)
;; This file is meant to be an experimental ground for `lispy' keybindings
;; and the changes here are intended to be created as a PR to the main `lispy'
;; repository.
;; PRs, tweaks or comments are very welcome!

;;; Code:
(require 'lispy nil t)
(require 'evil-collection)

(defconst evil-collection-lispy-maps '(lispy-mode-map))

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

(defvar evil-collection-lispy-mode-map-special-evil
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (lispy-define-key map "l" 'lispy-right)
    (lispy-define-key map "h" 'lispy-left)
    (lispy-define-key map "f" 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit)))) ;; `lispy-flow' -> w
    (lispy-define-key map "j" 'lispy-down)
    (lispy-define-key map "k" 'lispy-up)
    (lispy-define-key map "d" 'lispy-delete) ;; `lispy-different' -> o
    (lispy-define-key map "o" 'lispy-different) ;; `lispy-other-mode' -> Q
    (lispy-define-key map "p" 'lispy-paste) ;; `lispy-eval-other-window' -> P
    (lispy-define-key map "P" 'lispy-eval-other-window) ;; `lispy-paste' -> p
    (lispy-define-key map "y" 'lispy-new-copy) ;; `lispy-occur' -> /
    (lispy-define-key map "z" 'lh-knight/body)

    ;; outline
    (lispy-define-key map "J" 'lispy-join)     ;; `lispy-outline-next'
    (lispy-define-key map "K" 'lispy-describe) ;; `lispy-outline-prev'
    (lispy-define-key map "L" 'lispy-outline-goto-child)

    ;; Paredit transformations
    (lispy-define-key map ">" 'lispy-slurp-or-barf-right) ;; `lispy-slurp'
    (lispy-define-key map "<" 'lispy-slurp-or-barf-left) ;; `lispy-barf'

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
    (lispy-define-key map "m" 'lispy-view) ;; `lispy-mark-list' -> v

    ;; dialect-specific
    (lispy-define-key map "e" 'lispy-eval)
    (lispy-define-key map "E" 'lispy-eval-and-insert)
    (lispy-define-key map "G" 'lispy-goto-local)
    (lispy-define-key map "g" 'lispy-goto)
    (lispy-define-key map "F" 'lispy-follow t)
    (lispy-define-key map "D" 'pop-tag-mark)
    (lispy-define-key map "A" 'lispy-beginning-of-defun)
    (lispy-define-key map "_" 'lispy-underscore)
    ;; miscellanea
    (define-key map (kbd "SPC") 'lispy-space)
    (lispy-define-key map "i" 'lispy-tab)
    (lispy-define-key map "I" 'lispy-shifttab)
    (lispy-define-key map "N" 'lispy-narrow)
    (lispy-define-key map "W" 'lispy-widen)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map "u" 'lispy-undo)

    (lispy-define-key map "q" 'lispy-x) ;; `lispy-ace-paren' -> f
    (lispy-define-key map "Q" 'lispy-other-mode) ;; `lispy-ace-char' -> t
    (lispy-define-key map "v" 'lispy-mark-list)  ;; `lispy-view' -> m
    (lispy-define-key map "t" 'lispy-ace-char) ;; `lispy-teleport' -> T
    (lispy-define-key map "n" 'lispy-flow) ;; `lispy-new-copy' -> y
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
    (lispy-define-key map "C-J" 'lispy-outline-next) ;; Hmnn...
    (lispy-define-key map "C-K" 'lispy-outline-prev) ;; Hmnn...
    (lispy-define-key map "]" 'lispy-forward)
    (lispy-define-key map "[" 'lispy-backward)
    (lispy-define-key map "{" 'lispy-brackets)
    (lispy-define-key map "^" 'lispy-splice-sexp-killing-backward)
    map)
  "`evil' flavored `lispy' bindings when in special state.")

(defvar evil-collection-lispy-mode-map-evil
  (let ((map (copy-keymap lispy-mode-map-base)))
    (define-key map (kbd "M-)") 'lispy-wrap-round)
    (define-key map (kbd "M-s") 'lispy-splice)
    (define-key map (kbd "M-<up>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-<down>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-r") 'lispy-raise-sexp)
    (define-key map (kbd "M-C") 'lispy-convolute-sexp)
    (define-key map (kbd "M-S") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    (define-key map (kbd "]") 'lispy-forward)
    (define-key map (kbd "[") 'lispy-backward)
    (define-key map (kbd "M-(") 'lispy-wrap-round)
    (define-key map (kbd "M-{") 'lispy-wrap-braces)
    (define-key map (kbd "M-}") 'lispy-wrap-braces)
    (define-key map (kbd "<") 'lispy-slurp-or-barf-left)
    (define-key map (kbd ">") 'lispy-slurp-or-barf-right)
    (define-key map (kbd "M-y") 'lispy-new-copy)
    (define-key map (kbd "<C-return>") 'lispy-open-line)
    (define-key map (kbd "<M-return>") 'lispy-meta-return)
    (define-key map (kbd "M-k") 'lispy-move-up)
    (define-key map (kbd "M-j") 'lispy-move-down)
    (define-key map (kbd "M-o") 'lispy-string-oneline)
    (define-key map (kbd "M-p") 'lispy-clone)
    (define-key map (kbd "M-d") 'lispy-delete)
    map)
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
                (when (memq 'special-evil theme) evil-collection-lispy-mode-map-special-evil)
                (when (memq 'evil theme) evil-collection-lispy-mode-map-evil)
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

(defun evil-collection-lispy-setup ()
  "Set up `evil' bindings for `lispy'."
  (advice-add 'lispy-set-key-theme :override 'evil-collection-lispy-set-key-theme)
  (lispy-set-key-theme '(special-evil evil)))

(provide 'evil-collection-lispy)
;;; evil-collection-lispy.el ends here
