;;; evil-elisp-mode.el --- Bindings for `elisp-mode'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, elisp, lisp

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
;;; Bindings for `elisp-mode'.

;;; Code:
(require 'elisp-mode)
(require 'evil)

(defun evil-elisp-mode-last-sexp-setup-props (beg end value alt1 alt2)
  "Set up text properties for the output of `elisp--eval-last-sexp'.
BEG and END are the start and end of the output in current-buffer.
VALUE is the Lisp value printed, ALT1 and ALT2 are strings for the
alternative printed representations that can be displayed."
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'elisp-last-sexp-toggle-display)
    (define-key map [down-mouse-2] 'mouse-set-point)
    (define-key map [mouse-2] 'elisp-last-sexp-toggle-display)
    (evil-define-key 'insert map "\C-m"
      (lookup-key (current-global-map) (kbd "RET")))
    (add-text-properties
     beg end
     `(printed-value (,value ,alt1 ,alt2)
                     mouse-face highlight
                     keymap ,map
                     help-echo "RET, mouse-2: toggle abbreviated display"
                     rear-nonsticky (mouse-face keymap help-echo
                                                printed-value)))))
(defun evil-elisp-mode-setup ()
  (advice-add 'last-sexp-setup-props
              :override #'evil-elisp-mode-last-sexp-setup-props))

(provide 'evil-elisp-mode)
;;; evil-elisp-mode.el ends here
