;;; evil-collection-sly.el --- Evil bindings for `sly' -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, sly, tools

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
;; Evil bindings for `sly-mode'.

;;; Code:
;; WARNING: This mirrors evil-collection-slime.el.  Keep them in sync!

(require 'evil-collection)
(require 'sly nil t)

(declare-function sly-last-expression "sly")
(declare-function sly-eval-print "sly")

(defvar sly-connection-list-mode-map)
(defvar sly-db-mode-map)
(defvar sly-inspector-mode-map)
(defvar sly-mode-map)
(defvar sly-popup-buffer-mode-map)
(defvar sly-thread-control-mode-map)
(defvar sly-trace-dialog-mode-map)
(defvar sly-stickers--replay-mode-map)
(defvar sly-xref-mode-map)

(defconst evil-collection-sly-maps '(sly-connection-list-mode-map
                                     sly-db-mode-map
                                     sly-inspector-mode-map
                                     sly-mode-map
                                     sly-popup-buffer-mode-map
                                     sly-stickers--replay-mode-map
                                     sly-thread-control-mode-map
                                     sly-trace-dialog-mode-map
                                     sly-xref-mode-map))

;; Same as `evil-collection-slime-last-sexp'.
(defun evil-collection-sly-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(defun evil-collection-sly-eval-print-last-expression (string)
  "Evaluate sexp before point; print value into the current buffer.

Evil version of `sly-eval-print-last-expression' that accounts for
`evil-move-beyond-eol'."
  (interactive
   (list (progn
           (when (and (not evil-move-beyond-eol)
                      (or (evil-normal-state-p) (evil-motion-state-p)))
             (unless (or (eobp) (eolp))
               (forward-char)))
           (sly-last-expression))))
  (insert "\n")
  (sly-eval-print string))

;;;###autoload
(defun evil-collection-sly-setup ()
  "Set up `evil' bindings for `sly'."
  (unless evil-move-beyond-eol
    (advice-add 'sly-eval-last-expression :around 'evil-collection-sly-last-sexp)
    (advice-add 'sly-pprint-eval-last-expression :around 'evil-collection-sly-last-sexp)
    (advice-add 'sly-mrepl-return :around 'evil-collection-sly-last-sexp)
    (advice-add 'sly-eval-print-last-expression :override 'evil-collection-sly-eval-print-last-expression))

  (evil-collection-define-key 'normal 'sly-db-mode-map
    [follow-link] 'mouse-face
    "\C-i" 'sly-db-cycle
    "S" 'sly-db-show-frame-source
    "d" 'sly-db-pprint-eval-in-frame
    "D" 'sly-db-disassemble
    "gi" 'sly-db-inspect-in-frame
    (kbd "M-j") 'sly-db-details-down
    (kbd "M-k") 'sly-db-details-up
    "gg" 'sly-db-beginning-of-backtrace
    "G" 'sly-db-end-of-backtrace
    "I" 'sly-db-invoke-restart-by-name
    "a" 'sly-db-abort
    "A" 'sly-db-break-with-system-debugger
    "B" 'sly-db-break-with-default-debugger
    "P" 'sly-db-print-condition
    "C" 'sly-db-inspect-condition
    "g:" 'sly-interactive-eval
    "0" 'sly-db-invoke-restart-0
    "1" 'sly-db-invoke-restart-1
    "2" 'sly-db-invoke-restart-2
    "3" 'sly-db-invoke-restart-3
    "4" 'sly-db-invoke-restart-4
    "5" 'sly-db-invoke-restart-5
    "6" 'sly-db-invoke-restart-6
    "7" 'sly-db-invoke-restart-7
    "8" 'sly-db-invoke-restart-8
    "9" 'sly-db-invoke-restart-9)
  (evil-collection-bind 'sly-db-mode-map
                        'next-section 'sly-db-details-down
                        'prev-section 'sly-db-details-up
                        'quit 'sly-db-quit
                        'describe-mode 'describe-mode
                        'refresh 'sly-db-restart-frame
                        'toggle 'sly-db-toggle-details
                        'debug-continue 'sly-db-continue
                        'debug-step-over 'sly-db-next
                        'debug-step-into 'sly-db-step
                        'debug-step-out 'sly-db-out
                        'debug-breakpoint 'sly-db-break-on-return
                        'debug-eval 'sly-db-eval-in-frame
                        'debug-restart 'sly-db-return-from-frame
                        'debug-frame-up 'sly-db-up
                        'debug-frame-down 'sly-db-down)

  (evil-collection-define-key 'normal 'sly-inspector-mode-map
    [mouse-6] 'sly-inspector-pop
    [mouse-7] 'sly-inspector-next
    ;; TODO: `sly-inspector-next' and `sly-inspector-pop' should probably
    ;; just be bound to C-i and C-o.
    (kbd "C-o") 'sly-inspector-pop
    (kbd "C-i") 'sly-inspector-next
    "e" 'sly-inspector-eval
    "M-p" 'sly-inspector-history
    "gv" 'sly-inspector-toggle-verbose
    (kbd "C-i") 'forward-button
    "." 'sly-edit-definition)
  (evil-collection-bind 'sly-inspector-mode-map
                        'next-button 'forward-button
                        'previous-button 'backward-button
                        'next-item 'sly-inspector-next
                        'prev-item 'sly-inspector-pop
                        'next-section 'sly-inspector-next
                        'prev-section 'sly-inspector-pop
                        'quit 'sly-inspector-quit
                        'find-definition 'sly-edit-definition
                        'lookup-doc 'sly-inspector-describe-inspectee
                        'refresh 'sly-inspector-reinspect
                        'refresh-all 'sly-inspector-fetch-all)

  (evil-collection-bind 'sly-mode-map
                        'find-usages 'sly-who-references
                        'find-definition 'sly-edit-definition
                        'pop-definition 'sly-pop-find-definition-stack
                        'lookup-doc 'sly-describe-symbol
                        'goto-repl 'sly-mrepl)

  (evil-collection-bind 'sly-popup-buffer-mode-map
                        'quit 'quit-window
                        'find-definition 'sly-edit-definition
                        'pop-definition 'sly-pop-find-definition-stack)

  (evil-collection-inhibit-insert-state 'sly-thread-control-mode-map)
  (evil-collection-define-key 'normal 'sly-thread-control-mode-map
    "a" 'sly-thread-attach
    "d" 'sly-thread-debug)
  (evil-collection-bind 'sly-thread-control-mode-map
                        'refresh 'sly-update-threads-buffer
                        'delete-2 'sly-thread-kill)

  (evil-collection-bind 'sly-xref-mode-map
                        'quit 'quit-window
                        'next-button 'forward-button
                        'previous-button 'backward-button)
  (evil-collection-define-key 'normal 'sly-xref-mode-map
    (kbd "SPC") 'sly-xref-show
    (kbd "C-i") 'forward-button
    ;; "r" 'sly-xref-retract ; TODO: Equivalent for Sly?
    )
  (evil-collection-bind 'sly-xref-mode-map
                        'action 'sly-xref-goto
                        'action-other 'sly-show-xref
                        'action-stay 'sly-xref-show
                        'next-item 'sly-xref-next-line
                        'prev-item 'sly-xref-prev-line
                        'next-section 'sly-xref-next-line
                        'prev-section 'sly-xref-prev-line
                        'refresh 'sly-recompile-xref
                        'refresh-all 'sly-recompile-all-xrefs)

  (evil-collection-bind 'sly-mrepl-mode-map
                        'repl-submit 'sly-mrepl-return
                        'repl-newline 'newline
                        'repl-force-newline 'newline
                        'next-item 'sly-mrepl-next-prompt
                        'prev-item 'sly-mrepl-previous-prompt
                        'next-section 'sly-mrepl-next-prompt
                        'prev-section 'sly-mrepl-previous-prompt
                        'history-previous 'sly-mrepl-previous-input-or-button
                        'history-next     'sly-mrepl-next-input-or-button)

  (evil-collection-bind 'sly-trace-dialog-mode-map
                        'quit 'quit-window
                        'refresh 'sly-trace-dialog-fetch-status
                        'refresh-all 'sly-trace-dialog-fetch-traces)

  (evil-collection-define-key 'normal 'sly-stickers--replay-mode-map
    ;; The "n", "p", "x", "h" and "q" key are hard-coded in the description, so
    ;; we need to set them to the expected command.
    "gg" 'sly-stickers-replay-jump-to-beginning
    "G" 'sly-stickers-replay-jump-to-end
    "F" 'sly-stickers-forget
    "J" 'sly-stickers-replay-next-for-sticker
    "K" 'sly-stickers-replay-prev-for-sticker
    "N" 'sly-stickers-replay-next-for-sticker
    "P" 'sly-stickers-replay-prev-for-sticker
    "R" 'sly-stickers-replay-reset-ignore-list
    "zv" 'sly-stickers-replay-toggle-pop-to-stickers
    "h" 'sly-stickers-replay-toggle-help
    "j" 'sly-stickers-replay-next
    "k" 'sly-stickers-replay-prev
    "n" 'sly-stickers-replay-next
    "p" 'sly-stickers-replay-prev
    "v" 'sly-stickers-replay-pop-to-current-sticker
    "x" 'sly-stickers-replay-toggle-ignore-sticker
    "zi" 'sly-stickers-replay-toggle-ignore-sticker
    "zz" 'sly-stickers-replay-toggle-ignore-zombies)
  (evil-collection-bind 'sly-stickers--replay-mode-map
                        'quit 'quit-window
                        'action-other 'sly-stickers-replay-pop-to-current-sticker
                        'find-definition 'sly-stickers-replay-jump
                        'describe-mode 'sly-stickers-replay-toggle-help)

  (evil-collection-bind 'sly-connection-list-mode-map
                        'next-button 'forward-button
                        'previous-button 'backward-button)
  (evil-collection-define-key 'normal 'sly-connection-list-mode-map
    (kbd "C-i") 'forward-button
    "R" 'sly-restart-connection-at-point
    "d" 'sly-connection-list-make-default
    "o" 'tabulated-list-sort)
  (evil-collection-bind 'sly-connection-list-mode-map
                        'quit 'quit-window
                        'refresh 'sly-update-connection-list
                        'action 'sly-connection-list-default-action
                        'delete-2 'sly-quit-connection-at-point)

  (add-hook 'sly-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-sly)
;;; evil-collection-sly.el ends here
