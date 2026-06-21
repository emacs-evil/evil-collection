;;; evil-collection-sly.el --- Evil bindings for `sly' -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
    "t" 'sly-db-toggle-details
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
  (evil-collection-bind 'next-section     'sly-db-mode-map 'sly-db-details-down)
  (evil-collection-bind 'prev-section     'sly-db-mode-map 'sly-db-details-up)
  (evil-collection-bind 'quit             'sly-db-mode-map 'sly-db-quit)
  (evil-collection-bind 'describe-mode    'sly-db-mode-map 'describe-mode)
  (evil-collection-bind 'refresh          'sly-db-mode-map 'sly-db-restart-frame)
  (evil-collection-bind 'debug-continue   'sly-db-mode-map 'sly-db-continue)
  (evil-collection-bind 'debug-step-over  'sly-db-mode-map 'sly-db-next)
  (evil-collection-bind 'debug-step-into  'sly-db-mode-map 'sly-db-step)
  (evil-collection-bind 'debug-step-out   'sly-db-mode-map 'sly-db-out)
  (evil-collection-bind 'debug-breakpoint 'sly-db-mode-map 'sly-db-break-on-return)
  (evil-collection-bind 'debug-eval       'sly-db-mode-map 'sly-db-eval-in-frame)
  (evil-collection-bind 'debug-restart    'sly-db-mode-map 'sly-db-return-from-frame)
  (evil-collection-bind 'debug-frame-up   'sly-db-mode-map 'sly-db-up)
  (evil-collection-bind 'debug-frame-down 'sly-db-mode-map 'sly-db-down)

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
  (evil-collection-bind 'next-button     'sly-inspector-mode-map 'forward-button)
  (evil-collection-bind 'previous-button 'sly-inspector-mode-map 'backward-button)
  (evil-collection-bind 'next-item       'sly-inspector-mode-map 'sly-inspector-next)
  (evil-collection-bind 'prev-item       'sly-inspector-mode-map 'sly-inspector-pop)
  (evil-collection-bind 'next-section    'sly-inspector-mode-map 'sly-inspector-next)
  (evil-collection-bind 'prev-section    'sly-inspector-mode-map 'sly-inspector-pop)
  (evil-collection-bind 'quit            'sly-inspector-mode-map 'sly-inspector-quit)
  (evil-collection-bind 'find-definition 'sly-inspector-mode-map 'sly-edit-definition)
  (evil-collection-bind 'lookup-doc      'sly-inspector-mode-map 'sly-inspector-describe-inspectee)
  (evil-collection-bind 'refresh         'sly-inspector-mode-map 'sly-inspector-reinspect)
  (evil-collection-bind 'refresh-all     'sly-inspector-mode-map 'sly-inspector-fetch-all)

  (evil-collection-bind 'find-usages     'sly-mode-map 'sly-who-references)
  (evil-collection-bind 'find-definition 'sly-mode-map 'sly-edit-definition)
  (evil-collection-bind 'pop-definition  'sly-mode-map 'sly-pop-find-definition-stack)
  (evil-collection-bind 'lookup-doc      'sly-mode-map 'sly-describe-symbol)
  (evil-collection-bind 'goto-repl       'sly-mode-map 'sly-mrepl)

  (evil-collection-bind 'quit 'sly-popup-buffer-mode-map 'quit-window)

  (evil-collection-bind 'find-definition 'sly-popup-buffer-mode-map 'sly-edit-definition)
  (evil-collection-bind 'pop-definition  'sly-popup-buffer-mode-map 'sly-pop-find-definition-stack)

  (evil-collection-inhibit-insert-state 'sly-thread-control-mode-map)
  (evil-collection-define-key 'normal 'sly-thread-control-mode-map
    "a" 'sly-thread-attach
    "d" 'sly-thread-debug
    "x" 'sly-thread-kill)
  (evil-collection-bind 'refresh 'sly-thread-control-mode-map 'sly-update-threads-buffer)

  (evil-collection-bind 'quit            'sly-xref-mode-map 'quit-window)
  (evil-collection-bind 'next-button     'sly-xref-mode-map 'forward-button)
  (evil-collection-bind 'previous-button 'sly-xref-mode-map 'backward-button)
  (evil-collection-define-key 'normal 'sly-xref-mode-map
    (kbd "SPC") 'sly-xref-show
    (kbd "C-i") 'forward-button
    ;; "r" 'sly-xref-retract ; TODO: Equivalent for Sly?
    )
  (evil-collection-bind 'action       'sly-xref-mode-map 'sly-xref-goto)
  (evil-collection-bind 'action-other 'sly-xref-mode-map 'sly-show-xref)
  (evil-collection-bind 'action-stay  'sly-xref-mode-map 'sly-xref-show)
  (evil-collection-bind 'next-item    'sly-xref-mode-map 'sly-xref-next-line)
  (evil-collection-bind 'prev-item    'sly-xref-mode-map 'sly-xref-prev-line)
  (evil-collection-bind 'next-section 'sly-xref-mode-map 'sly-xref-next-line)
  (evil-collection-bind 'prev-section 'sly-xref-mode-map 'sly-xref-prev-line)
  (evil-collection-bind 'refresh     'sly-xref-mode-map 'sly-recompile-xref)
  (evil-collection-bind 'refresh-all 'sly-xref-mode-map 'sly-recompile-all-xrefs)

  (evil-collection-bind 'repl-submit        'sly-mrepl-mode-map 'sly-mrepl-return)
  (evil-collection-bind 'repl-newline       'sly-mrepl-mode-map 'newline)
  (evil-collection-bind 'repl-force-newline 'sly-mrepl-mode-map 'newline)

  (evil-collection-bind 'next-item    'sly-mrepl-mode-map 'sly-mrepl-next-prompt)
  (evil-collection-bind 'prev-item    'sly-mrepl-mode-map 'sly-mrepl-previous-prompt)
  (evil-collection-bind 'next-section 'sly-mrepl-mode-map 'sly-mrepl-next-prompt)
  (evil-collection-bind 'prev-section 'sly-mrepl-mode-map 'sly-mrepl-previous-prompt)
  (evil-collection-define-key 'normal 'sly-mrepl-mode-map
    (kbd "C-p") 'sly-mrepl-previous-input-or-button
    (kbd "C-n") 'sly-mrepl-next-input-or-button)

  (evil-collection-bind 'quit        'sly-trace-dialog-mode-map 'quit-window)
  (evil-collection-bind 'refresh     'sly-trace-dialog-mode-map 'sly-trace-dialog-fetch-status)
  (evil-collection-bind 'refresh-all 'sly-trace-dialog-mode-map 'sly-trace-dialog-fetch-traces)

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
    "g?" 'sly-stickers-replay-toggle-help
    "gd" 'sly-stickers-replay-jump
    "j" 'sly-stickers-replay-next
    "k" 'sly-stickers-replay-prev
    "n" 'sly-stickers-replay-next
    "p" 'sly-stickers-replay-prev
    "v" 'sly-stickers-replay-pop-to-current-sticker
    "x" 'sly-stickers-replay-toggle-ignore-sticker
    "zi" 'sly-stickers-replay-toggle-ignore-sticker
    "zz" 'sly-stickers-replay-toggle-ignore-zombies)
  (evil-collection-bind 'quit         'sly-stickers--replay-mode-map 'quit-window)
  (evil-collection-bind 'action-other 'sly-stickers--replay-mode-map 'sly-stickers-replay-pop-to-current-sticker)

  (evil-collection-bind 'next-button     'sly-connection-list-mode-map 'forward-button)
  (evil-collection-bind 'previous-button 'sly-connection-list-mode-map 'backward-button)
  (evil-collection-define-key 'normal 'sly-connection-list-mode-map
    (kbd "C-i") 'forward-button
    "R" 'sly-restart-connection-at-point
    "d" 'sly-connection-list-make-default
    "x" 'sly-quit-connection-at-point
    "o" 'tabulated-list-sort)
  (evil-collection-bind 'quit    'sly-connection-list-mode-map 'quit-window)
  (evil-collection-bind 'refresh 'sly-connection-list-mode-map 'sly-update-connection-list)
  (evil-collection-bind 'action  'sly-connection-list-mode-map 'sly-connection-list-default-action)

  (add-hook 'sly-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-sly)
;;; evil-collection-sly.el ends here
