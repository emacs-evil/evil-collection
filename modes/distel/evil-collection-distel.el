;;; evil-collection-distel.el --- Bindings for `distel' -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
;;; Bindings for `distel'.

;;; Code:
(require 'distel nil t)
(require 'evil-collection)

(defconst evil-collection-distel-maps '(erlang-extended-mode-map
                                        edb-attach-mode-map
                                        edb-monitor-mode-map
                                        edb-variables-mode-map))

;;;###autoload
(defun evil-collection-distel-setup ()
  "Set up `evil' bindings for `distel'."
  (evil-collection-bind 'erlang-extended-mode-map
                        'find-definition 'erl-find-source-under-point
                        'pop-definition 'erl-find-source-unwind
                        'lookup-doc 'erl-find-doc-under-point
                        'goto-repl 'erl-ie-show-session)

  (when evil-collection-setup-debugger-keys
    (evil-collection-define-key 'normal 'erlang-extended-mode-map
      [f5] 'edb-toggle-interpret
      [f9] 'edb-toggle-breakpoint
      ;; ("\C-c\C-ds" edb-synch-breakpoints)
      ;; ("\C-c\C-dS" edb-save-dbg-state)
      ;; ("\C-c\C-dR" edb-restore-dbg-state)
      ;; ("\C-c\C-dm" edb-monitor)
      )

    (evil-collection-bind 'edb-monitor-mode-map
                          'action 'edb-attach-command
                          'quit 'erl-bury-viewer)

    (evil-collection-bind 'edb-variables-mode-map 'action 'edb-show-variable)

    (evil-collection-define-key 'normal 'edb-attach-mode-map
      "H" 'edb-attach-finish)
    (evil-collection-bind 'edb-attach-mode-map
                          'quit 'erl-quit-viewer
                          'describe-mode 'edb-attach-help))

  (evil-collection-bind 'edb-attach-mode-map
                        'debug-continue 'edb-attach-continue
                        'debug-step-over 'edb-attach-next
                        'debug-step-into 'edb-attach-step
                        'debug-breakpoint 'edb-toggle-breakpoint
                        'debug-frame-up 'edb-attach-up
                        'debug-frame-down 'edb-attach-down)

  (evil-collection-bind 'erlang-extended-mode-map 'find-usages 'erl-who-calls)

  (add-hook 'erlang-extended-mode-hook 'evil-normalize-keymaps))

(provide 'evil-collection-distel)
;;; evil-collection-distel.el ends here
