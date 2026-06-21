;;; evil-collection-distel.el --- Bindings for `distel' -*- lexical-binding: t -*-

;; Copyright (C) 2021 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
  (evil-collection-bind 'find-definition 'erlang-extended-mode-map 'erl-find-source-under-point)
  (evil-collection-bind 'pop-definition  'erlang-extended-mode-map 'erl-find-source-unwind)
  (evil-collection-bind 'lookup-doc      'erlang-extended-mode-map 'erl-find-doc-under-point)
  (evil-collection-bind 'goto-repl       'erlang-extended-mode-map 'erl-ie-show-session)

  (when evil-collection-setup-debugger-keys
    (evil-collection-define-key 'normal 'erlang-extended-mode-map
      [f5] 'edb-toggle-interpret
      [f9] 'edb-toggle-breakpoint
      ;; ("\C-c\C-ds" edb-synch-breakpoints)
      ;; ("\C-c\C-dS" edb-save-dbg-state)
      ;; ("\C-c\C-dR" edb-restore-dbg-state)
      ;; ("\C-c\C-dm" edb-monitor)
      )

    (evil-collection-define-key 'normal 'edb-monitor-mode-map
      (kbd "RET") 'edb-attach-command)
    (evil-collection-bind 'quit 'edb-monitor-mode-map 'erl-bury-viewer)

    (evil-collection-define-key 'normal 'edb-variables-mode-map
      (kbd "RET") 'edb-show-variable)

    (evil-collection-define-key 'normal 'edb-attach-mode-map
      "H" 'edb-attach-finish)
    (evil-collection-bind 'quit             'edb-attach-mode-map 'erl-quit-viewer)
    (evil-collection-bind 'describe-mode    'edb-attach-mode-map 'edb-attach-help))

  (evil-collection-bind 'debug-continue   'edb-attach-mode-map 'edb-attach-continue)
  (evil-collection-bind 'debug-step-over  'edb-attach-mode-map 'edb-attach-next)
  (evil-collection-bind 'debug-step-into  'edb-attach-mode-map 'edb-attach-step)
  (evil-collection-bind 'debug-breakpoint 'edb-attach-mode-map 'edb-toggle-breakpoint)
  (evil-collection-bind 'debug-frame-up   'edb-attach-mode-map 'edb-attach-up)
  (evil-collection-bind 'debug-frame-down 'edb-attach-mode-map 'edb-attach-down)

  (evil-collection-bind 'find-usages 'erlang-extended-mode-map 'erl-who-calls)

  (add-hook 'erlang-extended-mode-hook 'evil-normalize-keymaps))

(provide 'evil-collection-distel)
;;; evil-collection-distel.el ends here
