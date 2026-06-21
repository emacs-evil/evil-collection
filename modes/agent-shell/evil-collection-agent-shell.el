;;; evil-collection-agent-shell.el --- Bindings for agent-shell -*- lexical-binding: t -*-

;; Copyright (C) 2026 Joseph LaFreniere

;; Author: Joseph LaFreniere <git@lafreniere.xyz>
;; Maintainer: Joseph LaFreniere <git@lafreniere.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, agent-shell, tools

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
;;; Bindings for agent-shell.

;;; Code:
(require 'evil-collection)
(require 'agent-shell nil t)

(defconst evil-collection-agent-shell-maps '(agent-shell-mode-map
                                             agent-shell-viewport-edit-mode-map
                                             agent-shell-viewport-view-mode-map
                                             agent-shell-diff-mode-map))

(defvar agent-shell-mode-map)
(defvar agent-shell-viewport-edit-mode-map)
(defvar agent-shell-viewport-view-mode-map)
(defvar agent-shell-diff-mode-map)

(defcustom evil-collection-agent-shell-want-permission-bindings t
  "When non-nil, bind permission button keys in normal state.

`y', `n', `!', and `v' will activate the latest pending permission
button when one is visible, and fall through to their default Evil
commands otherwise."
  :type 'boolean
  :group 'evil-collection)

;;; Permission button helpers.

(defun evil-collection-agent-shell--latest-permission-keymap ()
  "Return the keymap of the latest pending permission button, or nil.
Walks the buffer backward from `point-max' looking for text with the
`agent-shell-permission-button' property -- the navigatable single-char
marker that `agent-shell--make-permission-button' adds to each button."
  (save-excursion
    (goto-char (point-max))
    (when-let* ((match (text-property-search-backward
                        'agent-shell-permission-button t t)))
      (get-text-property (prop-match-beginning match) 'keymap))))

(defun evil-collection-agent-shell--pending-permission-p ()
  "Return non-nil when the current buffer has a pending permission button."
  (and (evil-collection-agent-shell--latest-permission-keymap) t))

(defun evil-collection-agent-shell--press (key)
  "Dispatch KEY through the latest pending permission's button keymap.
KEY is a string passed to `kbd'."
  (let* ((km (or (evil-collection-agent-shell--latest-permission-keymap)
                 (user-error "No pending permission")))
         (binding (lookup-key km (kbd key))))
    (cond
     ((commandp binding) (call-interactively binding))
     ((null binding) (user-error "No `%s' action on this permission" key))
     (t (user-error "Unexpected binding for `%s': %S" key binding)))))

(defun evil-collection-agent-shell-permission-allow-once ()
  "Allow the latest pending tool-call permission once."
  (interactive)
  (evil-collection-agent-shell--press "y"))

(defun evil-collection-agent-shell-permission-reject-once ()
  "Reject the latest pending tool-call permission and interrupt the agent."
  (interactive)
  (evil-collection-agent-shell--press "C-c C-c"))

(defun evil-collection-agent-shell-permission-allow-always ()
  "Always-allow the tool kind for the latest pending permission."
  (interactive)
  (evil-collection-agent-shell--press "!"))

(defun evil-collection-agent-shell-permission-view-diff ()
  "View the diff for the latest pending permission, if any."
  (interactive)
  (evil-collection-agent-shell--press "v"))

(defvar evil-collection-agent-shell-permission-allow-once
  `(menu-item "" nil :filter
              ,(lambda (&optional _)
                 (when (evil-collection-agent-shell--pending-permission-p)
                   'evil-collection-agent-shell-permission-allow-once))))

(defvar evil-collection-agent-shell-permission-reject-once
  `(menu-item "" nil :filter
              ,(lambda (&optional _)
                 (when (evil-collection-agent-shell--pending-permission-p)
                   'evil-collection-agent-shell-permission-reject-once))))

(defvar evil-collection-agent-shell-permission-allow-always
  `(menu-item "" nil :filter
              ,(lambda (&optional _)
                 (when (evil-collection-agent-shell--pending-permission-p)
                   'evil-collection-agent-shell-permission-allow-always))))

(defvar evil-collection-agent-shell-permission-view-diff
  `(menu-item "" nil :filter
              ,(lambda (&optional _)
                 (when (evil-collection-agent-shell--pending-permission-p)
                   'evil-collection-agent-shell-permission-view-diff))))

;;;###autoload
(defun evil-collection-agent-shell-setup ()
  "Set up `evil' bindings for `agent-shell'."
  ;; `agent-shell-mode-map' binds \"n\" and \"p\" at the map level, which causes
  ;; them to intercept keystrokes even in insert state.  Remove those bindings
  ;; entirely.
  (if (fboundp 'keymap-unset)
      (progn
        (keymap-unset agent-shell-mode-map "n" t)
        (keymap-unset agent-shell-mode-map "p" t))
    (define-key agent-shell-mode-map "n" nil)
    (define-key agent-shell-mode-map "p" nil))

  (evil-collection-define-key 'insert 'agent-shell-mode-map
    "n" 'self-insert-command
    "p" 'self-insert-command)

  (when evil-collection-agent-shell-want-permission-bindings
    (evil-collection-define-key 'normal 'agent-shell-mode-map
      "v" evil-collection-agent-shell-permission-view-diff
      "y" evil-collection-agent-shell-permission-allow-once
      "n" evil-collection-agent-shell-permission-reject-once
      "!" evil-collection-agent-shell-permission-allow-always))

  (evil-collection-define-key 'normal 'agent-shell-mode-map
    (kbd "C-<tab>") 'agent-shell-cycle-session-mode)
  (evil-collection-bind 'agent-shell-mode-map
                        'cycle-next 'agent-shell-next-item
                        'cycle-previous 'agent-shell-previous-item)

  (when evil-collection-want-g-bindings
    (evil-collection-define-key 'normal 'agent-shell-mode-map
      "gs" 'agent-shell-cycle-session-mode
      "gx" 'agent-shell-interrupt
      "gm" 'agent-shell-set-session-mode
      "gv" 'agent-shell-set-session-model
      "go" 'agent-shell-other-buffer
      "gp" 'agent-shell-yank-dwim
      "gr" 'agent-shell-reload
      "gR" 'agent-shell-restart
      "gF" 'agent-shell-fork
      "gy" 'agent-shell-copy-session-id
      "gc" 'agent-shell-prompt-compose
      "gq" 'agent-shell-queue-request
      "gt" 'agent-shell-open-transcript))

  (evil-collection-define-key 'normal 'agent-shell-viewport-edit-mode-map
    [remap evil-save-and-close] 'agent-shell-viewport-compose-send
    [remap evil-save-modified-and-close] 'agent-shell-viewport-compose-send
    [remap evil-ret] 'agent-shell-viewport-compose-send
    [remap evil-write] 'agent-shell-viewport-compose-send
    [remap evil-quit] 'agent-shell-viewport-compose-cancel
    "M-k" 'agent-shell-viewport-previous-history
    "M-j" 'agent-shell-viewport-next-history
    (kbd "C-o") 'agent-shell-viewport-compose-peek-last)

  (when evil-collection-want-g-bindings
    (evil-collection-define-key 'normal 'agent-shell-viewport-edit-mode-map
      "gs" 'agent-shell-cycle-session-mode
      "gm" 'agent-shell-viewport-set-session-mode
      "gv" 'agent-shell-viewport-set-session-model
      "go" 'agent-shell-other-buffer
      "gp" 'agent-shell-yank-dwim
      "g/" 'agent-shell-viewport-search-history
      "gy" 'agent-shell-viewport-copy-session-id
      "gt" 'agent-shell-viewport-open-transcript)
    (evil-collection-bind 'agent-shell-viewport-edit-mode-map 'describe-mode 'agent-shell-viewport-compose-help-menu))

  (evil-collection-define-key 'normal 'agent-shell-viewport-view-mode-map
    [remap evil-save-and-close] 'agent-shell-viewport-interrupt
    [remap evil-save-modified-and-close] 'agent-shell-viewport-interrupt
    [remap evil-ret] 'agent-shell-viewport-interrupt
    [remap evil-write] 'agent-shell-viewport-interrupt
    [remap evil-quit] 'bury-buffer)
  (evil-collection-bind 'agent-shell-viewport-view-mode-map
                        'cycle-next 'agent-shell-viewport-next-item
                        'cycle-previous 'agent-shell-viewport-previous-item
                        'next-item 'agent-shell-viewport-next-item
                        'prev-item 'agent-shell-viewport-previous-item
                        'next-section 'agent-shell-viewport-next-page
                        'prev-section 'agent-shell-viewport-previous-page
                        'quit 'bury-buffer)

  (when evil-collection-want-g-bindings
    (evil-collection-define-key 'normal 'agent-shell-viewport-view-mode-map
      "g1" 'agent-shell-viewport-reply-1
      "g2" 'agent-shell-viewport-reply-2
      "g3" 'agent-shell-viewport-reply-3
      "g4" 'agent-shell-viewport-reply-4
      "g5" 'agent-shell-viewport-reply-5
      "g6" 'agent-shell-viewport-reply-6
      "g7" 'agent-shell-viewport-reply-7
      "g8" 'agent-shell-viewport-reply-8
      "g9" 'agent-shell-viewport-reply-9
      "gs" 'agent-shell-viewport-cycle-session-mode
      "gv" 'agent-shell-viewport-set-session-model
      "go" 'agent-shell-other-buffer
      "gm" 'agent-shell-viewport-set-session-mode
      ;; Maybe move elsewhere.
      "gr" 'agent-shell-viewport-reply
      "gy" 'agent-shell-viewport-reply-yes
      "gM" 'agent-shell-viewport-reply-more
      "ga" 'agent-shell-viewport-reply-again
      "gc" 'agent-shell-viewport-reply-continue
      "ge" 'agent-shell-viewport-edit
      "gF" 'agent-shell-viewport-fork
      "gY" 'agent-shell-viewport-copy-session-id
      "gt" 'agent-shell-viewport-open-transcript
      "gz" 'agent-shell-viewport-refresh)
    (evil-collection-bind 'agent-shell-viewport-view-mode-map 'describe-mode 'agent-shell-viewport-help-menu))

  (evil-collection-bind 'agent-shell-diff-mode-map
                        'quit #'kill-current-buffer
                        'find-file 'agent-shell-diff-open-file)

  (if evil-collection-want-g-bindings
      (evil-collection-define-key 'normal 'agent-shell-diff-mode-map
        "gy" 'agent-shell-diff-accept-all
        "gn" 'agent-shell-diff-reject-all)
    (evil-collection-define-key 'normal 'agent-shell-diff-mode-map
      "y" 'agent-shell-diff-accept-all
      "n" 'agent-shell-diff-reject-all))

  (add-hook 'agent-shell-diff-mode-hook #'evil-normalize-keymaps)
  (add-hook 'agent-shell-mode-hook #'evil-normalize-keymaps)
  (add-hook 'agent-shell-viewport-view-mode-hook #'evil-normalize-keymaps)
  (add-hook 'agent-shell-viewport-edit-mode-hook #'evil-normalize-keymaps)

  (advice-add 'agent-shell-jump-to-latest-permission-button-row
              :after (lambda (&rest _) (evil-normalize-keymaps))))

(provide 'evil-collection-agent-shell)
;;; evil-collection-agent-shell.el ends here
