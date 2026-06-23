;;; evil-collection-indium.el --- Bindings for `indium' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: emacs, indium, javascript, tools

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
;; Bindings for `indium'.

;;; Code:
(require 'evil-collection)
(require 'indium nil t)

(defconst evil-collection-indium-maps '(indium-debugger-mode-map
                                        indium-inspector-mode-map
                                        indium-debugger-locals-mode-map
                                        indium-debugger-frames-mode-map
                                        indium-interaction-mode-map
                                        indium-repl-mode-map))

;;;###autoload
(defun evil-collection-indium-setup ()
  "Set up `evil' bindings for `indium'."
  (when evil-collection-setup-debugger-keys
    (evil-collection-define-key 'normal 'indium-debugger-mode-map
      "S" 'indium-debugger-stack-frames
      "H" 'indium-debugger-here)
    (add-hook 'indium-debugger-mode-hook #'evil-normalize-keymaps))

  (evil-collection-bind 'indium-debugger-mode-map
                        'quit 'indium-debugger-resume
                        'debug-continue 'indium-debugger-resume
                        'debug-step-over 'indium-debugger-step-over
                        'debug-step-into 'indium-debugger-step-into
                        'debug-step-out 'indium-debugger-step-out
                        'debug-eval 'indium-debugger-evaluate
                        'debug-locals 'indium-debugger-locals
                        'debug-frame-up 'indium-debugger-previous-frame
                        'debug-frame-down 'indium-debugger-next-frame)

  (evil-collection-define-key 'normal 'indium-inspector-mode-map
    [mouse-1] 'indium-follow-link
    "L" 'indium-inspector-pop)
  (evil-collection-bind 'indium-inspector-mode-map
                        'cycle-next 'indium-inspector-next-reference
                        'cycle-previous 'indium-inspector-previous-reference
                        'action 'indium-follow-link
                        'next-item 'indium-inspector-next-reference
                        'prev-item 'indium-inspector-previous-reference
                        'next-section 'indium-inspector-next-reference
                        'prev-section 'indium-inspector-previous-reference
                        'quit 'quit-window
                        'refresh 'indium-inspector-refresh)

  (evil-collection-define-key 'normal 'indium-debugger-locals-mode-map
    "L" nil
    "gr" nil)
  (evil-collection-bind 'indium-debugger-locals-mode-map 'quit 'quit-window)

  (evil-collection-define-key 'normal 'indium-debugger-frames-mode-map
    [return] 'indium-follow-link)
  (evil-collection-bind 'indium-debugger-frames-mode-map
                        'cycle-next 'indium-debugger-frames-next-frame
                        'cycle-previous 'indium-debugger-frames-previous-frame
                        'action 'indium-follow-link
                        'next-item 'indium-debugger-frames-next-frame
                        'prev-item 'indium-debugger-frames-previous-frame
                        'next-section 'indium-debugger-frames-next-frame
                        'prev-section 'indium-debugger-frames-previous-frame
                        'quit 'quit-window)

  (evil-collection-bind 'indium-interaction-mode-map
                        'refresh 'indium-update-script-source
                        'goto-repl 'indium-switch-to-repl-buffer)

  (when evil-collection-setup-debugger-keys
    (evil-collection-define-key 'normal 'indium-interaction-mode-map
      [left-fringe mouse-1] 'indium-mouse-toggle-breakpoint
      [left-margin mouse-1] 'indium-mouse-toggle-breakpoint
      [S-f5] 'indium-debugger-resume))

  (evil-collection-bind 'indium-interaction-mode-map
                        'debug-continue 'indium-debugger-resume
                        'debug-step-over 'indium-debugger-step-over
                        'debug-step-into 'indium-debugger-step-into
                        'debug-step-out 'indium-debugger-step-out
                        'debug-breakpoint 'indium-toggle-breakpoint)

  (evil-collection-bind 'indium-repl-mode-map
                        'next-item 'indium-repl-next-input
                        'prev-item 'indium-repl-previous-input
                        'next-section 'indium-repl-next-input
                        'prev-section 'indium-repl-previous-input))

(provide 'evil-collection-indium)
;;; evil-collection-indium.el ends here
