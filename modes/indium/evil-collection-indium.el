;;; evil-collection-indium.el --- Bindings for `indium' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
      "L" 'indium-debugger-locals
      "S" 'indium-debugger-stack-frames
      "H" 'indium-debugger-here
      "e" 'indium-debugger-evaluate
      ">" 'indium-debugger-next-frame
      "<" 'indium-debugger-previous-frame)
    (add-hook 'indium-debugger-mode-hook #'evil-normalize-keymaps))

  (evil-collection-theme-bind 'quit            'indium-debugger-mode-map 'indium-debugger-resume)
  (evil-collection-theme-bind 'debug-continue  'indium-debugger-mode-map 'indium-debugger-resume)
  (evil-collection-theme-bind 'debug-step-over 'indium-debugger-mode-map 'indium-debugger-step-over)
  (evil-collection-theme-bind 'debug-step-into 'indium-debugger-mode-map 'indium-debugger-step-into)
  (evil-collection-theme-bind 'debug-step-out  'indium-debugger-mode-map 'indium-debugger-step-out)

  (evil-collection-define-key 'normal 'indium-inspector-mode-map
    (kbd "RET") 'indium-follow-link
    [mouse-1] 'indium-follow-link
    "L" 'indium-inspector-pop
    [tab] 'indium-inspector-next-reference
    [backtab] 'indium-inspector-previous-reference)
  (evil-collection-theme-bind 'next-item    'indium-inspector-mode-map 'indium-inspector-next-reference)
  (evil-collection-theme-bind 'prev-item    'indium-inspector-mode-map 'indium-inspector-previous-reference)
  (evil-collection-theme-bind 'next-section 'indium-inspector-mode-map 'indium-inspector-next-reference)
  (evil-collection-theme-bind 'prev-section 'indium-inspector-mode-map 'indium-inspector-previous-reference)
  (evil-collection-theme-bind 'quit    'indium-inspector-mode-map 'quit-window)
  (evil-collection-theme-bind 'refresh 'indium-inspector-mode-map 'indium-inspector-refresh)

  (evil-collection-define-key 'normal 'indium-debugger-locals-mode-map
    "L" nil
    "gr" nil)
  (evil-collection-theme-bind 'quit 'indium-debugger-locals-mode-map 'quit-window)

  (evil-collection-define-key 'normal 'indium-debugger-frames-mode-map
    [return] 'indium-follow-link
    (kbd "RET") 'indium-follow-link
    [tab] 'indium-debugger-frames-next-frame
    [backtab] 'indium-debugger-frames-previous-frame)
  (evil-collection-theme-bind 'next-item    'indium-debugger-frames-mode-map 'indium-debugger-frames-next-frame)
  (evil-collection-theme-bind 'prev-item    'indium-debugger-frames-mode-map 'indium-debugger-frames-previous-frame)
  (evil-collection-theme-bind 'next-section 'indium-debugger-frames-mode-map 'indium-debugger-frames-next-frame)
  (evil-collection-theme-bind 'prev-section 'indium-debugger-frames-mode-map 'indium-debugger-frames-previous-frame)
  (evil-collection-theme-bind 'quit 'indium-debugger-frames-mode-map 'quit-window)

  (evil-collection-theme-bind 'refresh   'indium-interaction-mode-map 'indium-update-script-source)
  (evil-collection-theme-bind 'goto-repl 'indium-interaction-mode-map 'indium-switch-to-repl-buffer)

  (when evil-collection-setup-debugger-keys
    (evil-collection-define-key 'normal 'indium-interaction-mode-map
      [left-fringe mouse-1] 'indium-mouse-toggle-breakpoint
      [left-margin mouse-1] 'indium-mouse-toggle-breakpoint
      [S-f5] 'indium-debugger-resume))

  (evil-collection-theme-bind 'debug-continue   'indium-interaction-mode-map 'indium-debugger-resume)
  (evil-collection-theme-bind 'debug-step-over  'indium-interaction-mode-map 'indium-debugger-step-over)
  (evil-collection-theme-bind 'debug-step-into  'indium-interaction-mode-map 'indium-debugger-step-into)
  (evil-collection-theme-bind 'debug-step-out   'indium-interaction-mode-map 'indium-debugger-step-out)
  (evil-collection-theme-bind 'debug-breakpoint 'indium-interaction-mode-map 'indium-toggle-breakpoint)

  (evil-collection-theme-bind 'next-item    'indium-repl-mode-map 'indium-repl-next-input)
  (evil-collection-theme-bind 'prev-item    'indium-repl-mode-map 'indium-repl-previous-input)
  (evil-collection-theme-bind 'next-section 'indium-repl-mode-map 'indium-repl-next-input)
  (evil-collection-theme-bind 'prev-section 'indium-repl-mode-map 'indium-repl-previous-input))

(provide 'evil-collection-indium)
;;; evil-collection-indium.el ends here
