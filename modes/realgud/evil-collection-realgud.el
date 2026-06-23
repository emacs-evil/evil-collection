;;; evil-collection-realgud.el --- Bindings for `realgud' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, emacs, tools, realgud

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
;; Bindings for `realgud'.

;;; Code:
(require 'evil-collection)
(require 'realgud nil t)

(defconst evil-collection-realgud-maps '(realgud:shortkey-mode-map))

;;;###autoload
(defun evil-collection-realgud-setup ()
  "Set up `evil' bindings for `realgud'."
  ;; This one is to represent `realgud-populate-src-buffer-map-plain'.
  (evil-collection-define-key 'normal 'realgud:shortkey-mode-map
    "X" 'realgud:cmd-clear
    "-" 'realgud:cmd-disable
    "+" 'realgud:cmd-enable
    "T" 'realgud:cmd-backtrace
    "f" 'realgud:cmd-finish
    "Q" 'realgud:cmd-kill
    "r" 'realgud:cmd-restart
    "R" 'realgud:cmd-restart
    "!" 'realgud:cmd-shell

    ;; (evil-collection-define-key nil map [M-down]    'realgud-track-hist-newer)
    ;; (evil-collection-define-key nil map [M-kp-2]    'realgud-track-hist-newer)
    ;; (evil-collection-define-key nil map [M-up]      'realgud-track-hist-older)
    ;; (evil-collection-define-key nil map [M-kp-8]    'realgud-track-hist-older)
    ;; (evil-collection-define-key nil map [M-kp-up]   'realgud-track-hist-older)
    ;; (evil-collection-define-key nil map [M-kp-down] 'realgud-track-hist-newer)
    ;; (evil-collection-define-key nil map [M-print]   'realgud-track-hist-older)
    ;; (evil-collection-define-key nil map [M-S-down]  'realgud-track-hist-newest)
    ;; (evil-collection-define-key nil map [M-S-up]    'realgud-track-hist-oldest)
    )
  (evil-collection-bind 'realgud:shortkey-mode-map
                        'quit 'realgud:cmd-quit
                        'delete 'realgud:cmd-delete)

  (evil-collection-define-key 'normal 'realgud:shortkey-mode-map
    (kbd "C-x C-q") 'realgud-short-key-mode
    "1" 'realgud-goto-arrow1
    "2" 'realgud-goto-arrow2
    "3" 'realgud-goto-arrow3
    "4" 'realgud:goto-loc-hist-4
    "5" 'realgud:goto-loc-hist-5
    "6" 'realgud:goto-loc-hist-6
    "7" 'realgud:goto-loc-hist-7
    "8" 'realgud:goto-loc-hist-8
    "9" 'realgud:goto-loc-hist-9
    "E" 'realgud:cmd-eval-at-point
    "U" 'realgud:cmd-until
    "H" 'realgud:cmd-until-here
    [mouse-2] 'realgud:tooltip-eval
    [left-fringe mouse-1] 'realgud-cmds--mouse-add-remove-bp
    [left-margin mouse-1] 'realgud-cmds--mouse-add-remove-bp
    "D" 'realgud:cmd-newer-frame
    "u" 'realgud:cmd-older-frame
    "gR" 'realgud-recenter-arrow ;; FIXME: Hmnn!
    "C" 'realgud-window-cmd-undisturb-src
    "S" 'realgud-window-src-undisturb-cmd
    "!" 'realgud:cmd-shell)

  (evil-collection-bind 'realgud:shortkey-mode-map
                        'describe-mode 'realgud:cmdbuf-info-describe
                        'refresh 'realgud:cmd-restart
                        'debug-continue 'realgud:cmd-continue
                        'debug-step-over 'realgud:cmd-next
                        'debug-step-into 'realgud:cmd-step
                        'debug-breakpoint 'realgud:cmd-break
                        'debug-eval 'realgud:cmd-eval-dwim
                        'debug-restart 'realgud:cmd-restart
                        'debug-frame-up 'realgud:cmd-older-frame
                        'debug-frame-down 'realgud:cmd-newer-frame
                        'jump 'realgud:cmd-jump)

  (add-hook 'realgud-short-key-mode-hook #'evil-normalize-keymaps))

(provide 'evil-collection-realgud)
;;; evil-collection-realgud.el ends here
