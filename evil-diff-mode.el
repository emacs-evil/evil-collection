;;; evil-diff-mode.el --- Add Evil bindings to diff-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, diff, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Evil-Diff re-uses the read-only particularity of `diff-mode':
;; When the buffer is read-only, enter motion state
;; and manipulate the diffs with simple bindings.
;; When the buffer is writage, use normal/insert states with some Evil-specific
;; keys to ease navigation.
;;
;; See also `evil-diff-toggle-setup'.

;;; Code:

(require 'evil)
(require 'diff-mode)

(defun evil-diff-read-only-state-switch ()
  "Make read-only in motion state, writable in normal state."
  (if buffer-read-only
      (progn
        (evil-motion-state)
        (message "Evil Diff: enter motion state"))
    (evil-normal-state)
    (message "Evil Diff: enter normal state")))

(defun evil-diff-toggle-setup ()
  "Toggle visiting diff buffers in motion state."
  (interactive)
  (when (eq major-mode 'diff-mode)
    (if (memq 'evil-diff-read-only-state-switch read-only-mode-hook)
        (remove-hook 'read-only-mode-hook 'evil-diff-read-only-state-switch t)
      (add-hook 'read-only-mode-hook 'evil-diff-read-only-state-switch nil t)
      (read-only-mode))))

;;; TODO: Report toggle function upstream?
(defun evil-diff-toggle-context-unified (start end)
  "Toggle between context and unified views.

START and END are either taken from the region (if a prefix arg is given) or
else cover the whole buffer."
  (interactive (if (or current-prefix-arg (use-region-p))
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  ;; There seems to be no way to know whether we are in context or unified views.
  ;; Workaround: assume that point-max will change.  This is brittle.
  (let ((old-point-max (point-max)))
    (diff-unified->context start end)
    (when (= old-point-max (point-max))
      (diff-context->unified start end))))

;;; TODO: Report toggle function upstream?
(defun evil-diff-toggle-restrict-view (&optional arg)
  "Toggle the restriction of the view to the current hunk.
When restricting and if the prefix ARG is given, restrict the view to the
current file instead."
  (interactive "P")
  (if (buffer-narrowed-p)
      (widen)
    (diff-restrict-view arg)))

(defun evil-diff-mode-setup ()
  "Set up `evil' bindings for `diff-mode'."
  (evil-define-key 'normal diff-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "[") 'diff-file-prev
    (kbd "]") 'diff-file-next
    (kbd "C-j") 'diff-hunk-next
    (kbd "C-k") 'diff-hunk-prev

    "\\" 'read-only-mode) ; magit has "\"

  (evil-define-key 'motion diff-mode-map
    ;; motion
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "[") 'diff-file-prev
    (kbd "]") 'diff-file-next
    (kbd "C-j") 'diff-hunk-next
    (kbd "C-k") 'diff-hunk-prev

    (kbd "<return>") 'diff-goto-source
    "A" 'diff-add-change-log-entries-other-window

    "a" 'diff-apply-hunk
    "*" 'diff-refine-hunk
    "D" 'diff-file-kill
    "d" 'diff-hunk-kill

    "ge" 'diff-ediff-patch
    "i" 'next-error-follow-minor-mode
    "o" 'evil-diff-toggle-restrict-view
    "~" 'diff-reverse-direction
    "s" 'diff-split-hunk
    "c" 'diff-test-hunk
    "x" 'evil-diff-toggle-context-unified
    "#" 'diff-ignore-whitespace-hunk

    "\\" 'read-only-mode)) ; magit has "\"



(add-hook 'diff-mode-hook 'evil-diff-toggle-setup)

(defun evil-diff-unload-function ()
  "For `unload-feature'."
  (remove-hook 'diff-mode-hook 'evil-diff-toggle-setup))

(provide 'evil-diff-mode)
;;; evil-diff-mode.el ends here
