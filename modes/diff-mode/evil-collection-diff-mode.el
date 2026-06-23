;;; evil-collection-diff-mode.el --- Add Evil bindings to diff-mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
;;; This should be a comment.

;;; Code:

(require 'evil-collection)
(require 'diff-mode)

(defconst evil-collection-diff-mode-maps '(diff-mode-map))

;;; TODO: Report toggle function upstream?
(defun evil-collection-diff-toggle-context-unified (start end)
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
(defun evil-collection-diff-toggle-restrict-view (&optional arg)
  "Toggle the restriction of the view to the current hunk.
When restricting and if the prefix ARG is given, restrict the view to the
current file instead."
  (interactive "P")
  (if (buffer-narrowed-p)
      (widen)
    (diff-restrict-view arg)))

(defvar evil-collection-diff-mode-map (make-sparse-keymap))

(define-minor-mode evil-collection-diff-mode
   "A minor mode to attach to `diff-mode' results"
  :group 'evil-collection-diff-mode
  :keymap evil-collection-diff-mode-map
  :lighter nil)

(defun evil-collection-diff-mode-turn-on ()
   "Turn on `evil-collection-diff-mode' and normalize keymaps."
  (evil-collection-diff-mode)
  (evil-normalize-keymaps))

(defun evil-collection-diff-mode-setup-on-minor-mode ()
   "Set up bindings on `evil-collection-diff-mode'."
  (evil-collection-define-key 'normal 'evil-collection-diff-mode-map
    "ge" 'diff-ediff-patch
    "\\" 'read-only-mode)
  (evil-collection-bind 'evil-collection-diff-mode-map
                        'action 'diff-goto-source
                        'next-item 'diff-hunk-next
                        'prev-item 'diff-hunk-prev
                        'next-section 'diff-file-next
                        'prev-section 'diff-file-prev
                        'next-section-2 'diff-hunk-next
                        'prev-section-2 'diff-hunk-prev
                        'quit 'quit-window)

  (if evil-collection-want-g-bindings
      (evil-collection-define-key 'normal 'evil-collection-diff-mode-map
        "gA" 'diff-add-change-log-entries-other-window
        "ga" 'diff-apply-hunk
        "g*" 'diff-refine-hunk
        "gX" 'diff-file-kill
        "gx" 'diff-hunk-kill
        "gi" 'next-error-follow-minor-mode
        "go" 'evil-collection-diff-toggle-restrict-view
        "g~" 'diff-reverse-direction
        "gs" 'diff-split-hunk
        "gc" 'diff-test-hunk
        "g%" 'evil-collection-diff-toggle-context-unified
        "g#" 'diff-ignore-whitespace-hunk)
    (evil-collection-define-key 'normal 'evil-collection-diff-mode-map
      "A" 'diff-add-change-log-entries-other-window
      "a" 'diff-apply-hunk
      "*" 'diff-refine-hunk
      "D" 'diff-file-kill
      "d" 'diff-hunk-kill
      "i" 'next-error-follow-minor-mode
      "o" 'evil-collection-diff-toggle-restrict-view
      "~" 'diff-reverse-direction
      "s" 'diff-split-hunk
      "c" 'diff-test-hunk
      "x" 'evil-collection-diff-toggle-context-unified
      "#" 'diff-ignore-whitespace-hunk))

  ;; Enable a separate minor mode so that we can bind keys to it.
  (add-hook 'diff-mode-hook 'evil-collection-diff-mode))

;;;###autoload
(defun evil-collection-diff-mode-setup ()
  "Set up `evil' bindings for `diff-mode'."
  ;; Don't switch to read-only/motion state by default as this can interfere
  ;; with other modes which require a writable buffer, e.g. magit.
  (evil-set-initial-state 'diff-mode 'normal)
  (evil-collection-diff-mode-setup-on-minor-mode))

(provide 'evil-collection-diff-mode)
;;; evil-collection-diff-mode.el ends here
