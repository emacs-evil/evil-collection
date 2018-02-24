;;; evil-collection-helm.el --- Evil bindings for Helm  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, helm, tools

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
;; Evil bindings for Helm.

;;; Code:
(require 'evil)
(require 'helm-files nil t) ; TODO: Check if this is the ideal requirement and if we are not loading too much.

;; To navigate Helm entries with <hjkl> in insert state, we need a modifier.
;; Using the C- modifier would conflict with the help prefix "C-h".  So we use
;; M- prefixed bindings instead.

;; Helm-find-files: We cannot use "h" and "l" in normal state to navigate up and
;; down the file system hierarchy since we need them to use it to edit the
;; minibuffer content.

(defvar helm-map)
(defvar helm-find-files-map)
(defvar helm-read-file-map)

;; From https://github.com/emacs-helm/helm/issues/362.
;; Also see https://emacs.stackexchange.com/questions/17058/change-cursor-type-in-helm-header-line#17097.
;; TODO: With Evil, the cursor type is not right in the header line and the evil
;; cursor remains in the minibuffer.  Visual selections also reveal overlayed
;; text.
(defun evil-collection-helm-hide-minibuffer-maybe ()
  "Hide text in minibuffer when `helm-echo-input-in-header-line' is non-nil."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun evil-collection-helm--set-header-line (&optional update)
  "Like `helm--set-header-line' with some Evil-specific tweaks.
- The cursor changes when in Evil insert state.
- Visual selection is highlighted."
  (with-selected-window (minibuffer-window)
    (let* ((beg (save-excursion (vertical-motion 0 (helm-window)) (point)))
           (end (save-excursion (end-of-visual-line) (point)))
           ;; The visual line where the cursor is.
           (cont (buffer-substring beg end))
           (pref (propertize
                  " "
                  'display (if (string-match-p (regexp-opt `(,helm--prompt
                                                             ,helm--action-prompt))
                                               cont)
                               `(space :width ,helm-header-line-space-before-prompt)
                             (propertize
                              "->"
                              'face 'helm-header-line-left-margin))))
           (pos (- (point) beg)))
      ;; Increment pos each time we find a "%" up to current-pos (#1648).
      (cl-loop for c across (buffer-substring-no-properties beg (point))
               when (eql c ?%) do (cl-incf pos))
      ;; Increment pos when cursor is on a "%" to make it visible in header-line
      ;; i.e "%%|" and not "%|%" (#1649).
      (when (eql (char-after) ?%) (setq pos (1+ pos)))
      (setq cont (replace-regexp-in-string "%" "%%" cont))
      (let ((state evil-state)
            (region-active (region-active-p))
            (m (mark t)))
        (with-helm-buffer
          (setq header-line-format (concat pref cont " "))
          (when region-active
            (setq m (- m beg))
            ;; Increment pos to handle the space before prompt (i.e `pref').
            (put-text-property (1+  (min m pos))  (+ 2 (max m pos))
                               'face
                               (list :background (face-background 'region))
                               header-line-format))
          (put-text-property
           ;; Increment pos to handle the space before prompt (i.e `pref').
           (+ 1 pos) (+ 2 pos)
           'face
           (if (eq state 'insert)
               'underline
             ;; Don't just use 'cursor, this can hide the current character.
             (list :inverse-video t
                   :foreground (face-background 'cursor)
                   :background (face-background 'default)))
           header-line-format)
          (when update (force-mode-line-update)))))))

(defun evil-collection-helm-setup ()
  "Set up `evil' bindings for `helm'."
  (add-hook 'helm-minibuffer-set-up-hook 'evil-collection-helm-hide-minibuffer-maybe)
  (advice-add 'helm--set-header-line :override 'evil-collection-helm--set-header-line)

  (evil-define-key '(insert normal) helm-map
    (kbd "M-[") 'helm-previous-source
    (kbd "M-]") 'helm-next-source
    (kbd "M-l") 'helm-execute-persistent-action
    (kbd "M-j") 'helm-next-line
    (kbd "M-k") 'helm-previous-line
    (kbd "C-f") 'helm-next-page
    (kbd "C-b") 'helm-previous-page)

  (dolist (map (list helm-find-files-map helm-read-file-map))
    (evil-define-key* 'normal map
                      "go" 'helm-ff-run-switch-other-window
                      "/" 'helm-ff-run-find-sh-command)
    (evil-define-key* '(insert normal) map
                      (kbd "S-<return>") 'helm-ff-run-switch-other-window
                      (kbd "M-h") 'helm-find-files-up-one-level))

  ;; TODO: Change the Helm header to display "M-l" instead of "C-l".  We don't
  ;; want to modify the Emacs Helm map.

  (evil-define-key '(insert normal) helm-generic-files-map (kbd "S-<return>") 'helm-ff-run-switch-other-window)
  (evil-define-key '(insert normal) helm-buffer-map (kbd "S-<return>") 'helm-buffer-switch-other-window)
  (evil-define-key '(insert normal) helm-buffer-map (kbd "M-<return>") 'display-buffer)
  (evil-define-key '(insert normal) helm-moccur-map (kbd "S-<return>") 'helm-moccur-run-goto-line-ow)
  (evil-define-key '(insert normal) helm-grep-map (kbd "S-<return>") 'helm-grep-run-other-window-action)
  (evil-define-key 'normal helm-generic-files-map "go" 'helm-ff-run-switch-other-window)
  (evil-define-key 'normal helm-buffer-map "go" 'helm-buffer-switch-other-window)
  (evil-define-key 'normal helm-buffer-map "gO" 'display-buffer)
  (evil-define-key 'normal helm-moccur-map "go" 'helm-moccur-run-goto-line-ow)
  (evil-define-key 'normal helm-grep-map "go" 'helm-grep-run-other-window-action)

  (evil-define-key 'normal helm-buffer-map
    "=" 'helm-buffer-run-ediff
    "%" 'helm-buffer-run-query-replace-regexp
    "D" 'helm-buffer-run-kill-persistent) ; Ivy has "D".

  (evil-define-key 'normal helm-find-files-map
    "=" 'helm-ff-run-ediff-file
    "%" 'helm-ff-run-query-replace-regexp
    "D" 'helm-ff-run-delete-file)       ; Ivy has "D".

  (evil-define-key 'normal helm-map
    (kbd "<tab>") 'helm-select-action   ; TODO: Ivy has "ga".
    (kbd "[") 'helm-previous-source
    (kbd "]") 'helm-next-source
    "gk" 'helm-previous-source
    "gj" 'helm-next-source
    (kbd "(") 'helm-prev-visible-mark
    (kbd ")") 'helm-next-visible-mark
    "j" 'helm-next-line
    "k" 'helm-previous-line
    "gg" 'helm-beginning-of-buffer
    "G" 'helm-end-of-buffer

    "/" 'helm-quit-and-find-file

    ;; refresh
    "gr" 'helm-refresh

    "yp" 'helm-yank-selection
    "yP" 'helm-copy-to-buffer
    "yy" 'helm-kill-selection-and-quit
    (kbd "SPC") 'helm-toggle-visible-mark))

(provide 'evil-collection-helm)
;;; evil-collection-helm.el ends here
