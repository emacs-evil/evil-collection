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

(defun evil-collection-helm-setup ()
  "Set up `evil' bindings for `helm'."
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
    "D" 'helm-ff-run-delete-file) ; Ivy has "D".

  (evil-define-key 'normal helm-map
    (kbd "<tab>") 'helm-select-action ; TODO: Ivy has "ga".
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
