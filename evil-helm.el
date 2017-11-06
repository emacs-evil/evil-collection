;;; evil-helm.el --- Evil bindings for Helm  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, compile, evil
;; HomePage: https://github.com/jojojames/evil-collection

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

;;; Code:

(require 'evil)
(require 'helm-files) ; TODO: Check if this is the ideal requirement and if we are not loading too much.

;; To navigate Helm entries with <hjkl> in insert state, we need a modifier.
;; Using the C- modifier would conflict with the help prefix "C-h".  So we use
;; M- prefixed bindings instead.

;; Helm-find-files: We cannot use "h" and "l" in normal state to navigate up and
;; down the file system hierarchy since we need them to use it to edit the
;; minibuffer content.

;; TODO: This does not seem to do anything.  Calling the `evil-define-key's manually works though.
(defun evil-helm-set-keys ()

  (evil-define-key '(insert normal) helm-map
    (kbd "C-f") 'helm-next-page
    (kbd "C-b") 'helm-previous-page
    (kbd "M-h") 'helm-next-source
    (kbd "M-j") 'helm-next-line
    (kbd "M-k") 'helm-previous-line
    (kbd "M-l") 'helm-execute-persistent-action)

  ;; (kbd "<escape>") 'helm-keyboard-quit)

  (evil-define-key 'normal helm-map
    (kbd "<tab>") 'helm-select-action
    "j" 'helm-next-line
    "k" 'helm-previous-line
    "g" 'helm-beginning-of-buffer
    "G" 'helm-end-of-buffer
    (kbd "SPC") 'helm-toggle-visible-mark
    ;; (kbd "S-SPC") 'evil-helm-toggle-visible-mark-backwards ; TODO: Not needed?
    (kbd "C-f") 'helm-next-page
    (kbd "C-b") 'helm-previous-page)

  (dolist (map (list helm-find-files-map helm-read-file-map))
    (evil-define-key 'insert map
      (kbd "M-h") 'helm-find-files-up-one-level
      (kbd "M-l") 'helm-execute-persistent-action
      (kbd "C-l") nil))) ; So the header displays the new `helm-execute-persistent-action' binding.

(provide 'evil-helm)
;;; evil-helm.el ends here
