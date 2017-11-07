;;; evil-helm.el --- Evil bindings for Helm  -*- lexical-binding: t -*-

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
(require 'helm-files) ; TODO: Check if this is the ideal requirement and if we are not loading too much.

;; To navigate Helm entries with <hjkl> in insert state, we need a modifier.
;; Using the C- modifier would conflict with the help prefix "C-h".  So we use
;; M- prefixed bindings instead.

;; Helm-find-files: We cannot use "h" and "l" in normal state to navigate up and
;; down the file system hierarchy since we need them to use it to edit the
;; minibuffer content.

(defun evil-helm-setup ()
  ;; TODO: We should not modify helm-map in Emacs state but somehow it does not
  ;; work otherwise.
  (define-key helm-map (kbd "M-[") 'helm-previous-source)
  (define-key helm-map (kbd "M-]") 'helm-next-source)
  (define-key helm-map (kbd "M-l") 'helm-execute-persistent-action)
  (dolist (map (list helm-find-files-map helm-read-file-map))
    (define-key map (kbd "M-h") 'helm-find-files-up-one-level)
    (define-key map (kbd "M-l") 'helm-execute-persistent-action) ; TODO: Inheritance does not seem to work for that binding.
    (define-key map (kbd "C-l") nil)) ; So the header displays the above binding.

  (evil-define-key 'normal helm-map
    (kbd "[") 'helm-previous-source
    (kbd "]") 'helm-next-source
    "j" 'helm-next-line
    "k" 'helm-previous-line
    "g" 'helm-beginning-of-buffer
    "G" 'helm-end-of-buffer
    (kbd "SPC") 'helm-toggle-visible-mark)

  (evil-define-key '(normal insert) helm-map
    (kbd "M-j") 'helm-next-line
    (kbd "M-k") 'helm-previous-line
    (kbd "C-f") 'helm-next-page
    (kbd "C-b") 'helm-previous-page))

(provide 'evil-helm)
;;; evil-helm.el ends here
