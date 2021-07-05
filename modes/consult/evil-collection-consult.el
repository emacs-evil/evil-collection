;;; evil-collection-consult.el --- Evil bindings for consult -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, consult, tools

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
;; Evil bindings for `consult'.

;;; Code:
(require 'evil-collection)
(require 'consult nil t)

(defvar consult-line-numbers-widen)
(declare-function consult--forbid-minibuffer "consult")
(declare-function consult--fontify-all "consult")
(declare-function consult--in-range-p "consult")
(declare-function consult--line-with-cursor "consult")
(declare-function consult--location-candidate "consult")
(declare-function consult--remove-dups "consult")
(declare-function consult--mark-candidates "consult")
(declare-function consult-mark "consult")

(defun evil-collection-consult-set-bindings ()
  "Set the bindings."
  (evil-set-command-property 'consult-outline :jump t)
  (evil-set-command-property 'consult-mark :jump t)
  (evil-set-command-property 'consult-line :jump t))

(defun evil-collection-consult--evil-mark-ring ()
  "Return alist of char & marker for evil markers in current buffer."
  (sort (cl-remove-if (lambda (elem)
                        (or (evil-global-marker-p (car elem))
                            (not (markerp (cdr-safe elem)))))
                      evil-markers-alist)
        #'car-less-than-car))

(defun evil-collection-consult--mark-candidates (&optional markers)
  "Return alist of lines containing markers from `evil-mark-alist'.
Opional MARKERS should be an alist containing (char . marker) pairs
as defined in `evil-collection-consult--evil-mark-ring'."
  (consult--forbid-minibuffer)
  (unless (evil-collection-consult--evil-mark-ring)
    (user-error "No marks"))
  (consult--fontify-all)
  (let* ((candidates)
         (current-buf (current-buffer)))
    (save-excursion
      (dolist (marker (or markers (evil-collection-consult--evil-mark-ring)))
        (let ((pos (marker-position (cdr marker)))
              (buf (marker-buffer (cdr marker))))
          (when (and (eq buf current-buf)
                     (consult--in-range-p pos))
            (goto-char pos)
            (push (consult--location-candidate
                   (format "%s: %s" (char-to-string (car marker)) (consult--line-with-cursor (cdr marker))) (cdr marker)
                   (line-number-at-pos pos consult-line-numbers-widen))
                  candidates)))))
    (nreverse (delete-dups candidates))))

(defun evil-collection-consult-mark ()
  "Jump to an evil marker in the current buffer."
  (interactive)
  (unwind-protect
      (progn
        (advice-add #'consult--mark-candidates :override
                    #'evil-collection-consult--mark-candidates)
        (consult-mark (evil-collection-consult--evil-mark-ring)))
    (advice-remove #'consult--mark-candidates
                   #'evil-collection-consult--mark-candidates)))

;;;###autoload
(defun evil-collection-consult-setup ()
  "Set up `evil' bindings for `consult'."
  (evil-collection-consult-set-bindings))

(provide 'evil-collection-consult)
;;; evil-collection-consult.el ends here
