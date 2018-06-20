;;; evil-collection-ace-jump-mode.el --- Bindings for `ace-jump-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for `ace-jump-mode'

;;; Code:
(require 'evil-collection)
(require 'ace-jump-mode nil t)

(declare-function 'ace-jump-char-mode "ace-jump-mode")
(declare-function 'ace-jump-word-mode "ace-jump-mode")
(declare-function 'ace-jump-line-mode "ace-jump-mode")

(defvar evil-collection-ace-jump-mode-jump-active nil)

(defmacro evil-collection-ace-jump-mode-enclose-ace-jump-for-motion (&rest body)
  "Enclose ace-jump to make it suitable for motions.
This includes restricting `ace-jump-mode' to the current window
in visual and operator state, deactivating visual updates, saving
the mark and entering `recursive-edit'."
  (declare (indent defun)
           (debug t))
  `(let ((old-mark (mark))
         (ace-jump-mode-scope
          (if (and (not (memq evil-state '(visual operator)))
                   (boundp 'ace-jump-mode-scope))
              ace-jump-mode-scope
            'window)))
     (ignore ace-jump-mode-scope) ;; Make byte compiler happy.
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (let ((evil-collection-ace-jump-mode-jump-active 'prepare))
           (add-hook 'ace-jump-mode-end-hook
                     #'evil-collection-ace-jump-mode-jump-exit-recursive-edit)
           ,@body
           (when evil-collection-ace-jump-mode-jump-active
             (setq evil-collection-ace-jump-mode-jump-active t)
             (recursive-edit)))
       (remove-hook 'post-command-hook
                    #'evil-collection-ace-jump-mode-jump-exit-recursive-edit)
       (remove-hook 'ace-jump-mode-end-hook
                    #'evil-collection-ace-jump-mode-jump-exit-recursive-edit)
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(defun evil-collection-ace-jump-mode-jump-exit-recursive-edit ()
  "Exit a recursive edit caused by an evil jump."
  (cond
   ((eq evil-collection-ace-jump-mode-jump-active 'prepare)
    (setq evil-collection-ace-jump-mode-jump-active nil))
   (evil-collection-ace-jump-mode-jump-active
    (remove-hook 'post-command-hook
                 #'evil-collection-ace-jump-mode-jump-exit-recursive-edit)
    (exit-recursive-edit))))

(evil-define-motion evil-ace-jump-char-mode (_)
  "Jump visually directly to a char using ace-jump."
  :type inclusive
  (evil-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (evil-collection-ace-jump-mode-enclose-ace-jump-for-motion
       (call-interactively 'ace-jump-char-mode))
      ;; if we jump backwards, motion type is exclusive, analogously
      ;; to `evil-find-char-backward'
      (when (and (equal buf (current-buffer))
                 (< (point) pnt))
        (setq evil-this-type
              (cond
               ((eq evil-this-type 'exclusive) 'inclusive)
               ((eq evil-this-type 'inclusive) 'exclusive)))))))

(evil-define-motion evil-ace-jump-char-to-mode (_)
  "Jump visually to the char in front of a char using ace-jump."
  :type inclusive
  (evil-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (evil-collection-ace-jump-mode-enclose-ace-jump-for-motion
       (call-interactively 'ace-jump-char-mode))
      (if (and (equal buf (current-buffer))
               (< (point) pnt))
          (progn
            (or (eobp) (forward-char))
            (setq evil-this-type
                  (cond
                   ((eq evil-this-type 'exclusive) 'inclusive)
                   ((eq evil-this-type 'inclusive) 'exclusive))))
        (backward-char)))))

(evil-define-motion evil-ace-jump-line-mode (_)
  "Jump visually to the beginning of a line using ace-jump."
  :type line
  :repeat abort
  (evil-without-repeat
    (evil-collection-ace-jump-mode-enclose-ace-jump-for-motion
     (call-interactively 'ace-jump-line-mode))))

(evil-define-motion evil-ace-jump-word-mode (_)
  "Jump visually to the beginning of a word using ace-jump."
  :type exclusive
  :repeat abort
  (evil-without-repeat
    (evil-collection-ace-jump-mode-enclose-ace-jump-for-motion
     (call-interactively 'ace-jump-word-mode))))

(defun evil-collection-ace-jump-mode-setup ()
  "Set up `evil' bindings for `ace-jump-mode'."

  (defadvice ace-jump-done (after evil activate)
    (when evil-collection-ace-jump-mode-jump-active
      (add-hook 'post-command-hook #'evil-collection-ace-jump-mode-jump-exit-recursive-edit)))

  (evil-collection-define-key nil 'evil-motion-state-map
    [remap ace-jump-char-mode] #'evil-ace-jump-char-mode
    [remap ace-jump-line-mode] #'evil-ace-jump-line-mode
    [remap ace-jump-word-mode] #'evil-ace-jump-word-mode))

(provide 'evil-collection-ace-jump-mode)
;;; evil-collection-ace-jump-mode.el ends here
