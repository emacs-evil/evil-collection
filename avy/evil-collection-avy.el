;;; evil-collection-avy.el --- Bindings for `avy' -*- lexical-binding: t -*-

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
;;; Bindings for `avy'

;;; Code:
(require 'avy nil t)
(require 'evil-collection)

(declare-function 'avy-goto-word-or-subword-1 "avy")
(declare-function 'avy-goto-line "avy")
(declare-function 'avy-goto-char "avy")
(declare-function 'avy-goto-char-2 "avy")
(declare-function 'avy-goto-char-2-above "avy")
(declare-function 'avy-goto-char-2-below "avy")
(declare-function 'avy-goto-char-in-line "avy")
(declare-function 'avy-goto-word-0 "avy")
(declare-function 'avy-goto-word-1 "avy")
(declare-function 'avy-goto-word-1-above "avy")
(declare-function 'avy-goto-word-1-below "avy")
(declare-function 'avy-goto-subword-0 "avy")
(declare-function 'avy-goto-subword-1 "avy")
(declare-function 'avy-goto-char-timer "avy")

(defmacro evil-collection-avy-enclose-avy-for-motion (&rest body)
  "Enclose avy to make it suitable for motions.
Based on `evil-collection-ace-jump-mode-enclose-ace-jump-for-motion'."
  (declare (indent defun)
           (debug t))
  `(let ((avy-all-windows
          (if (and (not (memq evil-state '(visual operator)))
                   (boundp 'avy-all-windows))
              avy-all-windows
            nil)))
     (ignore avy-all-windows) ;; Make byte compiler happy.
     ,@body))

(defmacro evil-collection-avy-define-avy-motion (command type)
  (declare (indent defun)
           (debug t))
  (let ((name (intern (format "evil-%s" command))))
    `(evil-define-motion ,name (_count)
       ,(format "Evil motion for `%s'." command)
       :type ,type
       :jump t
       :repeat abort
       (evil-without-repeat
         (evil-collection-avy-enclose-avy-for-motion
          (call-interactively ',command))))))

;; define evil-avy-* motion commands for avy-* commands
(evil-collection-avy-define-avy-motion avy-goto-char inclusive)
(evil-collection-avy-define-avy-motion avy-goto-char-2 inclusive)
(evil-collection-avy-define-avy-motion avy-goto-char-2-above inclusive)
(evil-collection-avy-define-avy-motion avy-goto-char-2-below inclusive)
(evil-collection-avy-define-avy-motion avy-goto-char-in-line inclusive)
(evil-collection-avy-define-avy-motion avy-goto-char-timer inclusive)
(evil-collection-avy-define-avy-motion avy-goto-line line)
(evil-collection-avy-define-avy-motion avy-goto-line-above line)
(evil-collection-avy-define-avy-motion avy-goto-line-below line)
(evil-collection-avy-define-avy-motion avy-goto-subword-0 exclusive)
(evil-collection-avy-define-avy-motion avy-goto-subword-1 exclusive)
(evil-collection-avy-define-avy-motion avy-goto-symbol-1 exclusive)
(evil-collection-avy-define-avy-motion avy-goto-symbol-1-above exclusive)
(evil-collection-avy-define-avy-motion avy-goto-symbol-1-below exclusive)
(evil-collection-avy-define-avy-motion avy-goto-word-0 exclusive)
(evil-collection-avy-define-avy-motion avy-goto-word-1 exclusive)
(evil-collection-avy-define-avy-motion avy-goto-word-1-above exclusive)
(evil-collection-avy-define-avy-motion avy-goto-word-1-below exclusive)
(evil-collection-avy-define-avy-motion avy-goto-word-or-subword-1 exclusive)

(defun evil-collection-avy-setup ()
  "Set up `evil' bindings for `avy'."
  ;; remap avy-* commands to evil-avy-* commands
  (dolist (command '(avy-goto-char
                     avy-goto-char-2
                     avy-goto-char-2-above
                     avy-goto-char-2-below
                     avy-goto-char-in-line
                     avy-goto-char-timer
                     avy-goto-line
                     avy-goto-line-above
                     avy-goto-line-below
                     avy-goto-subword-0
                     avy-goto-subword-1
                     avy-goto-symbol-1
                     avy-goto-symbol-1-above
                     avy-goto-symbol-1-below
                     avy-goto-word-0
                     avy-goto-word-1
                     avy-goto-word-1-above
                     avy-goto-word-1-below
                     avy-goto-word-or-subword-1))
    (evil-collection-define-key nil 'evil-motion-state-map
      (vector 'remap command) (intern-soft (format "evil-%s" command)))))

(provide 'evil-collection-avy)
;;; evil-collection-avy.el ends here
