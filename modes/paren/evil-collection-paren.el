;;; evil-collection-paren.el --- Bindings for `paren'. -*- lexical-binding: t -*-

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
;;; Bindings for `paren'.

;;; Code:
(require 'paren)
(require 'evil-collection)

(defun evil-collection-paren-show-paren-function (f &rest args)
  "Integrate `show-paren-function' with `evil'."
  (if (not (bound-and-true-p evil-mode))
      (apply f args)
    (if (if (memq 'not evil-highlight-closing-paren-at-point-states)
            (memq evil-state evil-highlight-closing-paren-at-point-states)
          (not (memq evil-state evil-highlight-closing-paren-at-point-states)))
        (apply f args)
      (let ((pos (point)) syntax narrow)
        (setq pos
              (catch 'end
                (dotimes (var (1+ (* 2 evil-show-paren-range)))
                  (if (zerop (mod var 2))
                      (setq pos (+ pos var))
                    (setq pos (- pos var)))
                  (setq syntax (syntax-class (syntax-after pos)))
                  (cond
                   ((eq syntax 4)
                    (setq narrow pos)
                    (throw 'end pos))
                   ((eq syntax 5)
                    (throw 'end (1+ pos)))))))
        (if pos
            (save-excursion
              (goto-char pos)
              (save-restriction
                (when narrow
                  (narrow-to-region narrow (point-max)))
                (apply f args)))
          ;; prevent the preceding pair from being highlighted
          (dolist (ov '(show-paren--overlay
                        show-paren--overlay-1
                        show-paren-overlay
                        show-paren-overlay-1))
            (let ((ov (and (boundp ov) (symbol-value ov))))
              (when (overlayp ov) (delete-overlay ov)))))))))

(defun evil-collection-paren-setup ()
  "Set up `evil' bindings for `paren'."
  (advice-add 'show-paren-function
              :around 'evil-collection-paren-show-paren-function))

(provide 'evil-collection-paren)
;;; evil-collection-paren.el ends here
