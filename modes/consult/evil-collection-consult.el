;;; evil-collection-consult.el --- Evil bindings for consult -*- lexical-binding: t -*-

;; Copyright (C) 2017, 2024 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
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
;;
;; Since
;;
;; `consult-outline'
;; `consult-mark'
;; `consult-global-mark'
;; `consult-imenu'
;; `consult-org-heading'
;; `consult-org-agenda'
;; `consult-line'
;;
;; are all autoloaded, no (require 'consult nil t) is needed.

;;; Code:
(require 'evil-collection)

(defvar consult-line-numbers-widen)
(declare-function consult--forbid-minibuffer "consult")
(declare-function consult--fontify-all "consult")
(declare-function consult--in-range-p "consult")
(declare-function consult--line-with-mark "consult")
(declare-function consult--location-candidate "consult")
(declare-function consult--remove-dups "consult")
(declare-function consult--mark-candidates "consult")
(declare-function consult-mark "consult")
(declare-function consult-global-mark "consult")

(defun evil-collection-consult-set-bindings ()
  "Set the bindings."
  (dolist (cmd '(consult-outline
                 consult-mark
                 consult-global-mark
                 consult-imenu
                 consult-org-heading
                 consult-org-agenda
                 consult-line))
    (evil-declare-not-repeat cmd)
    (evil-set-command-property cmd :jump t)))

(defun evil-collection-consult--evil-mark-ring (show-local show-global)
  "Return alist of char and marker based on SHOW-LOCAL and SHOW-GLOBAL."
  (let* ((curbuf (current-buffer))
         (local (if show-local
                    (cl-remove-if-not
                     (lambda (elem)
                       (let ((char (car elem))
                             (marker (cdr elem)))
                         (and (markerp marker)
                              (eq (marker-buffer marker) curbuf)
                              (>= char ?a) (<= char ?z))))
                     evil-markers-alist)
                  nil))
         (global-marker-table (default-value 'evil-markers-alist))
         (global
          (pcase show-global
            ('all
             ;; Include global marks from all live buffers.
             (cl-remove-if-not
              (lambda (elem)
                (let ((char (car elem))
                      (marker (cdr elem)))
                  (and (markerp marker)
                       (>= char ?A) (<= char ?Z)
                       (marker-buffer marker) ; live buffer
                       (buffer-live-p (marker-buffer marker)))))
              global-marker-table))
            ('local
             ;; Include global marks only within the current buffer.
             (cl-remove-if-not
              (lambda (elem)
                (let ((char (car elem))
                      (marker (cdr elem)))
                  (and (markerp marker)
                       (eq (marker-buffer marker) curbuf)
                       (>= char ?A) (<= char ?Z))))
              global-marker-table))
            (_ nil)))) ; nil -> don't include global
    (append local global)))

(defun evil-collection-consult--mark-candidates (markers scope)
  "Format Evil MARKERS for use with consult.
SCOPE is a cons pair (SHOW-LOCAL . SHOW-GLOBAL).

Markers from the current buffer are shown first (sorted by line number),
followed by markers from other buffers (sorted by buffer name and line number)."
  (consult--forbid-minibuffer)
  (let ((candidates)
        (origin-buffer (current-buffer)))  ;; capture origin buffer
    (save-excursion
      (pcase-dolist (`(,char . ,marker) markers)
        (let ((pos (marker-position marker))
              (buf (marker-buffer marker)))
          (when (and pos buf (buffer-live-p buf))
            (with-current-buffer buf
              (when (consult--in-range-p pos)
                (goto-char pos)
                (let* ((line       (consult--line-with-mark marker))
                       (line-num   (line-number-at-pos pos consult-line-numbers-widen))
                       (is-local   (and (>= char ?a) (<= char ?z)))
                       (is-global  (and (>= char ?A) (<= char ?Z)))
                       (same-buffer (eq buf origin-buffer))
                       (show-local  (car scope))
                       (show-global (cdr scope))
                       (title
                        (cond
                         ;; Local marker: simplified
                         ((and is-local show-local)
                          (format "%s: %s" (char-to-string char) line))
                         ;; Global from current buffer: show filename only if (nil . 'all)
                         ((and is-global same-buffer
                               (not show-local)
                               (eq show-global 'all))
                          (format "%c: %s %s"
                                  char
                                  (or (file-name-nondirectory (buffer-file-name buf))
                                      (buffer-name buf))
                                  line))
                         ;; Global in current buffer: simplified
                         ((and is-global same-buffer)
                          (format "%s: %s" (char-to-string char) line))
                         ;; Global from other buffers: full format
                         (t
                          (format "%c: %s %s"
                                  char
                                  (or (file-name-nondirectory (buffer-file-name buf))
                                      (buffer-name buf))
                                  line)))))
                  (push (list title marker line-num buf) candidates))))))))
    ;; Sort candidate list: current buffer first, then by buffer-name + line-number
    (let ((sorted
           (sort candidates
                 (lambda (a b)
                   (let ((buf-a  (nth 3 a))
                         (buf-b  (nth 3 b))
                         (line-a (nth 2 a))
                         (line-b (nth 2 b)))
                     (cond
                      ;; If same buffer → sort by line number
                      ((eq buf-a buf-b)
                       (< line-a line-b))
                      ;; Current buffer comes first
                      ((eq buf-a origin-buffer) t)
                      ((eq buf-b origin-buffer) nil)
                      ;; Otherwise lexicographically by buffer name
                      (t (string< (buffer-name buf-a)
                                  (buffer-name buf-b)))))))))
      ;; Create consult-location candidates from sorted entries
      (mapcar (lambda (entry)
                (let ((title  (nth 0 entry))
                      (marker (nth 1 entry))
                      (line   (nth 2 entry)))
                  (consult--location-candidate
                   title marker line marker)))
              sorted))))

(defun evil-collection-consult--mark (show-local show-global)
  "Jump to Evil markers based on SHOW-LOCAL and SHOW-GLOBAL.
SHOW-LOCAL is t or nil (include local a-z markers).
SHOW-GLOBAL is one of:
  - nil        => exclude global (A-Z) markers
  - 'local     => include global markers from current buffer only
  - 'all       => include global markers from all buffers

Together, these selective flags decide what set of evil markers to include."
  (interactive)
  (let ((markers (evil-collection-consult--evil-mark-ring show-local show-global)))
    (if (null markers)
        (user-error "No marks")
      (cl-letf (((symbol-function 'consult--mark-candidates)
                 (lambda (&optional _ignore)
                   (evil-collection-consult--mark-candidates
                    markers
                    (cons show-local show-global)))))
        (consult-mark markers)))))


;;;###autoload
(defun evil-collection-consult-mark ()
  "Jump to a local/lowercase evil marker in the current buffer.
Same as `evil-collection-consult-mark-buffer-local'"
  (interactive)
  (evil-collection-consult--mark t nil))

;;;###autoload
(defun evil-collection-consult-mark-global ()
  "Jump to a global/uppercase evil marker in open buffers."
  (interactive)
  (evil-collection-consult--mark nil 'all))

;;;###autoload
(defun evil-collection-consult-mark-buffer ()
  "Jump to a local/lowercase or local/uppercase evil marker in the current buffer."
  (interactive)
  (evil-collection-consult--mark t 'local))

;;;###autoload
(defun evil-collection-consult-mark-buffer-local ()
  "Jump to a local/lowercase evil marker in the current buffer."
  (interactive)
  (evil-collection-consult--mark t nil))

;;;###autoload
(defun evil-collection-consult-mark-buffer-global ()
  "Jump to a local/uppercase evil marker in the current buffer."
  (interactive)
  (evil-collection-consult--mark nil 'local))

;;;###autoload
(defun evil-collection-consult-mark-all ()
  "Jump to a alocal/lowercase, local/uppercase or global/uppercase evil marker.
Global/uppercase markers are shown for all open buffers."
  (interactive)
  (evil-collection-consult--mark t 'all))

;; TODO Update version
(define-obsolete-function-alias
  'evil-collection-consult-mark
  'evil-collection-consult-mark-local
  "1.0.0")


;;;###autoload
(defun evil-collection-consult-jump-list ()
  "Jump to a position in the evil jump list."
  (interactive)
  (consult-global-mark (delq nil (mapcar (lambda (jump)
                                           (let ((mark (car jump)))
                                             (when (markerp mark)
                                               mark)))
                                         (ring-elements (evil--jumps-get-window-jump-list))))))

;;;###autoload
(defun evil-collection-consult-setup ()
  "Set up `evil' bindings for `consult'."
  (evil-collection-consult-set-bindings))

(provide 'evil-collection-consult)
;;; evil-collection-consult.el ends here
