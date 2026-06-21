;;; evil-collection-comint.el --- Bindings for `comint-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, comint, processes

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
;;; Bindings for `comint-mode'.

;;; Code:
(require 'comint)
(require 'evil-collection)

(defconst evil-collection-comint-maps '(comint-mode-map))

;;;###autoload
(defun evil-collection-comint-setup ()
  "Set up `evil' bindings for `comint'."
  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'comint-mode-map
      (kbd "C-d") #'evil-scroll-down))

  (evil-collection-bind 'repl-submit        'comint-mode-map #'comint-send-input)
  (evil-collection-bind 'repl-newline       'comint-mode-map #'newline)
  (evil-collection-bind 'repl-force-newline 'comint-mode-map #'newline)

  ;; Match Eshell bindings.
  (evil-collection-bind 'next-item    'comint-mode-map #'comint-next-prompt)
  (evil-collection-bind 'prev-item    'comint-mode-map #'comint-previous-prompt)
  (evil-collection-bind 'next-section 'comint-mode-map #'comint-next-prompt)
  (evil-collection-bind 'prev-section 'comint-mode-map #'comint-previous-prompt)

  (evil-collection-define-key 'normal 'comint-mode-map
    (kbd "C-p") #'comint-previous-input
    (kbd "C-n") #'comint-next-input)

  ;; TODO: What if the user changes `evil-want-C-u-delete' after this is run?
  (when evil-want-C-u-delete
    (evil-collection-define-key 'insert 'comint-mode-map
      (kbd "C-u") #'comint-kill-input))

  (evil-collection-define-key 'insert 'comint-mode-map
    (kbd "<up>") #'comint-previous-input
    (kbd "<down>") #'comint-next-input))

(provide 'evil-collection-comint)
;;; evil-collection-comint.el ends here
