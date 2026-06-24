;;; evil-collection-compile.el --- Evil bindings for `compile' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, compile, tools

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
;; Evil bindings for `compile'.

;;; Code:
(require 'evil-collection)
(require 'compile)

(defconst evil-collection-compile-maps '(compilation-mode-map
                                         compilation-minor-mode-map))

;;;###autoload
(defun evil-collection-compile-setup ()
  "Set up `evil' bindings for `compile'."
  (evil-set-initial-state 'compilation-mode 'normal)
  (evil-collection-set-readonly-bindings 'compilation-mode-map)

  (dolist (keymap evil-collection-compile-maps)

    (evil-collection-define-key nil keymap
      "g" nil)

    (evil-collection-bind keymap
                          'action 'compile-goto-error
                          'action-other 'compilation-display-error
                          'action-stay 'compilation-display-error
                          'next-item 'compilation-next-error
                          'prev-item 'compilation-previous-error
                          'next-section 'compilation-next-file
                          'prev-section 'compilation-previous-file
                          'next-section-2 'compilation-next-error
                          'prev-section-2 'compilation-previous-error
                          'cycle-next 'compilation-next-error
                          'cycle-previous 'compilation-previous-error
                          'refresh 'recompile)))

(provide 'evil-collection-compile)
;;; evil-collection-compile.el ends here
