;;; evil-collection-log-edit.el --- Evil bindings for log-edit -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, log-edit, tools

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
;; Evil bindings for log-edit.

;;; Code:
(require 'evil-collection)
(require 'log-edit nil t)

(defvar log-edit-mode-map)

(defconst evil-collection-log-edit-maps '(log-edit-mode-map))

;;;###autoload
(defun evil-collection-log-edit-setup ()
  "Set up `evil' bindings for `log-edit'."
  (evil-collection-define-key nil 'log-edit-mode-map
    [remap evil-save-and-close] 'log-edit-done
    [remap evil-save-modified-and-close] 'log-edit-done
    [remap evil-quit] 'log-edit-kill-buffer)

  (evil-collection-bind 'log-edit-mode-map
                        'describe-mode 'log-edit-mode-help
                        'quit-save 'quit-window
                        'quit-cancel 'quit-window
                        'next-item 'log-edit-next-comment
                        'prev-item 'log-edit-previous-comment
                        'next-section 'log-edit-next-comment
                        'prev-section 'log-edit-previous-comment))

(provide 'evil-collection-log-edit)
;;; evil-collection-log-edit.el ends here
