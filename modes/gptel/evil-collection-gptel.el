;;; evil-collection-gptel.el --- Bindings for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Zhiwei Chen <condy0919@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: emacs

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
;;; Bindings for gptel.

;;; Code:
(require 'evil-collection)
(require 'gptel nil t)


(defconst evil-collection-gptel-maps
  '(gptel-mode-map gptel-context-buffer-mode-map))

(defcustom evil-collection-gptel-want-ret-to-send t
  "When non nil, RET sends query to LLM."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-gptel-want-shift-ret-to-send t
  "When non nil, S-RET sends query to LLM."
  :group 'evil-collection
  :type 'boolean)

(defcustom evil-collection-gptel-want-shift-ret-menu t
  "When non nil, S-RET opens calls `gptel-menu'. This takes priority over
 `evil-collection-gptel-want-shift-ret-to-send'."
  :group 'evil-collection
  :type 'boolean)

;;;###autoload
(defun evil-collection-gptel-setup ()
  "Set up `evil' bindings for gptel."
  (when evil-collection-gptel-want-ret-to-send
    (evil-collection-define-key '(normal visual) 'gptel-mode-map
      (kbd "RET") 'gptel-send))
  (if evil-collection-gptel-want-shift-ret-menu
      (evil-collection-define-key '(normal visual) 'gptel-mode-map
        (kbd "S-RET") 'gptel-menu
        (kbd "<S-return>") 'gptel-menu)
    (when evil-collection-gptel-want-shift-ret-to-send
      (evil-collection-define-key '(normal visual) 'gptel-mode-map
        (kbd "S-RET") 'gptel-send
        (kbd "<S-return>") 'gptel-send)))

  (with-eval-after-load 'gptel-context
    (evil-collection-define-key 'normal 'gptel-context-buffer-mode-map
      "j" 'gptel-context-next
      "k" 'gptel-context-previous
      "d" 'gptel-context-flag-deletion
      "q" 'gptel-context-quit
      "ZQ" 'gptel-context-quit
      "ZZ" 'gptel-context-confirm
      "RET" 'gptel-context-visit)))

(provide 'evil-collection-gptel)
;;; evil-collection-gptel.el ends here
