;;; evil-collection-corfu.el --- Bindings for `corfu-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2022 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: evil, corfu, convenience, matching

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
;;; Bindings for `corfu-mode'.

;;; Code:
(require 'corfu nil t)
(require 'evil-collection)

(defgroup evil-collection-corfu nil
  "Evil bindings for `corfu-mode'."
  :group 'evil-collection)

(defvar corfu-map)

(defconst evil-collection-corfu-maps '(corfu-map))

(defun evil-collection-corfu-quit-and-escape ()
  "Call `corfu-quit' and then return to Normal State."
  (interactive)
  (call-interactively 'corfu-quit)
  (evil-normal-state))

;;;###autoload
(defun evil-collection-corfu-setup ()
  "Set up `evil' bindings for `corfu'."
  (evil-collection-define-key 'insert 'corfu-map
    (kbd "C-n") 'corfu-next
    (kbd "C-p") 'corfu-previous
    (kbd "C-j") 'corfu-next
    (kbd "C-k") 'corfu-previous
    (kbd "M-j") 'corfu-next
    (kbd "M-k") 'corfu-previous
    (kbd "<escape>") 'evil-collection-corfu-quit-and-escape)
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'insert 'corfu-map
      (kbd "C-u") 'corfu-scroll-up))
  (when evil-want-C-d-scroll
    (evil-collection-define-key 'insert 'corfu-map
      (kbd "C-d") 'corfu-scroll-down))

  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))

(provide 'evil-collection-corfu)
;;; evil-collection-corfu.el ends here
