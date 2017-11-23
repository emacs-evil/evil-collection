;;; evil-cus-theme.el --- Bindings for `cus-theme'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, custom themes, help, faces

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
;;; Bindings for `cus-theme'.

;;; Code:
(require 'cus-theme)
(require 'evil)

(defun evil-cus-theme-setup ()
  "Set up `evil' bindings for `cus-theme'."
  (evil-set-initial-state 'custom-new-theme-mode 'normal)
  (evil-set-initial-state 'custom-theme-choose-mode 'normal)

  (evil-define-key 'normal custom-theme-choose-mode-map
    "gj" 'widget-forward
    "gk" 'widget-backward
    (kbd "]") 'widget-forward
    (kbd "[") 'widget-backward
    (kbd "C-j") 'widget-forward
    (kbd "C-k") 'widget-backward
    "K" 'custom-describe-theme)

  (evil-define-key 'normal custom-new-theme-mode-map
    "gj" 'widget-forward
    "gk" 'widget-backward
    (kbd "]") 'widget-forward
    (kbd "[") 'widget-backward
    (kbd "C-j") 'widget-forward
    (kbd "C-k") 'widget-backward

    ;; quit
    "q" 'Custom-buffer-done
    "ZQ" 'evil-quit
    "ZZ" 'Custom-buffer-done))

(provide 'evil-cus-theme)
;;; evil-cus-theme.el ends here
