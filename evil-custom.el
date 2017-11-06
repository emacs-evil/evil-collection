;;; evil-custom.el --- Add Evil bindings to Customize

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Package-Requires: ((evil "1.2.3"))
;; Package-Version: 20170724.1223
;; Homepage: https://github.com/Ambrevar/evil-special-modes
;; Version: 0

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

;;; Code:

(require 'evil)
(require 'cus-edit)

;;;###autoload
(defun evil-custom-set-keys ()
  (evil-set-initial-state 'Custom-mode 'normal)

  (evil-define-key 'normal custom-mode-map
    ;; motion
    (kbd "<tab>") 'widget-forward
    (kbd "S-<tab>") 'widget-backward
    (kbd "<backtab>") 'widget-backward
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "<delete>") 'scroll-down-command
    (kbd "<return>") 'Custom-newline
    (kbd "]") 'widget-forward
    (kbd "[") 'widget-backward
    ;; TODO: Should the following be added?
    (kbd "C-j") 'widget-forward
    (kbd "C-k") 'widget-backward

    "^" 'Custom-goto-parent
    (kbd "C-o") 'Custom-goto-parent
    ;; TODO: Should the following be added?
    "<" 'Custom-goto-parent

    ;; quit
    "q" 'Custom-buffer-done
    "ZQ" 'evil-quit
    "ZZ" 'Custom-buffer-done))

(provide 'evil-custom)
;;; evil-custom.el ends here
