;;; evil-collection-git-timemachine.el --- Bindings for `git-timemachine' -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection

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
;; Evil keybindings for `git-timemachine' that conform to the principles
;; outlines in evil-collection

;;; Code:
(require 'evil)
(require 'git-timemachine)

(defun ecgt--setup-bindings ()
  "Setup `git-timemachine' since `evil-set-initial-state' is unavailable to
minor modes."
  (evil-motion-state nil)
  (evil-local-set-key 'motion "n" #'git-timemachine-show-next-revision))

(defun evil-collection-git-timemachine-setup ()
  "Setup `evil' keybindings for `git-timemachine'."
  (add-hook 'git-timemachine-mode-hook #'ecgt--setup-bindings))

(provide 'evil-collection-git-timemachine)
;;; evil-collection-git-timemachine.el ends here
