;;; evil-slime.el --- Evil integration for `slime'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, slime, emacs
;; HomePage: https://github.com/jojojames/evil-collection

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
;; Evil integration for `slime-mode'.

;;; Code:
(require 'evil-collection-util)
(require 'slime)

(defun evil-collection-set-keys ()
  (+evilify-map
   sldb-mode-map
   :mode sldb-mode
   :bindings
   "H" 'describe-mode
   "\C-j" 'sldb-down
   "\C-k" 'sldb-up
   "\M-j" 'sldb-details-down
   "\M-k" 'sldb-details-up
   "gb" 'sldb-break-on-return
   "gB" 'sldb-break-with-default-debugger)

  (+evilify-map
   slime-inspector-mode-map
   :mode slime-inspector-mode
   :bindings
   "gr" 'slime-inspector-reinspect)

  (evil-define-key 'normal slime-popup-buffer-mode-map
    (kbd "q") 'quit-window
    (kbd "M-.") 'slime-edit-definition)

  (add-hook 'slime-popup-buffer-mode-hook #'evil-normalize-keymaps))

(provide 'evil-slime)
;;; evil-slime.el ends here
