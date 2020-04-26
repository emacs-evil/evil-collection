;;; evil-collection-xwidget.el --- Evil bindings for Xwidget -*- lexical-binding: t -*-

;; Copyright (C) 2020 Ruslan Kamashev

;; Author: Ruslan Kamashev <rynffoll@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, xwidget, tools

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
;; Evil bindings for Xwidget.

;;; Code:
(require 'xwidget)
(require 'evil-collection)

(defvar evil-collection-xwidget-maps '(xwidget-webkit-mode-map))

;;;###autoload
(defun evil-collection-xwidget-setup ()
  "Set up `evil' bindings for `xwidget'."
  (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
    "q" 'quit-window
    "k" 'xwidget-webkit-scroll-down-line
    "j" 'xwidget-webkit-scroll-up-line
    "h" 'xwidget-webkit-scroll-backward
    "l" 'xwidget-webkit-scroll-forward
    (kbd "C-f") 'xwidget-webkit-scroll-up
    (kbd "C-b") 'xwidget-webkit-scroll-down
    "+" 'xwidget-webkit-zoom-in
    "=" 'xwidget-webkit-zoom-in
    "-" 'xwidget-webkit-zoom-out
    "R" 'xwidget-webkit-reload
    "gr" 'xwidget-webkit-reload
    "H" 'xwidget-webkit-back
    "L" 'xwidget-webkit-forward
    "gu" 'xwidget-webkit-browse-url
    "gg" 'xwidget-webkit-scroll-top
    "G" 'xwidget-webkit-scroll-bottom
    "y" 'xwidget-webkit-copy-selection-as-kill)

  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
      (kbd "C-d") 'xwidget-webkit-scroll-up))
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
      (kbd "C-u") 'xwidget-webkit-scroll-down)))

(provide 'evil-collection-xwidget)
;;; evil-collection-xwidget.el ends here
