;;; evil-collection-xwidget.el --- Evil bindings for Xwidget -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2021 Ruslan Kamashev

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

(defmacro evil-collection-xwidget-half-page-height ()
  "Return Emacs xwidget half window height in pixel."
  (let ((edges (window-inside-pixel-edges (selected-window))))
    (/ (- (nth 3 edges) (nth 1 edges)) 2)))

(defun evil-collection-xwidget-webkit-scroll-up-half-page ()
  "Scroll webkit up by half page."
  (interactive)
  (if (>= emacs-major-version 28)
      (xwidget-webkit-scroll-up (evil-collection-xwidget-half-page-height))
    (xwidget-webkit-scroll-up)))

(defun evil-collection-xwidget-webkit-scroll-down-half-page ()
  "Scroll webkit down by half page."
  (interactive)
  (if (>= emacs-major-version 28)
      (xwidget-webkit-scroll-down (evil-collection-xwidget-half-page-height))
    (xwidget-webkit-scroll-down)))

;;;###autoload
(defun evil-collection-xwidget-setup ()
  "Set up `evil' bindings for `xwidget'."
  (evil-collection-set-readonly-bindings 'xwidget-webkit-mode-map)
  (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
    ;; motion
    ;;
    ;; d/u are widely used in browser extension vimium.
    "j"         'xwidget-webkit-scroll-up-line
    "k"         'xwidget-webkit-scroll-down-line
    "h"         'xwidget-webkit-scroll-backward
    "l"         'xwidget-webkit-scroll-forward
    (kbd "C-f") 'xwidget-webkit-scroll-up
    (kbd "C-b") 'xwidget-webkit-scroll-down
    "d"         'evil-collection-xwidget-webkit-scroll-up-half-page
    "u"         'evil-collection-xwidget-webkit-scroll-down-half-page
    "gg"        'xwidget-webkit-scroll-top
    "G"         'xwidget-webkit-scroll-bottom
    ;; history browsing
    "H"         'xwidget-webkit-back
    "L"         'xwidget-webkit-forward
    "B"         'xwidget-webkit-browse-history
    ;; zoom
    "+"         'xwidget-webkit-zoom-in
    "="         'xwidget-webkit-zoom-in
    "-"         'xwidget-webkit-zoom-out
    ;; loading
    "R"         'xwidget-webkit-reload
    "gr"        'xwidget-webkit-reload
    ;; misc
    (kbd "RET") 'xwidget-webkit-insert-string
    "A"         'xwidget-webkit-adjust-size-dispatch
    "gu"        'xwidget-webkit-browse-url
    "W"         'xwidget-webkit-current-url)

  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
      (kbd "C-d") 'evil-collection-xwidget-webkit-scroll-up-half-page))
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
      (kbd "C-u") 'evil-collection-xwidget-webkit-scroll-down-half-page)))

(provide 'evil-collection-xwidget)
;;; evil-collection-xwidget.el ends here
