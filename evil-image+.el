;;; evil-image+.el --- Evil bindings for image-mode with image+ -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, image, tools

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
;; Evil bindings for image-mode with image+.

;;; Code:
(require 'evil)
(require 'image+)

(defun evil-image+-setup ()
  (evil-define-key 'motion image-mode-map
    "+" 'imagex-sticky-zoom-in
    "-" 'imagex-sticky-zoom-out
    "M" 'imagex-sticky-maximize
    "m" 'imagex-auto-adjust-mode
    "O" 'imagex-sticky-restore-original
    "S" 'imagex-sticky-save-image
    "r" 'imagex-sticky-rotate-right
    "l" 'imagex-sticky-rotate-left))

(provide 'evil-image+)
;;; evil-image+.el ends here
