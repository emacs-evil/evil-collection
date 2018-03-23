;;; evil-collection-racer.el --- Bindings for `racer'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: emacs, rust, tools, evil

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
;; Bindings for `racer'.

;;; Code:
(require 'evil)
(require 'racer nil t)

(defconst evil-collection-racer-maps '(racer-mode-map
                                       racer-help-mode-map))

(defun evil-collection-racer-setup ()
  "Set up `evil' bindings for `racer'."
  (evil-define-key 'normal racer-mode-map
    "gd" 'racer-find-definition
    (kbd "C-t") 'pop-tag-mark
    "K" 'racer-describe)

  (evil-define-key 'normal racer-help-mode-map
    "q" 'quit-window))

(provide 'evil-collection-racer)
;;; evil-collection-racer.el ends here
