;;; evil-collection-etags-select.el --- Bindings for `etags-select'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, emacs, tools

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
;;; Bindings for `etags-select'.

;;; Code:
(require 'evil-collection)
(require 'etags-select nil t)

(defun evil-collection-etags-select-setup ()
  "Set up `evil' bindings for `etags-select'.."
  ;; FIXME: probably etags-select should be recomended in docs
  (evil-collection-define-key nil 'evil-motion-state-map
    "g]" 'etags-select-find-tag-at-point))

(provide 'evil-collection-etags-select)
;;; evil-collection-etags-select.el ends here
