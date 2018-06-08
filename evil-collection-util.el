;;; evil-collection-util.el --- Utilities for `evil-collection'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/jojojames/evil-collection
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
;;; Bindings for

;;; Code:
(require 'evil)

(declare-function evil-collection-define-key "evil-collection")

(defmacro evil-collection-util-inhibit-insert-state (map)
  "Unmap insertion keys from normal state.
This is particularly useful for read-only modes."
  `(evil-collection-define-key
     'normal ',map
     [remap evil-append] #'ignore
     [remap evil-append-line] #'ignore
     [remap evil-insert] #'ignore
     [remap evil-insert-line] #'ignore
     [remap evil-change] #'ignore
     [remap evil-change-line] #'ignore
     [remap evil-substitute] #'ignore
     [remap evil-change-whole-line] #'ignore
     [remap evil-delete] #'ignore
     [remap evil-delete-line] #'ignore
     [remap evil-delete-char] #'ignore
     [remap evil-delete-backward-char] #'ignore
     [remap evil-replace] #'ignore
     [remap evil-replace-state] #'ignore
     [remap evil-open-below] #'ignore
     [remap evil-open-above] #'ignore
     [remap evil-paste-after] #'ignore
     [remap evil-paste-before] #'ignore
     [remap evil-join] #'ignore
     [remap evil-indent] #'ignore
     [remap evil-shift-left] #'ignore
     [remap evil-shift-right] #'ignore
     [remap evil-invert-char] #'ignore))

(provide 'evil-collection-util)
;;; evil-collection-util.el ends here
