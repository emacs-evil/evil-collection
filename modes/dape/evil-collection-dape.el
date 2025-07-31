;;; evil-collection-dape.el --- Bindings for `dape' -*- lexical-binding: t -*-

;; Copyright (C) 2024 Daniel Pettersson

;; Author: Daniel Pettersson <daniel@dpettersson.net>
;; Maintainer: Daniel Pettersson <daniel@dpettersson.net>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, tools, dape

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
;; Bindings for `dape'.

;;; Code:
(require 'evil-collection)
(require 'dape nil t)

(defvar dape-info-scope-mode-map)
(defvar dape-info-watch-mode-map)
(defvar dape-info-stack-mode-map)
(defvar dape-info-sources-mode-map)
(defvar dape-info-modules-mode-map)
(defvar dape-info-breakpoints-mode-map)
(defvar dape-info-threads-mode-map)

(defconst evil-collection-dape--info-mode-maps '(dape-info-scope-mode-map
                                                 dape-info-watch-mode-map
                                                 dape-info-stack-mode-map
                                                 dape-info-sources-mode-map
                                                 dape-info-modules-mode-map
                                                 dape-info-breakpoints-mode-map
                                                 dape-info-threads-mode-map))

(defconst evil-collection-dape-maps `(dape-info-watch-edit-mode-map
                                      dape-memory-mode-map
                                      . ,evil-collection-dape--info-mode-maps))

;;;###autoload
(defun evil-collection-dape-setup ()
  "Set up `evil' bindings for `dape'."
  (dolist (map-symbol evil-collection-dape--info-mode-maps)
    (evil-make-overriding-map (symbol-value map-symbol)))

  (evil-collection-define-key 'normal 'dape-info-watch-mode-map
    "i" 'dape-info-watch-edit-mode)

  (evil-collection-define-key nil 'dape-info-watch-edit-mode-map
    [remap evil-write] 'dape-info-watch-finish-edit)

  (evil-collection-define-key 'normal 'dape-memory-mode-map
    [remap evil-write] 'save-buffer
    "ZZ" 'save-buffer)

  (evil-collection-define-key 'normal 'dape-info-watch-edit-mode-map
    "ZQ" 'dape-info-watch-abort-changes
    "ZZ" 'dape-info-watch-finish-edit
    (kbd "<escape>") 'dape-info-watch-finish-edit))

(provide 'evil-collection-dape)
;;; evil-collection-dape.el ends here
