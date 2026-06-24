;;; evil-collection-vlf.el --- Evil bindings for vlf -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, vlf, tools

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
;; Evil bindings for `vlf'.

;;; Code:
(require 'evil-collection)
(require 'vlf nil t)

(defvar vlf-mode-map)
(declare-function vlf-change-batch-size "vlf")

(defconst evil-collection-vlf-maps '(vlf-mode-map))

(defun evil-collection-vlf-decrease-batch-size ()
  "Decrease vlf batch size by factor of 2."
  (interactive)
  (vlf-change-batch-size t))

;;; Code:
;;;###autoload
(defun evil-collection-vlf-setup ()
  "Set up `evil' bindings for `vlf'."
  (evil-set-initial-state 'vlf-mode 'normal)

  (add-hook 'vlf-mode-hook #'evil-normalize-keymaps)

  (evil-collection-define-key 'normal 'vlf-prefix-map
    "+" 'vlf-change-batch-size
    "-" 'evil-collection-vlf-decrease-batch-size
    "=" 'vlf-next-batch-from-point
    "g%" 'vlf-query-replace)
  (evil-collection-bind 'vlf-prefix-map
                        'action 'evil-ret
                        'action-other 'vlf-occur
                        'next-item 'vlf-next-batch
                        'prev-item 'vlf-prev-batch
                        'next-section 'vlf-next-batch
                        'prev-section 'vlf-prev-batch
                        'refresh 'vlf-revert
                        'jump 'vlf-jump-to-chunk)

  (if evil-collection-want-g-bindings
      (evil-collection-define-key 'normal 'vlf-prefix-map
        "g/" 'vlf-re-search-forward
        "g?" 'vlf-re-search-backward
        "gJ" 'vlf-jump-to-chunk
        "gE" 'vlf-ediff-buffers
        "g:" 'vlf-goto-line
        "gF" 'vlf-toggle-follow)
    (evil-collection-define-key 'normal 'vlf-prefix-map
      "S" 'vlf-re-search-backward
      "E" 'vlf-ediff-buffers
      "L" 'vlf-goto-line
      "F" 'vlf-toggle-follow)
    (evil-collection-bind 'vlf-prefix-map
                          'search-or-filter 'vlf-re-search-forward)))

(provide 'evil-collection-vlf)
;;; evil-collection-vlf.el ends here
