;;; evil-collection-ripgrep.el --- Bindings for `ripgrep' -*- lexical-binding: t -*-

;; Author: Sid Kasivajhula <sid@countvajhula.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
;; Bindings for `ripgrep'.

;;; Code:
(require 'evil-collection)
(require 'ripgrep nil t)

(defconst evil-collection-ripgrep-maps '(ripgrep-search-mode-map))

;;;###autoload
(defun evil-collection-ripgrep-setup ()
  "Set up `evil' bindings for `ripgrep'."
  (evil-collection-define-key 'normal 'ripgrep-search-mode-map
    "n" 'evil-search-next
    "{" 'compilation-previous-file
    "}" 'compilation-next-file
    "i" 'wgrep-change-to-wgrep-mode)
  (evil-collection-bind 'ripgrep-search-mode-map
                        'next-item 'next-error-no-select
                        'prev-item 'previous-error-no-select
                        'next-section 'next-error-no-select
                        'prev-section 'previous-error-no-select))


(provide 'evil-collection-ripgrep)
;;; evil-collection-ripgrep.el ends here
