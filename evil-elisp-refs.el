;;; evil-elisp-refs.el --- Evil integration for Elisp Refs -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, elisp-refs, tools
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
;; This package provides a sane set of defaults for `elisp-refs' when using
;; `evil-mode'.
(require 'evil-collection-util)
(require 'elisp-refs)

(defun evil-elisp-refs-set-keys ()
  (evil-define-key 'normal elisp-refs-mode-map
    (kbd "<tab>") #'elisp-refs-next-match
    (kbd "<backtab>") #'elisp-refs-prev-match
    (kbd "C-j") #'elisp-refs-next-match
    (kbd "C-k") #'elisp-refs-prev-match
    (kbd "gj") #'elisp-refs-next-match
    (kbd "gk") #'elisp-refs-prev-match
    (kbd "q") #'kill-this-buffer
    (kbd "RET") #'elisp-refs-visit-match))

(provide 'evil-elisp-refs)
;;; evil-elisp-refs.el ends here
