;;; evil-collection-ovpn-mode.el --- Bindings for `ovpn-mode' -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vitor Leal

;; Author: Vitor Leal <hello@vitorl.com>
;; Maintainer: Vitor Leal <hello@vitorl.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, emacs, convenience, tools

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
;;; Bindings for ovpn-mode.

;;; Code:
(require 'evil-collection)
(require 'ovpn-mode nil t)

(defvar ovpn-mode-map)
(defconst evil-collection-ovpn-mode-maps '(ovpn-mode-map))

;;;###autoload
(defun evil-collection-ovpn-mode-setup ()
  "Set up `evil' bindings for `ovpn-mode'."
  (evil-collection-define-key 'normal 'ovpn-mode-map
    "s" 'ovpn-mode-start-vpn
    "n" 'ovpn-mode-start-vpn-with-namespace
    "q" 'ovpn-mode-stop-vpn
    "r" 'ovpn-mode-restart-vpn
    "i" 'ovpn-mode-info-vpn
    "$" 'ovpn-mode-buffer-vpn
    "o" 'ovpn-mode-edit-vpn
    "~" 'ovpn-mode-dir-filter
    "d" 'ovpn-mode-dir-set
    "a" 'ovpn-mode-active
    "J" 'ovpn-mode-ipv6-linux-toggle
    "T" 'ovpn-mode-spawn-term-in-namespace
    "B" 'ovpn-mode-spawn-browser-in-namespace
    "ZZ" 'quit-window
    "ZQ" 'evil-quit))

(provide 'evil-collection-ovpn-mode)
;;; evil-collection-ovpn-mode.el ends here
