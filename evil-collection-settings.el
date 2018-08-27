;;; evil-collection-settings.el --- Settings for `evil-collection'. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
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
;;; Settings for `evil-collection'.
(require 'evil-collection)

;;; Code:
(defcustom evil-collection-settings-setup-debugger-keys t
  "Whether to bind debugger keys when debugger is active.

Debugger in this case is dependent on mode.

This is only relevant for debug modes that are part of another mode,

e.g. `indium'. Modes like `edebug' or `realgud' needs to be explicitly disabled

through removing their entry from `evil-collection-mode-list'."
  :type 'boolean
  :group 'evil-collection)

(provide 'evil-collection-settings)
;;; evil-collection-settings.el ends here
