;;; evil-collection-hackernews.el --- Evil bindings for hackernews-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, news, tools

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
;; Evil bindings for hackernews.

;;; Code:
(require 'evil-collection)
(require 'hackernews nil t)

(defconst evil-collection-hackernews-maps '(hackernews-mode-map
                                            hackernews-button-map))

;;;###autoload
(defun evil-collection-hackernews-setup ()
  "Set up `evil' bindings for `hackernews-mode'."
  (evil-collection-set-readonly-bindings 'hackernews-mode-map)
  (evil-collection-theme-bind 'next-item    'hackernews-mode-map 'hackernews-next-item)
  (evil-collection-theme-bind 'prev-item    'hackernews-mode-map 'hackernews-previous-item)
  (evil-collection-theme-bind 'next-section 'hackernews-mode-map 'hackernews-next-item)
  (evil-collection-theme-bind 'prev-section 'hackernews-mode-map 'hackernews-previous-item)
  (evil-collection-define-key 'normal 'hackernews-mode-map
    "+" 'hackernews-load-more-stories

    "gf" 'hackernews-switch-feed

    (kbd "<tab>") 'hackernews-next-comment
    (kbd "S-<tab>") 'hackernews-previous-comment

    "!" 'hackernews-button-mark-as-visited ; like mu4e
    "?" 'hackernews-button-mark-as-unvisited

    (kbd "RET") 'hackernews-button-browse-internal
    (kbd "S-<return>") 'push-button)
  (evil-collection-theme-bind 'refresh 'hackernews-mode-map 'hackernews-reload))

(provide 'evil-collection-hackernews)
;;; evil-collection-hackernews.el ends here
