;;; evil-collection-guix.el --- Evil bindings for Guix -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, guix, tools

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
;; Evil bindings for `guix'.

;;; Code:
(require 'evil-collection)
(require 'guix nil t)

(defconst evil-collection-guix-maps '(guix-output-list-mode-map
                                      guix-package-info-mode-map
                                      guix-profile-list-mode-map
                                      guix-profile-info-mode-map
                                      guix-generation-list-mode-map
                                      guix-generation-info-mode-map
                                      guix-license-list-mode-map
                                      guix-license-info-mode-map
                                      guix-location-list-mode-map
                                      guix-hydra-build-list-mode-map
                                      guix-hydra-build-info-mode-map
                                      guix-build-log-mode-map
                                      guix-service-list-mode-map))

(defmacro evil-collection-guix-set (map)
  "Set common bindings in MAP."
  `(progn
     (evil-collection-set-readonly-bindings ',map)
     (evil-collection-define-key 'normal ',map
        ;; motion
        "gm" 'guix-apply-manifest

        ;; sort
        "o" 'bui-list-sort

        ;; filter
        "ss" 'bui-enable-filter
        "sd" 'bui-disable-filter

        ;; mark
        "m" 'bui-list-mark
        "M" 'bui-list-mark-all
        "u" 'bui-list-unmark
        "U" 'bui-list-unmark-all

        (kbd "C-o") 'bui-history-back
        (kbd "C-i") 'bui-history-forward)
     (evil-collection-bind ',map
                           'next-button 'forward-button
                           'previous-button 'backward-button
                           'scroll-down 'scroll-up-command
                           'scroll-up 'scroll-down-command
                           'goto-repl 'guix-switch-to-repl
                           'describe-mode 'bui-show-hint
                           'refresh 'revert-buffer
                           'refresh-all 'bui-redisplay)))

;;;###autoload
(defun evil-collection-guix-setup ()
  "Set up `evil' bindings for `guix'."
  (evil-collection-define-push-action evil-collection-guix-bui-list-describe-action
                                      bui-list-describe)
  (evil-collection-define-push-action evil-collection-guix-location-list-action
                                      guix-location-list-show-packages)
  (evil-collection-guix-set guix-ui-map) ; Covers output-list and generation-list.

  (evil-collection-bind 'guix-output-list-mode-map
                        'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-output-list-mode-map
    "gb" 'guix-package-list-latest-builds
    "gG" 'guix-output-list-graph
    "gl" 'guix-output-list-lint
    "gs" 'guix-package-list-size

    "a" 'guix-output-list-mark-upgrade
    "A" 'guix-output-list-mark-upgrades
    "d" 'guix-output-list-mark-delete
    "gd" 'guix-output-list-edit
    "i" 'guix-output-list-mark-install

    ;; mark
    "x" 'guix-output-list-execute)

  (evil-collection-guix-set guix-package-info-mode-map)
  (evil-collection-define-key 'normal 'guix-package-info-mode-map
    "gG" 'guix-package-info-graph
    "gl" 'guix-package-info-lint
    "gs" 'guix-package-info-size

    "a" 'guix-package-info-upgrade
    "d" 'guix-package-info-delete
    "gd" 'guix-package-info-edit
    "i" 'guix-package-info-install)

  (evil-collection-guix-set guix-profile-list-mode-map)
  (evil-collection-bind 'guix-profile-list-mode-map 'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-profile-list-mode-map
    "c" 'guix-profile-list-set-current ; TODO: Bind to "." as per the rationale?
    "p" 'guix-profile-list-show-packages
    "a" 'guix-profile-list-show-generations
    "P" 'guix-profile-list-show-search-paths)

  (evil-collection-guix-set guix-profile-info-mode-map)
  (evil-collection-define-key 'normal 'guix-profile-info-mode-map
    "gm" 'guix-profile-info-apply-manifest

    "p" 'guix-profile-info-show-packages
    "a" 'guix-profile-info-show-generations
    "P" 'guix-profile-info-show-search-paths
    "c" 'guix-profile-info-set-current)

  (evil-collection-bind 'guix-generation-list-mode-map 'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-generation-list-mode-map
    "p" 'guix-generation-list-show-packages
    "D" 'guix-generation-list-mark-delete
    "P" 'guix-generation-list-show-search-paths
    "c" 'guix-generation-list-set-current
    "d" 'guix-generation-list-ediff

    "+" 'guix-generation-list-show-added-packages
    "-" 'guix-generation-list-show-removed-packages
    "=" 'guix-generation-list-diff

    ;; mark
    "x" 'guix-generation-list-execute)

  (evil-collection-guix-set guix-license-list-mode-map)
  (evil-collection-bind 'guix-license-list-mode-map 'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-license-list-mode-map
    "p" 'guix-license-list-show-packages
    "gd" 'guix-license-list-edit)

  (evil-collection-guix-set guix-license-info-mode-map)

  (evil-collection-guix-set guix-location-list-mode-map)
  (evil-collection-bind 'guix-location-list-mode-map
                        'action 'evil-collection-guix-location-list-action)
  (evil-collection-define-key 'normal 'guix-location-list-mode-map
    "p" 'guix-location-list-show-packages
    "gd" 'guix-location-list-edit)

  (evil-collection-guix-set guix-store-item-list-mode-map)
  (evil-collection-bind 'guix-store-item-list-mode-map 'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-store-item-list-mode-map
    "d" 'guix-store-item-list-mark-delete
    "gd" 'guix-store-item-list-edit
    "x" 'guix-store-item-list-execute)

  (evil-collection-guix-set guix-store-item-info-mode-map)

  (evil-collection-guix-set guix-hydra-build-list-mode-map)
  (evil-collection-bind 'guix-hydra-build-list-mode-map 'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-hydra-build-list-mode-map
    "gb" 'guix-hydra-build-list-latest-builds
    "gl" 'guix-hydra-build-list-view-log)

  (evil-collection-guix-set guix-hydra-build-info-mode-map)

  (evil-collection-set-readonly-bindings 'guix-build-log-mode-map)
  (evil-collection-bind 'guix-build-log-mode-map
                        'scroll-down 'scroll-up-command
                        'scroll-up 'scroll-down-command
                        'next-item 'guix-build-log-next-phase
                        'prev-item 'guix-build-log-previous-phase
                        'next-section 'guix-build-log-next-phase
                        'prev-section 'guix-build-log-previous-phase
                        'goto-repl 'guix-switch-to-repl
                        'refresh 'revert-buffer
                        'section-toggle 'guix-build-log-phase-toggle
                        'section-toggle-all 'guix-build-log-phase-toggle-all)

  (evil-collection-guix-set guix-service-list-mode-map)
  (evil-collection-bind 'guix-service-list-mode-map 'action 'evil-collection-guix-bui-list-describe-action)
  (evil-collection-define-key 'normal 'guix-service-list-mode-map
    "gd" 'guix-service-list-edit)

  (evil-collection-bind 'guix-devel-mode-map 'goto-repl 'guix-switch-to-repl))

(provide 'evil-collection-guix)
;;; evil-collection-guix.el ends here
