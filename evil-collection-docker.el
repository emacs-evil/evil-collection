;;; evil-collection-docker.el --- Evil bindings for docker.el -*- lexical-binding: t -*-

;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, docker, tools

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
;; Evil bindings for docker.el

;;; Code:
(require 'docker nil t)
(require 'evil-collection)

(defconst evil-collection-docker-maps '(docker-container-mode-map
                                        docker-image-mode-map
                                        docker-machine-mode-map
                                        docker-network-mode-map
                                        docker-volume-mode-map))

(defun evil-collection-docker-setup ()
  "Set up `evil' bindings for `docker'."
  (evil-collection-define-key 'normal 'docker-container-mode-map
    ";"  'docker-container-ls-popup
    "?"  'docker-container-help-popup
    "C"  'docker-container-cp-popup
    "D"  'docker-container-rm-popup
    "I"  'docker-container-inspect-popup
    "K"  'docker-container-kill-popup
    "L"  'docker-container-logs-popup
    "O"  'docker-container-stop-popup
    "P"  'docker-container-pause-popup
    "R"  'docker-container-restart-popup
    "S"  'docker-container-start-popup
    "a"  'docker-container-attach-popup
    "b"  'docker-container-shell-popup
    "d"  'docker-container-diff-popup
    "f"  'docker-container-find-file-popup
    "g/" tablist-mode-filter-map
    "gr" 'revert-buffer
    "q"  'quit-window
    "r"  'docker-container-rename-selection)

  (evil-collection-define-key 'normal 'docker-image-mode-map
    ";"  'docker-image-ls-popup
    "?"  'docker-image-help-popup
    "D"  'docker-image-rm-popup
    "F"  'docker-image-pull-popup
    "I"  'docker-image-inspect-popup
    "P"  'docker-image-push-popup
    "R"  'docker-image-run-popup
    "T"  'docker-image-tag-selection
    "g/" tablist-mode-filter-map
    "gr" 'revert-buffer
    "q"  'quit-window)

  (evil-collection-define-key 'normal 'docker-machine-mode-map
    ";"  'docker-machine-ls-popup
    "?"  'docker-machine-help-popup
    "C"  'docker-machine-create
    "D"  'docker-machine-rm-popup
    "E"  'docker-machine-env-popup
    "O"  'docker-machine-stop-popup
    "R"  'docker-machine-restart-popup
    "S"  'docker-machine-start-popup
    "gr" 'revert-buffer
    "q"  'quit-window)

  (evil-collection-define-key 'normal 'docker-network-mode-map
    ";"  'docker-network-ls-popup
    "?"  'docker-network-help-popup
    "D"  'docker-network-rm-popup
    "gr" 'revert-buffer
    "q"  'quit-window)

  (evil-collection-define-key 'normal 'docker-volume-mode-map
    ";"  'docker-volume-ls-popup
    "?"  'docker-volume-help-popup
    "D"  'docker-volume-rm-popup
    "d"  'docker-volume-dired-selection
    "gr" 'revert-buffer
    "q"  'quit-window))

(provide 'evil-collection-docker)

;;; evil-collection-docker.el ends here
