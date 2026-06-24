;;; evil-collection-docker.el --- Evil bindings for docker.el -*- lexical-binding: t -*-

;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
					docker-context-mode-map
                                        docker-image-mode-map
                                        docker-machine-mode-map
                                        docker-network-mode-map
                                        docker-volume-mode-map))

(defun evil-collection-docker-setup ()
  "Set up `evil' bindings for `docker'."
  (evil-collection-bind 'docker-container-mode-map 'rename 'docker-container-rename-selection)
  (evil-collection-define-key 'normal 'docker-container-mode-map
    ";"  'docker-container-ls
    "?"  'docker-container-help
    "C"  'docker-container-cp
    "D"  'docker-container-diff
    "I"  'docker-container-inspect
    "L"  'docker-container-logs
    "O"  'docker-container-stop
    "P"  'docker-container-pause
    "S"  'docker-container-start
    "a"  'docker-container-attach
    "b"  'docker-container-shells
    "f"  'docker-container-open
    "r"  'docker-container-restart)
  (evil-collection-bind 'docker-container-mode-map
                        'delete 'docker-container-rm
                        'delete-2 'docker-container-kill)

  (evil-collection-define-key 'normal 'docker-context-mode-map
    "?"  'docker-context-help
    "I"  'docker-context-inspect
    "X"  'docker-context-use)
  (evil-collection-bind 'docker-context-mode-map 'delete 'docker-context-rm)

  (evil-collection-define-key 'normal 'docker-image-mode-map
    ";"  'docker-image-ls
    "?"  'docker-image-help
    "D"  'docker-image-mark-dangling
    "F"  'docker-image-pull
    "I"  'docker-image-inspect
    "P"  'docker-image-push
    "R"  'docker-image-run
    "T"  'docker-image-tag-selection)
  (evil-collection-bind 'docker-image-mode-map 'delete 'docker-image-rm)

  (evil-collection-define-key 'normal 'docker-network-mode-map
    ";"  'docker-network-ls
    "?"  'docker-network-help
    "D"  'docker-network-mark-dangling
    "I"  'docker-network-inspect)
  (evil-collection-bind 'docker-network-mode-map 'delete 'docker-network-rm)

  (evil-collection-define-key 'normal 'docker-volume-mode-map
    ";"  'docker-volume-ls
    "?"  'docker-volume-help
    "D"  'docker-volume-mark-dangling
    "I"  'docker-volume-inspect
    "f"  'docker-volume-dired-selection)
  (evil-collection-bind 'docker-volume-mode-map 'delete 'docker-volume-rm))

(provide 'evil-collection-docker)

;;; evil-collection-docker.el ends here
