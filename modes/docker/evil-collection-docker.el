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

(declare-function transient-suffix-put "transient")

(defun evil-collection-docker--theme-key (id map-sym command)
  "Return the first key of theme entry ID for COMMAND in MAP-SYM.
Return nil when the theme entry is disabled."
  (let ((keys (evil-collection-binding-keys id map-sym command)))
    (when (evil-collection-binding-enabled-p id map-sym keys command)
      (car keys))))

(defun evil-collection-docker--set-transient-key (prefix loc key)
  "Set the key of the suffix at LOC in the transient PREFIX to KEY.
Do nothing when KEY is nil or when there is no suffix at LOC."
  (when key
    (ignore-errors
      (transient-suffix-put prefix loc :key key))))

(defun evil-collection-docker--adjust-transients ()
  "Make the `docker' transients use the keys of the `docker' buffers.
The suffix running the action of a transient gets the key that opens
this transient, so that pressing the key twice runs the action."
  (pcase-dolist (`(,map-sym ,help ,rm)
                 '((docker-container-mode-map
                    docker-container-help docker-container-rm)
                   (docker-context-mode-map
                    docker-context-help docker-context-rm)
                   (docker-image-mode-map
                    docker-image-help docker-image-rm)
                   (docker-network-mode-map
                    docker-network-help docker-network-rm)
                   (docker-volume-mode-map
                    docker-volume-help docker-volume-rm)))
    (let ((key (evil-collection-docker--theme-key 'delete map-sym rm)))
      (evil-collection-docker--set-transient-key rm "D" key)
      (evil-collection-docker--set-transient-key help rm key)))
  (let ((key (evil-collection-docker--theme-key
              'delete-2 'docker-container-mode-map 'docker-container-kill)))
    (evil-collection-docker--set-transient-key 'docker-container-kill "K" key)
    (evil-collection-docker--set-transient-key
     'docker-container-help 'docker-container-kill key))
  (evil-collection-docker--set-transient-key 'docker-container-diff "d" "D")
  (evil-collection-docker--set-transient-key 'docker-container-restart "R" "r")
  (pcase-dolist (`(,help ,command ,key)
                 '((docker-container-help docker-container-diff "D")
                   (docker-container-help docker-container-restart "r")
                   (docker-container-help docker-container-rename-selection "R")
                   (docker-image-help docker-image-mark-dangling "D")
                   (docker-network-help docker-network-mark-dangling "D")
                   (docker-volume-help docker-volume-mark-dangling "D")))
    (evil-collection-docker--set-transient-key help command key)))

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
  (evil-collection-bind 'docker-volume-mode-map 'delete 'docker-volume-rm)

  (evil-collection-docker--adjust-transients))

(provide 'evil-collection-docker)

;;; evil-collection-docker.el ends here
