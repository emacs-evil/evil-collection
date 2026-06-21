;;; evil-collection-hyrolo.el --- Bindings for `hyrolo' -*- lexical-binding: t -*-

;;; Commentary:
;;; Bindings for hyrolo.

;;; Code:
(require 'evil-collection)
(require 'hyperbole nil t)

(defvar hyrolo-mode-map)
(defconst evil-collection-hyrolo-maps '(hyrolo-mode-map))

(declare-function hyrolo-to-previous-loc "hyrolo")
(declare-function hyrolo-to-next-loc "hyrolo")
(declare-function hyrolo-overview "hyrolo")
(declare-function hyrolo-outline-show-all "hyrolo")

(defun evil-collection-hyrolo-setup ()
  "Set up `evil' bindings for hyrolo."
  (evil-collection-define-key 'normal 'hyrolo-mode-map
    (kbd "zo") #'hyrolo-outline-show-all
    (kbd "zc") #'hyrolo-overview)
  (evil-collection-theme-bind 'next-item    'hyrolo-mode-map #'hyrolo-to-next-loc)
  (evil-collection-theme-bind 'prev-item    'hyrolo-mode-map #'hyrolo-to-previous-loc)
  (evil-collection-theme-bind 'next-section 'hyrolo-mode-map #'hyrolo-to-next-loc)
  (evil-collection-theme-bind 'prev-section 'hyrolo-mode-map #'hyrolo-to-previous-loc)
  (evil-set-initial-state 'hyrolo-mode 'normal))

(provide 'evil-collection-hyrolo)
;;; evil-collection-hyrolo.el ends here
