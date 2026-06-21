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
  (evil-collection-bind 'hyrolo-mode-map
                        'next-item #'hyrolo-to-next-loc
                        'prev-item #'hyrolo-to-previous-loc
                        'next-section #'hyrolo-to-next-loc
                        'prev-section #'hyrolo-to-previous-loc)
  (evil-set-initial-state 'hyrolo-mode 'normal))

(provide 'evil-collection-hyrolo)
;;; evil-collection-hyrolo.el ends here
