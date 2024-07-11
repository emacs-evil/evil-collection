;;; evil-collection-hyrolo.el --- Bindings for `hyrolo' -*- lexical-binding: t -*-

;;; Commentary:
;;; Bindings for hyrolo.

;;; Code:
(require 'evil-collection)
(require 'hyperbole nil t)

(defvar eldoc-mode-map)
(defconst evil-collection-eldoc-maps '(hyrolo-mode-map))

(defun evil-collection-hyrolo-setup ()
  "Set up `evil' bindings for hyrolo."
  (evil-collection-define-key 'normal 'hyrolo-mode-map
    (kbd "[[") #'hyrolo-to-previous-loc
    (kbd "]]") #'hyrolo-to-next-loc
    (kbd "C-k") #'hyrolo-to-previous-loc
    (kbd "C-j") #'hyrolo-to-next-loc
    (kbd "gk") #'hyrolo-to-previous-loc
    (kbd "gj") #'hyrolo-to-next-loc
    (kbd "zo") #'hyrolo-outline-show-all
    (kbd "zc") #'hyrolo-overview))

(provide 'evil-collection-hyrolo)
;;; evil-collection-hyrolo.el ends here
