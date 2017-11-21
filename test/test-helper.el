;;; test-helper.el --- Helpers for evil-collection-test.el -*- lexical-binding: t -*-
(require 'ert)

;; FIXME: Adding `f' as a dependency just for this line.
(require 'f)
(let ((evil-collection-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path evil-collection-dir))
(require 'evil-collection)
;;; test-helper.el ends here
