;;; test-helper.el --- Helpers for evil-collection-test.el -*- lexical-binding: t -*-
(require 'ert)

(let ((evil-collection-dir
       (file-name-parent-directory
        (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path evil-collection-dir))
(require 'evil-collection)
;;; test-helper.el ends here
