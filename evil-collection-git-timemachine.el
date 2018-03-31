;;; evil-collection-git-timemachine.el --- Bindings for `git-timemachine' -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Evil keybindings for `git-timemachine' that conform to the principles outlines in evil-collection

;;; Code:
(require 'evil)
(require 'git-timemachine)

(defun ecgt--setup-bindings ()
  "Setup `git-timemachine' since `evil-set-initial-state' is unavailable to
minor modes."
  (evil-motion-state nil)
  (evil-local-set-key 'motion "n" #'git-timemachine-show-next-revision))

(defun evil-collection-git-timemachine-setup ()
  "Setup `evil' keybindings for `git-timemachine'."
  (add-hook 'git-timemachine-mode-hook #'ecgt--setup-bindings))

(provide 'evil-collection-git-timemachine)
;;; evil-collection-git-timemachine.el ends here
