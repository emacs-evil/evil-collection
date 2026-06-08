;;; evil-collection-fj.el --- Evil bindings for fj -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil bindings for fj.el.

;;; Code:
(require 'evil-collection)
(require 'fj nil t)

(defvar fj-commits-mode-map)
(defvar fj-compose-comment-mode)
(defvar fj-compose-comment-mode-map)
(defvar fj-compose-mode)
(defvar fj-compose-mode-map)
(defvar fj-generic-map)
(defvar fj-generic-tl-map)
(defvar fj-issue-tl-mode-map)
(defvar fj-item-view-mode-map)
(defvar fj-notifications-mode-map)
(defvar fj-owned-issues-tl-mode-map)
(defvar fj-repo-tl-map)
(defvar fj-repo-tl-mode-map)
(defvar fj-user-repo-tl-mode-map)
(defvar fj-users-mode-map)

(defconst evil-collection-fj-maps
  '(fj-commits-mode-map
    fj-compose-comment-mode-map
    fj-compose-mode-map
    fj-generic-map
    fj-generic-tl-map
    fj-issue-tl-mode-map
    fj-item-view-mode-map
    fj-notifications-mode-map
    fj-owned-issues-tl-mode-map
    fj-repo-tl-map
    fj-repo-tl-mode-map
    fj-user-repo-tl-mode-map
    fj-users-mode-map))

(defconst evil-collection-fj-view-maps
  '(fj-commits-mode-map
    fj-item-view-mode-map
    fj-notifications-mode-map
    fj-users-mode-map))

(defconst evil-collection-fj-tabulated-list-maps
  '(fj-issue-tl-mode-map
    fj-owned-issues-tl-mode-map
    fj-repo-tl-map
    fj-repo-tl-mode-map
    fj-user-repo-tl-mode-map))

(defun evil-collection-fj--setup-compose-buffer ()
  "Set up Evil bindings for the current fj compose buffer."
  (evil-local-set-key 'normal "ZQ" 'fj-compose-cancel)
  (evil-local-set-key 'normal "ZZ" 'fj-compose-send)
  (evil-insert-state))

;;;###autoload
(defun evil-collection-fj-setup ()
  "Set up `evil' bindings for fj.el."
  (dolist (mode '(fj-commits-mode
                  fj-issue-tl-mode
                  fj-item-view-mode
                  fj-notifications-mode
                  fj-owned-issues-tl-mode
                  fj-repo-tl-mode
                  fj-user-repo-tl-mode
                  fj-users-mode))
    (evil-set-initial-state mode 'normal))

  (dolist (map evil-collection-fj-view-maps)
    (evil-collection-set-readonly-bindings map))
  (dolist (map evil-collection-fj-tabulated-list-maps)
    (evil-collection-set-readonly-bindings map))

  (evil-collection-define-key 'normal 'fj-generic-map
    "gb" 'fj-switch-to-buffer
    "go" 'fj-browse-view
    "gr" 'fj-view-reload
    "]]" 'fj-item-next
    "[[" 'fj-item-prev
    "gj" 'fj-item-next
    "gk" 'fj-item-prev
    (kbd "C-j") 'fj-item-next
    (kbd "C-k") 'fj-item-prev
    (kbd "<tab>") 'fj-next-tab-item
    (kbd "<backtab>") 'fj-prev-tab-item)

  (evil-collection-define-key 'normal 'fj-generic-tl-map
    "gb" 'fj-switch-to-buffer
    "go" 'fj-tl-browse-entry
    "gm" 'imenu
    "gr" 'fj-view-reload
    ">" 'fj-next-page
    "<" 'fj-prev-page
    (kbd "<tab>") 'fj-next-tab-item
    (kbd "<backtab>") 'fj-prev-tab-item)

  (evil-collection-define-key 'normal 'fj-repo-tl-map
    (kbd "RET") 'fj-repo-list-issues
    (kbd "M-RET") 'fj-repo-list-pulls
    "C" 'fj-create-issue
    "r" 'fj-repo-readme
    "g/" 'fj-repo-search
    "gm" 'imenu
    "go" 'fj-tl-browse-entry)
  (evil-collection-define-operator-key 'yank 'fj-repo-tl-map
    "c" 'fj-repo-copy-clone-url)

  (evil-collection-define-key 'normal 'fj-issue-tl-mode-map
    (kbd "RET") 'fj-issues-tl-view
    "c" 'fj-item-comment
    "C" 'fj-create-issue
    "e" 'fj-item-edit
    "E" 'fj-item-edit-title
    "g/" 'fj-list-issues-search
    "x" 'fj-item-close
    "X" 'fj-item-delete
    "u" 'fj-item-reopen
    "al" 'fj-item-label-add
    "gL" 'fj-repo-commit-log
    "gm" 'imenu
    "go" 'fj-tl-browse-entry)
  (evil-collection-define-operator-key 'yank 'fj-issue-tl-mode-map
    "c" 'fj-repo-copy-clone-url
    "u" 'fj-copy-item-url)

  (evil-collection-define-key 'normal 'fj-item-view-mode-map
    "c" 'fj-item-comment
    "e" 'fj-item-edit
    "E" 'fj-item-edit-title
    "r" 'fj-add-reaction
    "g/" 'fj-list-issues-search
    "x" 'fj-item-close
    "X" 'fj-item-delete
    "u" 'fj-item-reopen
    "al" 'fj-item-label-add
    "ar" 'fj-add-reaction
    "gd" 'fj-view-pull-diff
    "gL" 'fj-repo-commit-log
    "gM" 'fj-merge-pull
    ">" 'fj-item-view-more)
  (evil-collection-define-operator-key 'yank 'fj-item-view-mode-map
    "u" 'fj-copy-item-url)

  (evil-collection-define-key 'normal 'fj-notifications-mode-map
    "g/" 'fj-list-issues-search
    ">" 'fj-next-page
    "<" 'fj-prev-page)

  (evil-collection-define-key 'normal 'fj-commits-mode-map
    "g/" 'fj-list-issues-search
    "gL" 'fj-repo-commit-log)

  (evil-collection-define-key 'normal 'fj-users-mode-map
    "g/" 'fj-list-issues-search
    "gL" 'fj-repo-commit-log
    ">" 'fj-next-page
    "<" 'fj-prev-page)

  (add-hook 'fj-compose-comment-mode-hook
            #'evil-collection-fj--setup-compose-buffer)
  (add-hook 'fj-compose-mode-hook
            #'evil-collection-fj--setup-compose-buffer))

(provide 'evil-collection-fj)
;;; evil-collection-fj.el ends here
