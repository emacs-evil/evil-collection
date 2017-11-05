;;; evil-cider.el --- Evil Integration for Cider -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, cider, tools
;; HomePage: https://github.com/jojojames/evil-collection

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
;; This package provides a sane set of defaults for `cider-mode' when using
;; `evil-mode'.

;;; Code:
(require 'cider)
(require 'evil-collection-util)

(defun evil-cider-set-keys ()
  (+evil-bind-key (normal visual)
                  (cider-mode-map)
                  (kbd "gz") 'cider-switch-to-repl-buffer
                  (kbd "gf") 'cider-find-resource
                  (kbd "K")  'cider-doc)

  (with-eval-after-load 'cider-repl
    (+evil-bind-key (normal visual)
                    (cider-repl-mode-map)
                    (kbd "gf") 'cider-find-resource
                    (kbd "K")  'cider-doc))

  (with-eval-after-load 'cider-test
    (evil-define-key 'normal cider-test-report-mode-map
      (kbd "C-c ,") 'cider-test-commands-map
      (kbd "C-c C-t") 'cider-test-commands-map
      (kbd "M-p") #'cider-test-previous-result
      (kbd "M-n") #'cider-test-next-result
      (kbd "M-.") #'cider-test-jump
      (kbd "<backtab>") #'cider-test-previous-result
      (kbd "TAB") #'cider-test-next-result
      (kbd "RET") #'cider-test-jump
      (kbd "t") #'cider-test-jump
      (kbd "d") #'cider-test-ediff
      (kbd "e") #'cider-test-stacktrace
      "f" #'cider-test-rerun-failed-tests
      "n" #'cider-test-run-ns-tests
      "L" #'cider-test-run-loaded-tests
      "p" #'cider-test-run-project-tests
      "gr" #'cider-test-run-test
      "q" #'cider-popup-buffer-quit-function))

  (with-eval-after-load 'cider-macroexpansion
    (evil-define-key 'normal cider-macroexpansion-mode-map
      (kbd "r") #'cider-macroexpand-again
      (kbd "q") #'cider-popup-buffer-quit-function
      (kbd "K") #'cider-doc
      (kbd "J") #'cider-javadoc
      (kbd ".") #'cider-find-var
      (kbd "m") #'cider-macroexpand-1-inplace
      (kbd "a") #'cider-macroexpand-all-inplace
      (kbd "u") #'cider-macroexpand-undo
      [remap undo] #'cider-macroexpand-undo))

  (with-eval-after-load 'cider-client
    (evil-define-key 'normal cider-connections-buffer-mode-map
      "d" #'cider-connections-make-default
      "g" #'cider-connection-browser
      "x" #'cider-connections-close-connection
      (kbd "RET") #'cider-connections-goto-connection
      "?" #'describe-mode
      "H" #'describe-mode)))

(provide 'evil-cider)
;;; evil-cider.el ends here
