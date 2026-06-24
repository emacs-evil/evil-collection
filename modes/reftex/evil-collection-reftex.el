;;; evil-collection-reftex.el --- Bindings for `reftex' -*- lexical-binding: t -*-

;; Copyright (C) 2017 Pierre Neidhardt

;; Author: Maximiliano Sandoval <msandova@protonmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: evil, reftex, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for `reftex-mode'.

;;; Code:
(require 'evil-collection)
(require 'reftex-ref nil t)
(require 'reftex-ref nil t)
(require 'reftex-cite nil t)
(require 'reftex-toc nil t)

(defconst evil-collection-reftex-maps '(reftex-select-shared-map
                                        reftex-toc-mode-map))

;; original code can be found in reftex-ref.el
(setq reftex-select-label-prompt
  "Select: [RET]select [j]next [k]previous [gr]escan [go]context [q]uit [g?]help")

;; original code can be found in reftex-cite.el
(setq reftex-citation-prompt
  "Select: [RET]select [j]next [k]previous [q]uit [g?]help")

;; original at reftex-ref.el
(setq reftex-select-label-help
  " j / k      Go to next/previous label (Cursor motion works as well)
 [ / ]      Go to previous/next section heading.
 c          Reuse last referenced label.
 J          Jump to a specific section, e.g. '3 J' jumps to section 3.
 s          Switch label type.
 gr         Reparse document.
 go / gO     Show context / Show insertion point.
 S          Switch to label menu of external document (with LaTeX package `xr').
 r / R      Toggle \\ref <-> \\vref / Rotate \\ref <=> \\fref <=> \\Fref.
 TAB        Enter a label with completion.
 m / M      Mark/unmark entry.
 x / X      Put all marked entries into one/many \\ref commands.
 q / RET    Quit without referencing / Accept current label.")

;; code can be found in reftex-cite.el
(setq reftex-citation-help
  " j / k      Go to next/previous entry (Cursor motion works as well).
 go / gO     Show citation / Show insertion point.
 q          Quit without inserting \\cite macro into buffer.
 TAB        Enter citation key with completion.
 RET        Accept current entry and create \\cite macro.
 m / M      Mark/Unmark the entry.
 o / O      Create BibTeX file with all marked / unmarked entries.
 X / X      Put all (marked) entries into one/many \\cite commands.")

(setq reftex-toc-help
      "                      AVAILABLE KEYS IN TOC BUFFER
                      ============================
    j / k      next-line / previous-line
    go         Show the corresponding location of the LaTeX document.
    TAB        Goto the location and keep the TOC window.
    RET        Goto the location and hide the TOC window (also on mouse-2).
    < / >      Promote / Demote section, or all sections in region.
    zi         Display Index. With prefix arg, restrict index to current section.
    q / ZZ     Hide/Kill *toc* buffer, return to position of reftex-toc command.
    l i c F    Toggle display of  [l]abels,  [i]ndex,  [c]ontext,  [F]ile borders.
    f          Toggle follow mode
    gr / gR    Reparse the LaTeX document / Reparse entire LaTeX document.
    .          In other window, show position from where `reftex-toc' was called.
    rl         Global search and replace to rename label at point.
    x          Switch to TOC of external document (with LaTeX package `xr').
    J          Jump to a specific section (e.g. '3 J' goes to section 3).")

;;;###autoload
(defun evil-collection-reftex-setup ()
  "Set up `evil' bindings for `reftex'."

  (evil-set-initial-state 'reftex-select-label-mode 'normal)
  (evil-set-initial-state 'reftex-select-bib-mode 'normal)

  (evil-collection-define-key 'normal 'reftex-select-shared-map
    "j" 'reftex-select-next
    "k" 'reftex-select-previous
    "gr" (lambda nil "Press `?' during selection to find out
    about this key" (interactive) (throw (quote myexit) 114)) ;reftex binds keys in a very arcane way using the number asigned by describe-char, in this case the value of "g" is 114
    "c" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 108))
    (kbd "<tab>") 'reftex-select-read-label
    "s" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 115))
    "x" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 97))
    "X" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 65))
    "S" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 120))
    "r" 'reftex-select-cycle-ref-style-forward
    "R" 'reftex-select-cycle-ref-style-backward
    "o" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 101))
    "O" (lambda nil "Press `?' during selection to find out
    about this key." (interactive) (throw (quote myexit) 69))

    ;; mark
    "m" 'reftex-select-mark             ; TODO: Need a mark toggle function.
    "u" 'reftex-select-unmark)
  (evil-collection-bind 'reftex-select-shared-map
                        'action 'reftex-select-accept
                        'action-other 'reftex-select-callback ; shows point where label is
                        'action-stay 'reftex-select-show-insertion-point
                        'next-item 'reftex-select-next-heading
                        'prev-item 'reftex-select-previous-heading
                        'next-section 'reftex-select-next-heading
                        'prev-section 'reftex-select-previous-heading
                        'quit 'reftex-select-quit
                        'quit-save 'reftex-select-quit
                        'quit-cancel 'evil-quit
                        'describe-mode 'reftex-select-help
                        'jump 'reftex-select-jump)

  (evil-set-initial-state 'reftex-toc-mode 'normal)

  ;; This one is more involved, in reftex-toc.el, line 267 it shows the prompt
  ;; string with the keybinds and I don't see any way of changing it to show evil-like binds.
  ;; Per the upstream `reftex-toc-show-help' text:
  ;;   go  / RET           Show the corresponding location of the LaTeX document
  ;;                       (RET also hides the TOC window)
  ;;   TAB / gO            Goto the location and keep the TOC window
  (evil-collection-define-key 'normal 'reftex-toc-mode-map
    "j" 'reftex-toc-next
    "k" 'reftex-toc-previous
    (kbd "<tab>") 'reftex-toc-goto-line
    "?" 'reftex-toc-show-help
    "l" 'reftex-toc-toggle-labels
    "i" 'reftex-toc-toggle-index
    "c" 'reftex-toc-toggle-context
    "F" 'reftex-toc-toggle-file-boundary
    "rl" 'reftex-toc-rename-label
    "zi" 'reftex-toc-display-index
    "x" 'reftex-toc-external
    "." 'reftex-toc-show-calling-point
    (kbd ">") 'reftex-toc-promote
    (kbd "<") 'reftex-toc-demote
    "f" 'reftex-toc-toggle-follow)
  (evil-collection-bind 'reftex-toc-mode-map
                        'action 'reftex-toc-goto-line-and-hide
                        'action-other 'reftex-toc-view-line
                        'action-stay 'reftex-toc-goto-line
                        'next-section 'reftex-toc-next-heading
                        'prev-section 'reftex-toc-previous-heading
                        'quit 'reftex-toc-quit
                        'quit-save 'reftex-toc-quit-and-kill
                        'quit-cancel 'evil-quit
                        'describe-mode 'reftex-toc-show-help
                        'refresh 'reftex-toc-rescan
                        'refresh-all 'reftex-toc-Rescan
                        'jump 'reftex-toc-jump))

(provide 'evil-collection-reftex)
;;; evil-collection-reftex.el ends here
