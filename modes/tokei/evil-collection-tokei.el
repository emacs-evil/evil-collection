;;; evil-collection-tokei.el --- Evil-based key bindings for tokei.el

;; Copyright (C) 2022 jgart

;; Author: jgart <jgart@dismail.de>
;; Maintainer: jgart <jgart@dismail.de>
;; Package-Requires: ((emacs "26.3") (evil "1.2.3") (magit "2.6.0"))
;; Homepage: https://github.com/emacs-evil/evil-collection
;; Version: 0.4.1

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

;; This library configures Evil and tokei.el to play well with each
;; other. See https://github.com/nagy/tokei.el

;;; Code:

(require 'evil-collection-magit)


(evil-collection-magit-setup)

(provide 'evil-collection-tokei)
;;; evil-collection-tokei.el ends here
