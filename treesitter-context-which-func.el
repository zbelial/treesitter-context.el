;;; treesitter-context.el --- Show context information around current point -*- lexical-binding: t; -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (posframe "1.4.2"))
;; Homepage: https://bitbucket.org/zbelial/treesitter-context
;; Keywords: Package Emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'treesitter-context)
(require 'treesitter-context-common)
(eval-when-compile
  (require 'treesitter-context-utils))

;;;###autoload
(defvar treesitter-context-which-func-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;###autoload
(define-minor-mode treesitter-context-which-func-mode
  "Enable treesitter-based which-func."
  :global nil
  :init-value nil
  :keymap treesitter-context-which-func-mode-map
  (if treesitter-context-which-func-mode
      (add-hook 'which-func-functions #'treesitter-context-which-func-function)
    (remove-hook 'which-func-functions #'treesitter-context-which-func-function)))

(provide 'treesitter-context-which-func)
