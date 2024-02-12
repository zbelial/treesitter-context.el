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

(eval-when-compile
  (require 'treesitter-context-utils))

(defun treesitter-context-mark-bigger-node ()
  (treesitter-context--when-available-quiet
    (let* ((region-beg (region-beginning))
           (region-end (region-end))
           (root (treesit-buffer-root-node))
           (node (treesit-node-descendant-for-range root region-beg region-end))
           (node-start (treesit-node-start node))
           (node-end (treesit-node-end node)))
      (while (and (not (or (< node-start region-beg)
                           (> node-end region-end)))
                  node)
        (setq node (treesit-node-parent node))
        (when node
          (setq node-start (treesit-node-start node)
                node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start))))

(eval-after-load 'expand-region
  (add-to-list 'er/try-expand-list 'treesitter-context-mark-bigger-node))

(provide 'treesitter-context-expand)
