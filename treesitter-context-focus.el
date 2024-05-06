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

(defface treesitter-context-focus-unfocused
  '((t :inherit shadow))
  "The face that overlays the unfocused area."
  :group 'treesitter-context)

(defface treesitter-context-focus-focused nil
  "The face that overlays the focused area."
  :group 'treesitter-context)

(defvar-local treesitter-context-focus-buffer nil
  "Local reference to the buffer focus functions operate on.")

(defvar-local treesitter-context-focus-pre-overlay nil
  "The overlay that dims the text prior to the current-point context.")

(defvar-local treesitter-context-focus-mid-overlay nil
  "The overlay that surrounds the text of the current-point.")

(defvar-local treesitter-context-focus-post-overlay nil
  "The overlay that dims the text past the current-point context.")

(defun treesitter-context-focus-move-focus ()
  "Move the focused section according to `focus-bounds'.

If `treesitter-context-focus-mode' is enabled, this command fires after each
command."
  (with-current-buffer treesitter-context-focus-buffer
    (let ((bounds (treesitter-context-focus-bounds)))
      (when bounds
        (treesitter-context-focus-move-overlays (nth 1 bounds) (nth 2 bounds))))))

(defun treesitter-context-focus-move-overlays (low high)
  "Move the overlays to highlight the region between LOW and HIGH."
  (move-overlay treesitter-context-focus-pre-overlay (point-min) low)
  (move-overlay treesitter-context-focus-mid-overlay low high)
  (move-overlay treesitter-context-focus-post-overlay high (point-max)))

(defun treesitter-context-focus-init ()
  "This function is run when command `treesitter-context-focus-mode' is enabled.

It sets the `treesitter-context-focus-pre-overlay', `treesitter-context-focus-mid-overlay', and
`treesitter-context-focus-post-overlay' to overlays; these are invisible until
`treesitter-context-focus-move-focus' is run.  It adds `treesitter-context-focus-move-focus' to
`post-command-hook'."
  (unless (or treesitter-context-focus-pre-overlay treesitter-context-focus-post-overlay)
    (setq treesitter-context-focus-pre-overlay  (make-overlay (point-min) (point-min))
          treesitter-context-focus-mid-overlay  (make-overlay (point-min) (point-max))
          treesitter-context-focus-post-overlay (make-overlay (point-max) (point-max))
          treesitter-context-focus-buffer (current-buffer))
    (overlay-put treesitter-context-focus-mid-overlay 'face 'treesitter-context-focus-focused)
    (mapc (lambda (o) (overlay-put o 'face 'treesitter-context-focus-unfocused))
          (list treesitter-context-focus-pre-overlay treesitter-context-focus-post-overlay))
    (add-hook 'post-command-hook 'treesitter-context-focus-move-focus nil t)
    (add-hook 'change-major-mode-hook 'treesitter-context-focus-terminate nil t)))

(defun treesitter-context-focus-terminate ()
  "This function is run when command `treesitter-context-focus-mode' is disabled.

The overlays pointed to by `treesitter-context-focus-pre-overlay',
`treesitter-context-focus-mid-overlay' and `treesitter-context-focus-post-overlay' are deleted, and
`treesitter-context-focus-move-focus' is removed from `post-command-hook'."
  (when (and treesitter-context-focus-pre-overlay treesitter-context-focus-post-overlay)
    (mapc 'delete-overlay
          (list treesitter-context-focus-pre-overlay treesitter-context-focus-mid-overlay treesitter-context-focus-post-overlay))
    (remove-hook 'post-command-hook 'treesitter-context-focus-move-focus t)
    (setq treesitter-context-focus-pre-overlay nil
          treesitter-context-focus-mid-overlay nil
          treesitter-context-focus-post-overlay nil)))


;;;###autoload
(define-minor-mode treesitter-context-focus-mode
  "Dim the font color of text in surrounding sections."
  :init-value nil
  (if treesitter-context-focus-mode
      (progn
        (if (and (treesit-available-p)
                 (member major-mode treesitter-context--focus-supported-mode))
            (progn
              (treesitter-context-focus-init))
          (setq treesitter-context-focus-mode nil)
          (message "treesitter-context-focus-mode cannot be enabled.")))
    (treesitter-context-focus-terminate)))


(provide 'treesitter-context-focus)
