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




;;; Require 
(require 'cl-lib)
(require 'treesit)
(require 'posframe-plus)

(defgroup treesitter-context nil
  "Show context information around current point."
  :group 'treesit
  :version "28.2")

(defcustom treesitter-context-idle-time 2.0
  "How many seconds to wait before refreshing context information."
  :version "29.1"
  :type 'float
  :safe 'floatp
  :group 'treesitter-context)

(defcustom treesitter-context-show-context-always nil
  "If t, show context all the time.
If nil, show context only when the outmost parent is invisible."
  :version "29.1"
  :type 'boolean
  :group 'treesitter-context)

(defcustom treesitter-context-hide-frame-after-move t
  "If t, hide the child-frame after moving cursor."
  :version "29.1"
  :type 'boolean
  :group 'treesitter-context)

(defcustom treesitter-context-frame-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defcustom treesitter-context-frame-min-width 60
  "Minimal width of the child frame."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defcustom treesitter-context-frame-min-height 5
  "Minimal height of the child frame."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesitter-context)

(defvar treesitter-context--supported-mode nil
  "Major modes that are support by treesitter-context.")

(defvar treesitter-context--buffer-name "*treesitter-context*"
  "Name of buffer used to show context.")

(defvar treesitter-context--indent-level 0
  "Indent level used to generate context information.")

(defvar-local treesitter-context--refresh-timer nil
  "Idle timer for refreshing context.")

(defvar-local treesitter-context--context-list nil
  "A list storing context. The outmost one is at the front.")

(defun treesitter-context--show-context ()
  "Show context in a child frame."
  (let* ((buffer (get-buffer-create treesitter-context--buffer-name))
         (contexts treesitter-context--context-list)
         (bg-mode (frame-parameter nil 'background-mode))
         (background-color
          (cond ((eq bg-mode 'dark)
                 (treesitter-context--color-blend (face-background 'default) "#000000" 0.1))
                ((eq bg-mode 'light)
                 (treesitter-context--color-blend (face-background 'default) "#000000" 0.8)))))
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (cl-dolist (text contexts)
        (insert text "\n")))
    (posframe-plus-show buffer t treesitter-context-hide-frame-after-move
                        :poshandler #'posframe-poshandler-window-top-right-corner
                        :border-width 1
                        :background-color background-color
                        :internal-border-color "orange"
                        :internal-border-width 1
                        :min-width (min (max treesitter-context-frame-min-width (/ (window-width) 2)) (window-width))
                        :min-height treesitter-context-frame-min-height
                        :accept-focus nil
                        :timeout treesitter-context-frame-autohide-timeout))
  nil)

(defun treesitter-context--refresh-context ()
  "Refresh context at the point."
  (unless (or (minibufferp)
              (equal (buffer-name) treesitter-context--buffer-name))
    (setq treesitter-context--context-list nil)
    (ignore-errors
      (setq treesitter-context--context-list (treesitter-context-collect-contexts))
      (treesitter-context--show-context))))

(defun treesitter-context--refresh-when-idle ()
  (when treesitter-context--refresh-timer
    (cancel-timer treesitter-context--refresh-timer)
    (setq treesitter-context--refresh-timer nil))
  (setq treesitter-context--refresh-timer (run-with-idle-timer treesitter-context-idle-time nil #'treesitter-context--refresh-context)))

(defun treesitter-context--hide-frame ()
  (posframe-hide (get-buffer treesitter-context--buffer-name)))

(define-minor-mode treesitter-context-mode
  "Show context information in treesit-based mode."
  :init-value nil
  :lighter " TC"
  (if treesitter-context-mode
      (progn
        (if (and (treesit-available-p)
                 (posframe-workable-p)
                 (member major-mode treesitter-context--supported-mode))
            (progn
              (add-hook 'post-command-hook #'treesitter-context--refresh-when-idle nil t)
              (add-hook 'buffer-list-update-hook #'treesitter-context--hide-frame nil t)
              (treesitter-context--refresh-context))
          (setq treesitter-context-mode nil)))
    (when treesitter-context--refresh-timer
      (cancel-timer treesitter-context--refresh-timer)
      (setq treesitter-context--refresh-timer nil))
    (treesitter-context--hide-frame)
    (remove-hook 'post-command-hook #'treesitter-context--refresh-when-idle t)
    (remove-hook 'buffer-list-update-hook #'treesitter-context--hide-frame t)))

(require 'treesitter-context-java)
(require 'treesitter-context-python)
(require 'treesitter-context-c)
(require 'treesitter-context-cpp)
(require 'treesitter-context-rust)
(require 'treesitter-context-go)
(require 'treesitter-context-json)
(require 'treesitter-context-javascript)

(provide 'treesitter-context)
