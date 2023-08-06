;;; treesit-context.el --- Show context information around current point -*- lexical-binding: t; -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (posframe "1.4.2"))
;; Homepage: https://bitbucket.org/zbelial/treesit-context
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

(defgroup treesit-context nil
  "Show context information around current point."
  :group 'treesit
  :version "28.2")

(defcustom treesit-context-idle-time 2.0
  "How many seconds to wait before refreshing context information."
  :version "29.1"
  :type 'float
  :safe 'floatp
  :group 'treesit-context)

(defcustom treesit-context-show-context-always nil
  "If t, show context all the time.
If nil, show context only when the outmost parent is invisible."
  :version "29.1"
  :type 'boolean
  :group 'treesit-context)

(defcustom treesit-context-hide-frame-after-move t
  "If t, hide the child-frame after moving cursor."
  :version "29.1"
  :type 'boolean
  :group 'treesit-context)

(defcustom treesit-context-frame-autohide-timeout 15
  "Child frame will hide itself after this seconds."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesit-context)

(defcustom treesit-context-frame-min-width 60
  "Minimal width of the child frame."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'treesit-context)

(defvar treesit-context--supported-mode nil
  "Major modes that are support by treesit-context.")

(defvar treesit-context--buffer-name "*treesit-context*"
  "Name of buffer used to show context.")

(defvar-local treesit-context--refresh-timer nil
  "Idle timer for refreshing context.")

(defvar-local treesit-context--context-list nil
  "A list storing context. The outmost one is at the front.")

(defun treesit-context--show-context ()
  ""
  (when treesit-context--context-list
    (let* ((buffer (get-buffer-create treesit-context--buffer-name))
           (contexts treesit-context--context-list)
           (bg-mode (frame-parameter nil 'background-mode))
           (background-color
            (cond ((eq bg-mode 'dark)
                   (treesit-context--color-blend (face-background 'default) "#000000" 0.1))
                  ((eq bg-mode 'light)
                   (treesit-context--color-blend (face-background 'default) "#000000" 0.8)))))
      (with-current-buffer buffer
        (erase-buffer)
        (goto-char (point-min))
        (cl-dolist (text contexts)
          (insert text "\n")))
      (posframe-plus-show buffer t treesit-context-hide-frame-after-move
                          :poshandler #'posframe-poshandler-window-top-right-corner
                          :border-width 1
                          :background-color background-color
                          :internal-border-color "orange"
                          :internal-border-width 1
                          :min-width treesit-context-frame-min-width
                          :accept-focus nil
                          :timeout treesit-context-frame-autohide-timeout)))
  nil)

(defun treesit-context--refresh-context ()
  "Refresh context at the point."
  (unless (or (minibufferp)
              (equal (buffer-name) treesit-context--buffer-name))
    (setq treesit-context--context-list nil)
    (ignore-errors
      (setq treesit-context--context-list (treesit-context-collect-contexts))
      (treesit-context--show-context))))

(defun treesit-context--refresh-when-idle ()
  (when treesit-context--refresh-timer
    (cancel-timer treesit-context--refresh-timer)
    (setq treesit-context--refresh-timer nil))
  (setq treesit-context--refresh-timer (run-with-idle-timer treesit-context-idle-time nil #'treesit-context--refresh-context)))

(defun treesit-context--hide-frame ()
  (posframe-hide (get-buffer treesit-context--buffer-name)))

(define-minor-mode treesit-context-mode
  "Show context information in treesit-based mode."
  :init-value nil
  :lighter " TC"
  (if treesit-context-mode
      (progn
        (if (and (treesit-available-p)
                 (posframe-workable-p)
                 (member major-mode treesit-context--supported-mode))
            (progn
              (add-hook 'post-command-hook #'treesit-context--refresh-when-idle nil t)
              (add-hook 'buffer-list-update-hook #'treesit-context--hide-frame nil t)
              )
          (setq treesit-context-mode nil)))
    (when treesit-context--refresh-timer
      (cancel-timer treesit-context--refresh-timer)
      (setq treesit-context--refresh-timer nil))
    (treesit-context--hide-frame)
    (remove-hook 'post-command-hook #'treesit-context--refresh-when-idle t)
    (remove-hook 'buffer-list-update-hook #'treesit-context--hide-frame t)))

(require 'treesit-context-java)
(require 'treesit-context-python)
(require 'treesit-context-c)
(require 'treesit-context-cpp)
(require 'treesit-context-rust)
(require 'treesit-context-go)

(provide 'treesit-context)
