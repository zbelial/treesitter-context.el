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

(defface treesitter-context-fold-ellipsis-face
  '()
  "Face for folded blocks"
  :group 'treesitter-context)

(defcustom treesitter-context-fold-ellipsis-content "..."
  "Text to show in place of a folded block."
  :tag "Ellipsis"
  :type 'string
  :group 'treesitter-context)

(defcustom treesitter-context-fold-show-fringe-marks t
  "Show fold markers in the fringe?"
  :tag "Show fringe marks?"
  :type 'boolean
  :group 'treesitter-context)

(defcustom treesitter-context-fold-unfold-when-fold-region nil
  "When fold a region, whether unfold old foldings in this region or not."
  :tag "Unfold old fold?"
  :type 'boolean
  :group 'treesitter-context)

(defface treesitter-context-fold-fringe-marks-face
  '()
  "Face for fringe marks."
  :group 'treesitter-context)

(defun treesitter-context-fold-ellipsis ()
  "Return propertized ellipsis content."
  (concat " "
          (propertize treesitter-context-fold-ellipsis-content 'face 'treesitter-context-fold-ellipsis-face)
          " "))

(defun treesitter-context-fold--get-overlays (beg end)
  "Get all overlays between BEG and END."
  (delq nil
        (mapcar (lambda (overlay)
                  (and (member "treesitter-context-fold" (overlay-properties overlay))
                       overlay))
                (overlays-in beg end))))

(defun treesitter-context-fold--get-exact-overlays (beg end)
  "Get overlays that starts at BEG and ends at END."
  (let ((overlays (treesitter-context-fold--get-overlays beg end))
        (result))
    (dolist (ov overlays)
      (when (and (= (overlay-start ov) beg)
                 (= (overlay-end ov) end))
        (push ov result)))
    result))

(defun treesitter-context-fold--show-region (beg end)
  "Delete all folding overlays between BEG and END."
  (mapcar 'delete-overlay (treesitter-context-fold--get-overlays beg end)))

(defun treesitter-context-fold--hide-region (beg end)
  "Fold region between BEG and END."
  (when (> end beg)
    (when treesitter-context-fold-unfold-when-fold-region
      (treesitter-context-fold--show-region beg end))
    (let ((before-string
           (concat
            (when treesitter-context-fold-show-fringe-marks
              (propertize " " 'display '(left-fringe right-triangle treesitter-context-fold-fringe-marks-face)))
            (treesitter-context-fold-ellipsis)))
          (new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'intangible t)
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'modification-hooks
                   (list (lambda (overlay &optional a b c d)
                           (delete-overlay overlay))))
      (overlay-put new-overlay 'before-string before-string)
      (overlay-put new-overlay 'category "treesitter-context-fold"))))

;;;###autoload
(defun treesitter-context-fold-hide ()
  "Fold current code node."
  (interactive)
  (treesitter-context--when-fold-available
   (when-let ((region (treesitter-context-fold-get-region)))
     (let ((beg (nth 0 region))
           (end (nth 1 region)))
       (let ((overlays (treesitter-context-fold--get-exact-overlays beg end)))
         ;; If there are overlays with the same region, don't try to fold again.
         (unless overlays
           (setq overlays (treesitter-context-fold--hide-region beg end)))
         overlays)))))

;;;###autoload
(defun treesitter-context-fold-show ()
  "Unfold current code node."
  (interactive)
  (treesitter-context--when-fold-available
   (treesitter-context-fold--show-region (line-beginning-position)
                                         (1+ (line-end-position)))))

;;;###autoload
(defun treesitter-context-fold-toggle ()
  "Toggle folding state of current code node."
  (interactive)
  (if (treesitter-context-fold--get-overlays (line-beginning-position)
                                             (1+ (line-end-position)))
      (treesitter-context-fold-show)
    (treesitter-context-fold-hide)))

;;;###autoload
(defun treesitter-context-fold-debug ()
  "Show debug info."
  (interactive)
  (treesitter-context--when-fold-available
   (let ((region (treesitter-context-fold-get-region)))
     (if region
         (message "region: %s" region)
       ;; (progn
       ;;   (message "start: %s" (nth 0 region))
       ;;   (message "end:   %s" (nth 1 region))
       ;;   (message "node:  %s" (nth 2 region)))
       (message "No valid region here.")))))

;;;###autoload
(defvar treesitter-context-fold-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;###autoload
(define-minor-mode treesitter-context-fold-mode
  "Fold code according to treesit syntax tree."
  :global nil
  :init-value nil
  :keymap treesitter-context-fold-mode-map
  (if treesitter-context-fold-mode
      (unless (and (treesit-available-p) (member major-mode treesitter-context--fold-supported-mode))
        (message "Treesitter context fold mode cannot be enabled.")
        (setq treesitter-context-fold-mode nil))))


;; Register code folding functions for better `evil-mode' integration
(defvar evil-fold-list) ;; Make the byte-compiler happy
(with-eval-after-load 'evil
  (add-to-list 'evil-fold-list
               `((treesitter-context-fold-mode)
                 :open       treesitter-context-fold-show
                 :open-all   nil
                 :close      treesitter-context-fold-hide
                 :close-all  nil
                 :toggle     treesitter-context-fold-toggle
                 :delete     nil
                 :open-rec   nil)))

(provide 'treesitter-context-fold)
