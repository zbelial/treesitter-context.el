;;; treesitter-context-utils.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context)
(eval-when-compile
  (require 'cl-macs))

(cl-defun treesitter-context-toggle-show ()
  "Show or hide context frame."
  (interactive)
  (when treesitter-context-mode
    (message "treesitter context mode is enabled already.")
    (cl-return-from treesitter-context-toggle-show nil))
  (when (not (and (treesit-available-p)
                  (posframe-workable-p)
                  (member major-mode treesitter-context--supported-mode)))
    (message "treesitter context mode cannot be enabled in current buffer.")
    (cl-return-from treesitter-context-toggle-show nil))
  (cond
   ((null treesitter-context--child-frame)
    (treesitter-context--refresh-context)
    (treesitter-context--show-context))
   ((and treesitter-context--child-frame
         (frame-visible-p treesitter-context--child-frame))
    (treesitter-context--hide-frame))
   ((and treesitter-context--child-frame
         (not (frame-visible-p treesitter-context--child-frame)))
    (treesitter-context--refresh-context)
    (treesitter-context--show-context))
   (t
    ;;nothing
    )))

  (provide 'treesitter-context-utils)
