;;; treesitter-context-utils.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context)
(eval-when-compile
  (require 'cl-macs))

;;;###autoload
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

;;;###autoload
(defmacro treesitter-context--when-fold-available (&rest body)
  "Run BODY only if treesit is available and current major mode is supported."
  (declare (indent 0))
  `(if (and (treesit-available-p)
            (member major-mode treesitter-context--fold-supported-mode))
       (progn ,@body)
     (message "Current major mode is not supported by treesitter-context-fold.")
     nil))

;;;###autoload
(defmacro treesitter-context--when-available (&rest body)
  "Run BODY only if treesit is available and current major mode is supported."
  (declare (indent 0))
  `(if (and (treesit-available-p)
            (member major-mode treesitter-context--fold-supported-mode))
       (progn ,@body)
     (message "Current major mode is not supported by treesitter-context.")
     nil))

;;;###autoload
(defmacro treesitter-context--when-available-quiet (&rest body)
  "Run BODY only if treesit is available and current major mode is supported."
  (declare (indent 0))
  `(when (and (treesit-available-p)
              (member major-mode treesitter-context--supported-mode))
     (progn ,@body)))

(provide 'treesitter-context-utils)
