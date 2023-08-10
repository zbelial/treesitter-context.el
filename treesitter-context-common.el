;;; treesitter-context-common.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit)
(require 'cl-generic)
(require 'cl-lib)
(require 'seq)

(defun treesitter-context--color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA. C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the influence of C1 on the result."
  (apply #'(lambda (r g b)
             (format "#%02x%02x%02x"
                     (ash r -8)
                     (ash g -8)
                     (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun treesitter-context--parent-nodes (node-types)
  "Get the parent nodes whose node type is in NODE-TYPES."
  (unless (or (minibufferp)
              (equal (buffer-name) treesitter-context--buffer-name))
    (ignore-errors
      (let ((node (treesit-node-at (point)))
            node-type parents)
        (while node
          (setq node-type (treesit-node-type node))
          (when (member node-type node-types)
            (cl-pushnew node parents))
          (setq node (treesit-node-parent node)))
        parents))))

(defun treesitter-context--capture (node query &optional beg end node-only)
  "Capture nodes and return them as a pair.
The car of the pair is context, and the cdr is context.end."
  (let (captures
        index
        total
        result
        first
        second
        third)
    (setq captures (treesit-query-capture node query (treesit-node-start node) (point)))
    (when captures
      (setq index 0)
      (setq total (length captures))
      (while (< index total)
        (setq first (nth index captures)
              second (nth (1+ index) captures)
              third (nth (+ index 2) captures))
        (cond
         ((and (eq (car first) 'context)
               (eq (car second) 'context.real)
               (eq (car third) 'context.end))
          (cl-pushnew (list first second third) result)
          (setq index (+ index 3)))
         ((and (eq (car first) 'context)
               (eq (car second) 'context.end))
          (cl-pushnew (list first second) result)
          (setq index (+ index 2)))
         ((and (eq (car first) 'context)
               (eq (car second) 'context))
          (cl-pushnew (list first) result)
          (setq index (1+ index)))
         ((and (eq (car first) 'context)
               (eq second nil))
          (cl-pushnew (list first) result)
          (setq index (1+ index)))
         (t
          (setq index (1+ index)))))
      (setq result (nreverse result)))
    result))

(defun treesitter-context--indent-context (context level offset)
  (let ((lines (string-split context "\n" t))
        (indentation (make-string (* level offset) ?\s))
        (result ""))
    (cl-dolist (line lines)
      (setq result (concat result indentation (string-trim line) "\n")))
    (string-trim-right result)))

(defun treesitter-context-collect-contexts-base (node-types query-patterns indent-offset)
  "Collect all of current node's parent nodes with node-type in NODE-TYPES.
Use QUERY-PATTERNS to capture potential nodes.
Each node is indented according to INDENT-OFFSET."
  (let* ((node (treesit-node-at (point)))
         (parents (treesitter-context--parent-nodes node-types))
         (root (nth 0 parents))
         root-start
         groups
         node-pairs
         context
         contexts)
    (when root
      (setq treesitter-context--indent-level 0)
      (setq root-start (treesit-node-start root))
      (when (or treesitter-context-show-context-always
                (> (window-start) root-start))
        (setq groups (treesitter-context--capture root query-patterns (treesit-node-start root) (point)))
        (when groups
          (setq node-pairs (seq-filter (lambda (group) (member (cdr (nth 0 group)) parents)) groups))
          (when node-pairs
            (let (context
                  context.real
                  context.end
                  len
                  start-pos
                  end-pos)
              (save-excursion
                (widen)
                (cl-dolist (np node-pairs)
                  (setq len (length np))
                  (cond
                   ((= len 1)
                    (setq context (cdr (nth 0 np)))
                    (setq start-pos (treesit-node-start context)
                          end-pos (treesit-node-end context))
                    (cl-pushnew (treesitter-context-indent-context context (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset) contexts))
                   ((= len 2)
                    (setq context (cdr (nth 0 np))
                          context.end (cdr (nth 1 np)))
                    (setq start-pos (treesit-node-start context)
                          end-pos (treesit-node-start context.end))
                    (cl-pushnew (treesitter-context-indent-context context (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset) contexts))
                   ((= len 3)
                    (setq context (cdr (nth 0 np))
                          context.real (cdr (nth 1 np))
                          context.end (cdr (nth 2 np)))
                    (setq start-pos (treesit-node-start context.real)
                          end-pos (treesit-node-start context.end))
                    (cl-pushnew (treesitter-context-indent-context context.real (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset) contexts)))
                  ;; (cl-pushnew (treesitter-context-indent-context context (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset) contexts)
                  (setq treesitter-context--indent-level (1+ treesitter-context--indent-level)))))))))
    (nreverse contexts)))

(cl-defgeneric treesitter-context-collect-contexts ()
  "Collect all of current node's parent nodes."
  (user-error "%s is not supported by treesitter-context." major-mode))

(cl-defgeneric treesitter-context-indent-context (node context indent-level indent-offset)
  (treesitter-context--indent-context context indent-level indent-offset))

(provide 'treesitter-context-common)
