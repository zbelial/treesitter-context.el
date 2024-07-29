;;; treesitter-context-common.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit)
(require 'cl-generic)
(require 'cl-lib)
(require 'seq)

(defvar treesitter-context--supported-mode nil
  "Major modes that are support by `treesitter-context-mode'.")

(defvar treesitter-context--fold-supported-mode nil
  "Major modes that are support by `treesitter-context-fold-mode'.")

(defvar treesitter-context--focus-supported-mode nil
  "Major modes that are support by `treesitter-context-focus-mode'.")

(defvar treesitter-context--which-func-supported-mode nil
  "Major modes that are support by treesitter-context which-func.")

;;; general
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

;;; context
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
    (setq captures (treesit-query-capture node query (or beg (treesit-node-start node)) (or end (1+ (point)))))
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
        result)
    (cl-dolist (line lines)
      (when (length> (string-trim line) 0)
        (cl-pushnew (concat indentation line "\n") result)))
    (nreverse result)))

;; not used yet
(defun treesitter-context--cut-context (beg end)
  (let ((beg-line-no (line-number-at-pos beg))
        (end-line-no (line-number-at-pos end))
        (first-indent 0)
        beg-column
        beg-column0-pos
        lines)
    (if (= beg-line-no end-line-no)
        (buffer-substring beg end)
      (save-excursion
        (save-restriction
          (goto-char beg)
          (setq beg-column0-pos (line-beginning-position))
          (setq beg-column (- beg beg-column0-pos))
          (push (buffer-substring beg (line-end-position)) lines)
          (forward-line)
          (while (< (line-number-at-pos) end-line-no)
            (if (> (current-indentation) beg-column)
                (push (buffer-substring (+ (line-beginning-position) beg-column) (line-end-position)) lines)
              (push (buffer-substring (line-beginning-position) (line-end-position)) lines))
            (forward-line))
          (if (> (current-indentation) beg-column)
              (push (buffer-substring (+ (line-beginning-position) beg-column) end) lines)
            (push (buffer-substring (line-beginning-position) end) lines))
          (mapconcat #'identity (nreverse lines) "\n"))))))

(defun treesitter-context-collect-contexts-base (node-types query-patterns indent-offset)
  "Collect all of current node's parent nodes with node-type in NODE-TYPES.
Use QUERY-PATTERNS to capture potential nodes.
Each node is indented according to INDENT-OFFSET."
  (let* ((parents (treesitter-context--parent-nodes node-types))
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
        (setq groups (treesitter-context--capture root query-patterns (treesit-node-start root) (1+ (point))))
        (when groups
          (setq node-pairs (seq-filter (lambda (group) (member (cdr (nth 0 group)) parents)) groups))
          (when node-pairs
            (let (context
                  context.real
                  context.end
                  len
                  start-pos
                  end-pos
                  line-no)
              (save-excursion
                (save-restriction
                  (widen)
                  (cl-dolist (np node-pairs)
                    (setq len (length np))
                    (cond
                     ((= len 1)
                      (setq context (cdr (nth 0 np)))
                      (setq start-pos (treesit-node-start context)
                            end-pos (treesit-node-end context)
                            line-no (line-number-at-pos start-pos))
                      (cl-pushnew (cons line-no (treesitter-context-indent-context context (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset)) contexts))
                     ((= len 2)
                      (setq context (cdr (nth 0 np))
                            context.end (cdr (nth 1 np)))
                      (setq start-pos (treesit-node-start context)
                            end-pos (treesit-node-start context.end)
                            line-no (line-number-at-pos start-pos))
                      (cl-pushnew (cons line-no (treesitter-context-indent-context context (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset)) contexts))
                     ((= len 3)
                      (setq context (cdr (nth 0 np))
                            context.real (cdr (nth 1 np))
                            context.end (cdr (nth 2 np)))
                      (setq start-pos (treesit-node-start context.real)
                            end-pos (treesit-node-start context.end)
                            line-no (line-number-at-pos start-pos))
                      (cl-pushnew (cons line-no (treesitter-context-indent-context context.real (buffer-substring start-pos end-pos) treesitter-context--indent-level indent-offset)) contexts)))
                    (setq treesitter-context--indent-level (1+ treesitter-context--indent-level))))))))))
    (nreverse contexts)))

(cl-defgeneric treesitter-context-collect-contexts ()
  "Collect all of current node's parent nodes."
  (user-error "%s is not supported by treesitter-context." major-mode))

(cl-defgeneric treesitter-context-indent-context (node context indent-level indent-offset)
  (treesitter-context--indent-context context indent-level indent-offset))


;;; focus
(defun treesitter-context--focus-bounds (node-types)
  (let ((node (treesit-node-at (point)))
        result
        (begin (point-min))
        (end (point-max))
        stop)
    (while (and node
                (not stop))
      (if (member (treesit-node-type node) node-types)
          (progn
            (setq stop t)
            (setq begin (treesit-node-start node)
                  end (treesit-node-end node)))
        (setq node (treesit-node-parent node))))
    (if node
        (list node begin end)
      (list (treesit-buffer-root-node) begin end))))

(cl-defgeneric treesitter-context-focus-bounds ()
  "Return the bound that should be focused."
  (user-error "%s is not supported by treesitter-context-focus." major-mode))

;;; fold
(defun treesitter-context-fold--get-region-base (node-types)
  "Get current code node's region."
  (let ((node (treesit-node-at (point)))
        (node-type)
        (start-pos)
        (end-pos)
        (current-line (line-number-at-pos nil t)))
    (when node
      (setq node-type (treesit-node-type node))
      (if (and (member node-type node-types)
               (>= current-line (line-number-at-pos (treesit-node-start node) t)))
          (progn
            (setq start-pos (treesit-node-start node)
                  end-pos (treesit-node-end node)))
        (setq node (treesit-parent-until node
                                         (lambda (n)
                                           (and (member (treesit-node-type n) node-types)
                                                (>= current-line (line-number-at-pos (treesit-node-start n) t))))))
        (when node
          (setq start-pos (treesit-node-start node)
                end-pos (treesit-node-end node)))))
    (if (and start-pos end-pos)
        (progn
          (save-excursion
            (goto-char start-pos)
            (setq start-pos (line-end-position)))
          (list start-pos end-pos node))
      (message "No code region to fold.")
      nil)))

(cl-defgeneric treesitter-context-fold-get-region ()
  "Get current code node's region."
  (user-error "%s is not supported by treesitter-context-fold." major-mode))

(provide 'treesitter-context-common)
