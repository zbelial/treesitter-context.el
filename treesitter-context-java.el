;;; treesitter-context-java.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defcustom treesitter-context-java-show-modifiers nil
  "If t, show modifierss of classes/methods."
  :version "29.1"
  :type 'boolean
  :group 'treesitter-context)

(defconst treesitter-context--java-node-types '("if_statement"
                                                "for_statement"
                                                "enhanced_for_statement"
                                                "while_statement"
                                                "class_declaration"
                                                "method_declaration")
  "Node types that may be showed.")

(defun treesitter-context--java-check-else-range (node)
  (let ((start-pos (treesit-node-start node))
        (end-pos (treesit-node-end node))
        (point (point)))
    (and (<= start-pos point)
         (>= end-pos point))))

(defconst treesitter-context--java-query
  (treesit-query-compile 'java '(
                                 (if_statement consequence: (_) @context.end !alternative :anchor) @context
                                 (if_statement consequence: (_) @context.end alternative: (_)) @context
                                 ((if_statement consequence: (_) "else" @context.real alternative: (_) @context.end) @context (:pred treesitter-context--java-check-else-range @context.end))
                                 (for_statement body: (_) @context.end) @context
                                 (while_statement body: (_) @context.end) @context
                                 (enhanced_for_statement body: (_) @context.end) @context
                                 (method_declaration body: (_) @context.end) @context
                                 (class_declaration body: (_) @context.end) @context))
  "Query patterns to capture desired nodes.")

(defconst treesitter-context--java-query-no-modifiers
  (treesit-query-compile 'java '(
                                 (if_statement consequence: (_) @context.end !alternative :anchor) @context
                                 (if_statement consequence: (_) @context.end alternative: (_)) @context
                                 ((if_statement consequence: (_) "else" @context.real alternative: (_) @context.end) @context (:pred treesitter-context--java-check-else-range @context.end))
                                 (for_statement body: (_) @context.end) @context
                                 (while_statement body: (_) @context.end) @context
                                 (enhanced_for_statement body: (_) @context.end) @context
                                 (method_declaration (_) type: (_) @context.real body: (_) @context.end) @context
                                 (class_declaration (_) "class" @context.real body: (_) @context.end) @context
                                 (class_declaration :anchor "class" body: (_) @context.end) @context
                                 ))
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode java-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base
   treesitter-context--java-node-types
   (if treesitter-context-java-show-modifiers
       treesitter-context--java-query
     treesitter-context--java-query-no-modifiers)
   treesitter-context-frame-indent-offset))

(cl-defmethod treesitter-context-indent-context (node context indent-level indent-offset &context (major-mode java-ts-mode))
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("else"))
        (progn
          (setq treesitter-context--indent-level (- indent-level 1))
          (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))
      (setq treesitter-context--indent-level indent-level)
      (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))))

(defconst treesitter-context--java-focus-node-types '("if_statement"
                                                      "for_statement"
                                                      "enhanced_for_statement"
                                                      "while_statement"
                                                      "class_declaration"
                                                      "method_declaration")
  "Node types that may be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode java-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--java-focus-node-types))


(defconst treesitter-context--java-fold-node-types '("if_statement"
                                                     "for_statement"
                                                     "enhanced_for_statement"
                                                     "while_statement"
                                                     "class_declaration"
                                                     "method_declaration")
  "Node types that may be folded.")

(cl-defmethod treesitter-context-fold-get-region (&context (major-mode java-ts-mode))
  "Get current code node's region."
  (let ((region (treesitter-context-fold--get-region-base treesitter-context--java-fold-node-types))
        (start)
        (end)
        (new-end)
        (node)
        (node-type)
        (target))
    (when region
      (setq start (nth 0 region)
            end (nth 1 region)
            node (nth 2 region))
      (if (= (char-before end) ?})
          (setq new-end (1- end))
        (setq new-end end))
      (setq node-type (treesit-node-type node))
      (cond
       ((or (string-equal node-type "method_declaration")
            (string-equal node-type "class_declaration"))
        (setq target (treesit-node-child-by-field-name node "body"))
        (if target
            (list (1+ (treesit-node-start target)) new-end node target)
          region))
       ((string-equal node-type "if_statement")
        (setq target (treesit-node-child-by-field-name node "consequence"))
        (if target
            (list (1+ (treesit-node-start target)) new-end node target)
          region))
       (t
        (list start new-end node))))))


;;; which-func
(defconst treesitter-context--java-which-func-node-types '("class_declaration" 
                                                           "method_declaration")
  "Node types that which-func cares about.")

(defun treesitter-context--java-which-func-name (node)
  (let ((node-type (treesit-node-type node))
        name-node)
    (cond
     ((member node-type '("class_declaration" "method_declaration"))
      (setq name-node (treesit-node-child-by-field-name node "name"))
      (when name-node
        (treesit-node-text name-node t)))
     (t
      ""))))

(cl-defmethod treesitter-context-which-func-function (&context (major-mode java-ts-mode))
  (treesitter-context--which-func-function-base treesitter-context--java-which-func-node-types #'treesitter-context--java-which-func-name))

;;; supported mode
(add-to-list 'treesitter-context--supported-mode 'java-ts-mode t)
(add-to-list 'treesitter-context--fold-supported-mode 'java-ts-mode t)
(add-to-list 'treesitter-context--focus-supported-mode 'java-ts-mode t)
(add-to-list 'treesitter-context--which-func-supported-mode 'java-ts-mode t)

(provide 'treesitter-context-java)
