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
  "Node types should be showed.")

(defun treesitter-context--java-check-else-range (node)
  (let ((start-pos (treesit-node-start node))
        (end-pos (treesit-node-end node))
        (point (point)))
    (and (<= start-pos point)
         (>= end-pos point))))

(defconst treesitter-context--java-query
  '(
    (if_statement consequence: (_) @context.end !alternative :anchor) @context
    (if_statement consequence: (_) @context.end alternative: (_)) @context
    ((if_statement consequence: (_) "else" @context.real alternative: (_) @context.end) @context (:pred treesitter-context--java-check-else-range @context.end))
    (for_statement body: (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (enhanced_for_statement body: (_) @context.end) @context
    (method_declaration body: (_) @context.end) @context
    (class_declaration body: (_) @context.end) @context)
  "Query patterns to capture desired nodes.")

(defconst treesitter-context--java-query-no-modifiers
  '(
    (if_statement consequence: (_) @context.end !alternative :anchor) @context
    (if_statement consequence: (_) @context.end alternative: (_)) @context
    ((if_statement consequence: (_) "else" @context.real alternative: (_) @context.end) @context (:pred treesitter-context--java-check-else-range @context.end))
    (for_statement body: (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (enhanced_for_statement body: (_) @context.end) @context
    (method_declaration (_) type: (_) @context.real body: (_) @context.end) @context
    (class_declaration (_) "class" @context.real body: (_) @context.end) @context
    (class_declaration :anchor "class" body: (_) @context.end) @context
    )
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
  "Node types should be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode java-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--java-focus-node-types))

(add-to-list 'treesitter-context--supported-mode 'java-ts-mode t)

(provide 'treesitter-context-java)
