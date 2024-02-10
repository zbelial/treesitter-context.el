;;; treesitter-context-go.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--go-node-types '("function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "communication_case" "expression_switch_statement" "expression_case" "type_switch_statement" "type_case" "default_case")
  "Node types that may be showed.")

(defun treesitter-context--go-check-else-range (node)
  (let ((start-pos (treesit-node-start node))
        (end-pos (treesit-node-end node))
        (point (point)))
    (and (<= start-pos point)
         (>= end-pos point))))

(defconst treesitter-context--go-query
  (treesit-query-compile 'go '(
                               (function_declaration body: (_) @context.end) @context
                               (func_literal body: (_) @context.end) @context
                               (method_declaration body: (_) @context.end) @context
                               (if_statement consequence: (_) @context.end !alternative :anchor) @context
                               (if_statement consequence: (_) @context.end alternative: (_)) @context
                               ((if_statement consequence: (_) "else" @context.real alternative: (_) @context.end) @context (:pred treesitter-context--go-check-else-range @context.end))
                               (for_statement body: (_) @context.end) @context
                               (expression_switch_statement value: (_) :anchor (_) @context.end) @context
                               (expression_case value: (_) :anchor) @context
                               (expression_case value: (_) :anchor (_) @context.end) @context
                               (type_switch_statement value: (_) :anchor (_) @context.end) @context
                               (type_case type: (_) :anchor) @context
                               (type_case type: (_) :anchor (_) @context.end) @context
                               (default_case :anchor (_) @context.end) @context
                               (communication_case communication: (_) @context.end) @context))
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode go-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--go-node-types treesitter-context--go-query treesitter-context-frame-indent-offset))

(cl-defmethod treesitter-context-indent-context (node context indent-level indent-offset &context (major-mode go-ts-mode))
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("else"))
        (progn
          (setq treesitter-context--indent-level (- indent-level 1))
          (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))
      (setq treesitter-context--indent-level indent-level)
      (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))))

(defconst treesitter-context--go-focus-node-types '("function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "communication_case" "expression_switch_statement" "expression_case" "type_switch_statement" "type_case" "default_case")
  "Node types that may be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode go-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--go-focus-node-types))

(defconst treesitter-context--go-fold-node-types '("function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "communication_case" "expression_switch_statement" "expression_case" "type_switch_statement" "type_case" "default_case")
  "Node types that may be folded.")

(cl-defmethod treesitter-context-fold-get-region (&context (major-mode go-ts-mode))
  "Get current code node's region."
  (treesitter-context-fold--get-region-base treesitter-context--go-fold-node-types))

(add-to-list 'treesitter-context--supported-mode 'go-ts-mode t)

(provide 'treesitter-context-go)
