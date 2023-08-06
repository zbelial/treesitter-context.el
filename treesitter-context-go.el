;;; treesitter-context-go.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--go-node-types '("function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "communication_case")
  "Node types should be showed.")

(defconst treesitter-context--go-query
  '(
    (function_declaration body: (_) @context.end) @context
    (func_literal body: (_) @context.end) @context
    (method_declaration body: (_) @context.end) @context
    (if_statement consequence: (_) @context.end) @context
    (for_statement body: (_) @context.end) @context
    (communication_case communication: (_) @context.end) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode go-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--go-node-types treesitter-context--go-query go-ts-mode-indent-offset))

(add-to-list 'treesitter-context--supported-mode 'go-ts-mode t)

(provide 'treesitter-context-go)
