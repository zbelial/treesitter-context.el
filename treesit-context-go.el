;;; treesit-context-go.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit-context-common)

(defconst treesit-context--go-node-types '("function_declaration" "func_literal" "method_declaration" "if_statement" "for_statement" "communication_case")
  "Node types should be showed.")

(defconst treesit-context--go-query
  '(
    (function_declaration body: (_) @context.end) @context
    (func_literal body: (_) @context.end) @context
    (method_declaration body: (_) @context.end) @context
    (if_statement consequence: (_) @context.end) @context
    (for_statement body: (_) @context.end) @context
    (communication_case communication: (_) @context.end) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesit-context-collect-contexts (&context (major-mode go-ts-mode))
  "Collect all of current node's parent nodes."
  (treesit-context-collect-contexts-base treesit-context--go-node-types treesit-context--go-query go-ts-mode-indent-offset))

(add-to-list 'treesit-context--supported-mode 'go-ts-mode t)

(provide 'treesit-context-go)
