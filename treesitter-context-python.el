;;; treesitter-context-python.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--python-node-types '("class_definition" "function_definition" "try_statement" "with_statement" "if_statement" "elif_clause" "case_clause" "while_statement" "except_clause" "match_statement")
  "Node types should be showed.")

(defconst treesitter-context--python-query
  '(
    (class_definition body: (_) @context.end) @context
    (function_definition body: (_) @context.end) @context
    (try_statement body: (_) @context.end) @context
    (with_statement body: (_) @context.end) @context
    (if_statement consequence: (_) @context.end) @context
    (elif_clause consequence: (_) @context.end) @context
    (case_clause consequence: (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (except_clause (block) @context.end) @context
    (match_statement body: (_) @context.end) @context)
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode python-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--python-node-types treesitter-context--python-query python-indent-offset))

(add-to-list 'treesitter-context--supported-mode 'python-ts-mode t)

(provide 'treesitter-context-python)
