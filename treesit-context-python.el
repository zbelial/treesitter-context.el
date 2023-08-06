;;; treesit-context-python.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit-context-common)

(defconst treesit-context--python-node-types '("class_definition" "function_definition" "try_statement" "with_statement" "if_statement" "elif_clause" "case_clause" "while_statement" "except_clause" "match_statement")
  "Node types should be showed.")

(defconst treesit-context--python-query
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

(cl-defmethod treesit-context-collect-contexts (&context (major-mode python-ts-mode))
  "Collect all of current node's parent nodes."
  (treesit-context-collect-contexts-base treesit-context--python-node-types treesit-context--python-query python-indent-offset))

(add-to-list 'treesit-context--supported-mode 'python-ts-mode t)

(provide 'treesit-context-python)
