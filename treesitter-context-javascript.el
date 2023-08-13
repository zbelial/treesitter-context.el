;;; treesitter-context-javascript.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--javascript-node-types '("if_statement" "else_clause" "for_statement" "for_in_statement" "while_statement" "class_declaration" "class" "function" "arrow_function" "function_declaration" "generator_function_declaration" "method_definition" "switch_statement" "switch_case" "switch_default" "pair" "variable_declarator")
  "Node types should be showed.")

(defconst treesitter-context--javascript-query
  '((if_statement consequence: (_) @context.end) @context
    (else_clause :anchor (_) @context.end) @context
    (for_statement body: (_) @context.end) @context
    (for_in_statement body: (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (class_declaration body: (_) @context.end) @context
    (class body: (_) @context.end) @context
    (function body: (_) @context.end) @context
    (arrow_function body: (_) @context.end) @context
    (function_declaration body: (_) @context.end) @context
    (generator_function_declaration body: (_) @context.end) @context
    (method_definition body: (_) @context.end) @context
    (switch_statement body: (_) @context.end) @context
    (switch_case body: (_) @context.end) @context
    (switch_default body: (_) @context.end) @context
    (variable_declarator name: (_) :anchor (_):? @context.end) @context
    (pair value: (_) @context.end) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode js-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--javascript-node-types treesitter-context--javascript-query treesitter-context-frame-indent-offset))

(cl-defmethod treesitter-context-indent-context (node context indent-level indent-offset &context (major-mode js-ts-mode))
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("else_clause"))
        (progn
          (setq treesitter-context--indent-level (- indent-level 1))
          (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))
      (setq treesitter-context--indent-level indent-level)
      (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))))

(add-to-list 'treesitter-context--supported-mode 'js-ts-mode t)

(provide 'treesitter-context-javascript)
