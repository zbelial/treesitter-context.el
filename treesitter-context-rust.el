;;; treesitter-context-rust.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--rust-node-types '("if_expression" "else_clause" "match_expression" "match_arm" "for_expression" "while_expression" "loop_expression" "closure_expression" "function_item" "impl_item" "trait_item" "struct_item" "enum_item" "mod_item")
  "Node types that may be showed.")

(defconst treesitter-context--rust-query
  '(
    (if_expression consequence: (_) @context.end) @context
    (else_clause (block (_)) @context.end) @context
    (match_expression body: (_) @context.end) @context
    (match_arm pattern: (_) :anchor (_) @context.end) @context
    (for_expression body: (_) @context.end) @context
    (while_expression body: (_) @context.end) @context
    (loop_expression body: (_) @context.end) @context
    (closure_expression body: (_) @context.end) @context
    (function_item body: (_) @context.end) @context
    (impl_item body: (_) @context.end) @context
    (trait_item body: (_) @context.end) @context
    (struct_item body: (_) @context.end) @context
    (enum_item body: (_) @context.end) @context
    (mod_item body: (_) @context.end) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode rust-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--rust-node-types treesitter-context--rust-query treesitter-context-frame-indent-offset))

(cl-defmethod treesitter-context-indent-context (node context indent-level indent-offset &context (major-mode rust-ts-mode))
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("else_clause"))
        (progn
          (setq treesitter-context--indent-level (- indent-level 1))
          (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))
      (setq treesitter-context--indent-level indent-level)
      (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))))

(defconst treesitter-context--rust-focus-node-types '("if_expression" "else_clause" "match_expression" "match_arm" "for_expression" "while_expression" "loop_expression" "closure_expression" "function_item" "impl_item" "trait_item" "struct_item" "enum_item" "mod_item")
  "Node types that may be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode rust-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--rust-focus-node-types))

(defconst treesitter-context--rust-fold-node-types '("if_expression" "else_clause" "match_expression" "match_arm" "for_expression" "while_expression" "loop_expression" "closure_expression" "function_item" "impl_item" "trait_item" "struct_item" "enum_item" "mod_item")
  "Node types that may be folded.")

(cl-defmethod treesitter-context-fold-get-region (&context (major-mode rust-ts-mode))
  "Get current code node's region."
  (treesitter-context-fold--get-region-base treesitter-context--rust-fold-node-types))

(add-to-list 'treesitter-context--supported-mode 'rust-ts-mode t)

(provide 'treesitter-context-rust)
