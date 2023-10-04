;;; treesitter-context-c.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--c++-node-types '("preproc_if" "preproc_ifdef" "preproc_else" "function_definition" "for_statement" "if_statement" "else_clause" "while_statement" "do_statement" "struct_specifier" "enum_specifier" "for_range_loop" "class_specifier" "linkage_specification" "switch_statement" "case_statement")
  "Node types should be showed.")

(defconst treesitter-context--c++-query
  '(
    (preproc_if (_) (_) @context.end) @context
    (preproc_ifdef name: (identifier) :anchor (_) @context.end) @context
    (preproc_else (_) @context.end) @context
    (function_definition body: (_) @context.end) @context
    (for_statement (compound_statement) @context.end) @context
    (if_statement consequence: (_) @context.end) @context
    (else_clause (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (do_statement body: (_) @context.end) @context
    (switch_statement body: (_) @context.end) @context
    (case_statement value: (_) :anchor) @context
    (case_statement value: (_) :anchor (_) @context.end) @context
    (case_statement "default" :anchor (_) @context.end) @context
    (struct_specifier body: (_) @context.end) @context
    (enum_specifier body: (_) @context.end) @context    
    (for_range_loop body: (_) @context.end) @context
    (class_specifier body: (_) @context.end) @context
    (linkage_specification body: (declaration_list (_) @context.end)) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode c++-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--c++-node-types treesitter-context--c++-query treesitter-context-frame-indent-offset))

(cl-defmethod treesitter-context-indent-context (node context indent-level indent-offset &context (major-mode c++-ts-mode))
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("elif_clause" "else_clause"))
        (progn
          (setq treesitter-context--indent-level (- indent-level 1))
          (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))
      (setq treesitter-context--indent-level indent-level)
      (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))))

(defconst treesitter-context--c++-focus-node-types '("preproc_if" "preproc_ifdef" "preproc_else" "function_definition" "for_statement" "if_statement" "else_clause" "while_statement" "do_statement" "struct_specifier" "enum_specifier" "for_range_loop" "class_specifier" "linkage_specification" "switch_statement" "case_statement")
  "Node types should be showed.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode c++-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--c++-focus-node-types))

(add-to-list 'treesitter-context--supported-mode 'c++-ts-mode t)

(provide 'treesitter-context-cpp)
