;;; treesitter-context-c.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--c++-node-types '("preproc_if" "preproc_ifdef" "preproc_else" "function_definition" "for_statement" "if_statement" "else_clause" "while_statement" "do_statement" "struct_specifier" "enum_specifier" "for_range_loop" "class_specifier" "namespace_definition" "linkage_specification" "switch_statement" "case_statement")
  "Node types that may be showed.")

(defconst treesitter-context--c++-query
  (treesit-query-compile 'cpp '(
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
                                (namespace_definition body: (_) @context.end) @context
                                (class_specifier body: (_) @context.end) @context
                                (linkage_specification body: (declaration_list (_) @context.end)) @context
                                ))
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

;;; focus
(defconst treesitter-context--c++-focus-node-types '("preproc_if" "preproc_ifdef" "preproc_else" "function_definition" "for_statement" "if_statement" "else_clause" "while_statement" "do_statement" "struct_specifier" "enum_specifier" "for_range_loop" "class_specifier" "linkage_specification" "switch_statement" "case_statement")
  "Node types that may be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode c++-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--c++-focus-node-types))

;;; fold
(defconst treesitter-context--c++-fold-node-types '("preproc_if" "preproc_ifdef" "preproc_else" "function_definition" "for_statement" "if_statement" "else_clause" "while_statement" "do_statement" "struct_specifier" "for_range_loop" "class_specifier" "linkage_specification" "switch_statement" "case_statement")
  "Node types that may be folded.")

(cl-defmethod treesitter-context-fold-get-region (&context (major-mode c++-ts-mode))
  "Get current code node's region."
  (let ((region (treesitter-context-fold--get-region-base treesitter-context--c++-fold-node-types))
        (start)
        (end)
        (node)
        (node-type)
        (target))
    (when region
      (setq start (nth 0 region)
            end (nth 1 region)
            node (nth 2 region))
      (setq node-type (treesit-node-type node))
      (cond
       (t
        (if (= (char-before end) ?})
            (list start (1- end) node)
          (list start end node)))))))

;;; which-func
(defconst treesitter-context--c++-which-func-node-types '("namespace_definition" "function_definition" "struct_specifier" "class_specifier")
  "Node types that which-func cares about.")

(defun treesitter-context--c++-which-func-name (node)
  (let ((node-type (treesit-node-type node)))
    (cond
     ((member node-type '("namespace_definition"))
      (when-let ((name-node (treesit-node-child-by-field-name node "name")))
        (treesit-node-text name-node t)))
     ((member node-type '("function_definition"))
      (when-let ((function-declarator (treesit-search-subtree node
                                                              (lambda (n) (equal (treesit-node-type n) "function_declarator"))
                                                              nil t)))
        (when-let ((declarator (treesit-node-child-by-field-name function-declarator "declarator")))
          (treesit-node-text declarator t))))
     ((member node-type '("class_specifier" "struct_specifier"))
      (when-let ((name (treesit-search-subtree node
                                               (lambda (n) (equal (treesit-node-type n) "type_identifier"))
                                               nil t)))
        (treesit-node-text name t)))
     (t
      ""))))

(cl-defmethod treesitter-context-which-func-function (&context (major-mode c++-ts-mode))
  (treesitter-context--which-func-function-base treesitter-context--c++-which-func-node-types #'treesitter-context--c++-which-func-name))

;;; supported mode
(add-to-list 'treesitter-context--supported-mode 'c++-ts-mode t)
(add-to-list 'treesitter-context--fold-supported-mode 'c++-ts-mode t)
(add-to-list 'treesitter-context--focus-supported-mode 'c++-ts-mode t)
(add-to-list 'treesitter-context--which-func-supported-mode 'c++-ts-mode t)

(provide 'treesitter-context-cpp)
