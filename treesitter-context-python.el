;;; treesitter-context-python.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--python-node-types '("class_definition" "function_definition" "try_statement" "with_statement" "if_statement" "elif_clause" "else_clause" "case_clause" "while_statement" "except_clause" "match_statement" "for_statement")
  "Node types that may be showed.")

(defconst treesitter-context--python-query
  (treesit-query-compile 'python '(
                                   (class_definition body: (_) @context.end) @context
                                   ;; (function_definition body: (_) @context.end) @context
                                   (function_definition parameters: (_) :anchor ":" @context.end) @context
                                   (try_statement body: (_) @context.end) @context
                                   (with_statement :anchor body: (_) @context.end) @context
                                   ;; (for_statement body: (_) @context.end) @context
                                   (for_statement right: (_) :anchor ":" @context.end) @context
                                   ;; (if_statement consequence: (_) @context.end) @context
                                   (if_statement condition: (_) :anchor ":" @context.end) @context
                                   ;; (elif_clause consequence: (_) @context.end) @context
                                   (elif_clause condition: (_) :anchor ":" @context.end) @context
                                   ;; (else_clause body: (_) @context.end) @context
                                   (else_clause :anchor ":" @context.end) @context
                                   (case_clause consequence: (_) @context.end) @context
                                   (while_statement body: (_) @context.end) @context
                                   (except_clause (block) @context.end) @context
                                   (match_statement body: (_) @context.end) @context))
  "Query patterns to capture desired nodes.")

(defconst treesitter-context--python-focus-node-types '("class_definition" "function_definition" "try_statement" "with_statement" "if_statement" "elif_clause" "else_clause" "case_clause" "while_statement" "match_statement" "for_statement")
  "Node types that may be focused.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode python-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--python-node-types treesitter-context--python-query treesitter-context-frame-indent-offset))

(cl-defmethod treesitter-context-indent-context (node context indent-level indent-offset &context (major-mode python-ts-mode))
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("elif_clause" "else_clause" "except_clause"))
        (progn
          (setq treesitter-context--indent-level (- indent-level 1))
          (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))
      (setq treesitter-context--indent-level indent-level)
      (treesitter-context--indent-context context treesitter-context--indent-level indent-offset))))

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode python-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--python-focus-node-types))

(defconst treesitter-context--python-fold-node-types '("class_definition" "function_definition" "try_statement" "with_statement" "if_statement" "elif_clause" "else_clause" "case_clause" "while_statement" "match_statement" "for_statement")
  "Node types that may be folded.")

(cl-defmethod treesitter-context-fold-get-region (&context (major-mode python-ts-mode))
  "Get current code node's region."
  (treesitter-context-fold--get-region-base treesitter-context--python-fold-node-types))

;;; which-func
(defconst treesitter-context--python-which-func-node-types '("class_definition" "function_definition")
  "Node types that which-func cares about")

(defun treesitter-context--python-which-func-name (node)
  (let ((node-type (treesit-node-type node))
        name-node)
    (cond
     ((member node-type '("class_definition" "function_definition"))
      (setq name-node (treesit-node-child-by-field-name node "name"))
      (when name-node
        (treesit-node-text name-node t)))
     (t
      ""))))

(cl-defmethod treesitter-context-which-func-function (&context (major-mode python-ts-mode))
  (treesitter-context--which-func-function-base treesitter-context--python-which-func-node-types #'treesitter-context--python-which-func-name))

;;; supported mode
(add-to-list 'treesitter-context--supported-mode 'python-ts-mode t)
(add-to-list 'treesitter-context--fold-supported-mode 'python-ts-mode t)
(add-to-list 'treesitter-context--focus-supported-mode 'python-ts-mode t)
(add-to-list 'treesitter-context--which-func-supported-mode 'python-ts-mode t)

(provide 'treesitter-context-python)
