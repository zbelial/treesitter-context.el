;;; treesitter-context-java.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--java-node-types '("if_statement"
                                                "for_statement"
                                                "enhanced_for_statement"
                                                "while_statement"
                                                "class_declaration"
                                                "method_declaration")
  "Node types should be showed.")

;; FIXME: there is no else_statement/else_clause in java parser,
;; so cannot decide whether the cursor is in else part or
(defconst treesitter-context--java-query
  '((if_statement consequence: (_) @context.end) @context
    ;; (if_statement (_) ("else" alternative: (_) @context.end) @context.real) @context
    (for_statement body: (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (enhanced_for_statement body: (_) @context.end) @context
    (method_declaration body: (_) @context.end) @context
    (class_declaration body: (_) @context.end) @context)
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode java-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--java-node-types treesitter-context--java-query java-ts-mode-indent-offset))

(add-to-list 'treesitter-context--supported-mode 'java-ts-mode t)

(provide 'treesitter-context-java)
