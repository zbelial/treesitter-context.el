;;; treesit-context-java.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesit-context-common)

(defconst treesit-context--java-node-types '("if_statement"
                                             "for_statement"
                                             "enhanced_for_statement"
                                             "while_statement"
                                             "class_declaration"
                                             "method_declaration")
  "Node types should be showed.")

;; FIXME: there is no else_statement/else_clause in java parser,
;; so cannot decide whether the cursor is in else part or not
(defconst treesit-context--java-query
  '((if_statement consequence: (_) @context.end) @context
    ;; (if_statement (_) ("else" alternative: (_) @context.end) @context.real) @context
    (for_statement body: (_) @context.end) @context
    (while_statement body: (_) @context.end) @context
    (enhanced_for_statement body: (_) @context.end) @context
    (method_declaration body: (_) @context.end) @context
    (class_declaration body: (_) @context.end) @context)
  "Query patterns to capture desired nodes.")

(cl-defmethod treesit-context-collect-contexts (&context (major-mode java-ts-mode))
  "Collect all of current node's parent nodes."
  (treesit-context-collect-contexts-base treesit-context--java-node-types treesit-context--java-query java-ts-mode-indent-offset))

(add-to-list 'treesit-context--supported-mode 'java-ts-mode t)

(provide 'treesit-context-java)
