;;; treesitter-context-json.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--json-node-types '("pair")
  "Node types should be showed.")

(defconst treesitter-context--json-query
  '((pair value: (_) @context.end) @context)
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode json-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--json-node-types treesitter-context--json-query treesitter-context-frame-indent-offset))

(defconst treesitter-context--json-focus-node-types '("pair")
  "Node types should be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode json-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--json-focus-node-types))

(add-to-list 'treesitter-context--supported-mode 'json-ts-mode t)

(provide 'treesitter-context-json)
