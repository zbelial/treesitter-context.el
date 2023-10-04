;;; treesitter-context-toml.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--toml-node-types '("table" "pair")
  "Node types should be showed.")

(defconst treesitter-context--toml-query
  '((table "[" (_) "]" :anchor (_) @context.end) @context
    (pair (_) :anchor "=" @context.end) @context
    )
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode toml-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--toml-node-types treesitter-context--toml-query treesitter-context-frame-indent-offset))

(defconst treesitter-context--toml-focus-node-types '("table" "pair")
  "Node types should be showed.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode toml-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--toml-focus-node-types))

(add-to-list 'treesitter-context--supported-mode 'toml-ts-mode t)

(provide 'treesitter-context-toml)
