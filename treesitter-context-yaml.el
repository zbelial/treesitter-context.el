;;; treesitter-context-yaml.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--yaml-node-types '("block_mapping_pair")
  "Node types that may be showed.")

(defconst treesitter-context--yaml-query
  (treesit-query-compile 'yaml '((block_mapping_pair key: (_) :anchor ":" (_) @context.end) @context))
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode yaml-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--yaml-node-types treesitter-context--yaml-query treesitter-context-frame-indent-offset))

(defconst treesitter-context--yaml-focus-node-types '("block_mapping_pair")
  "Node types that may be focused.")

(cl-defmethod treesitter-context-focus-bounds (&context (major-mode yaml-ts-mode))
  "Return the bound that should be focused."
  (treesitter-context--focus-bounds treesitter-context--yaml-focus-node-types))

(add-to-list 'treesitter-context--supported-mode 'yaml-ts-mode t)
(add-to-list 'treesitter-context--focus-supported-mode 'yaml-ts-mode t)

(provide 'treesitter-context-yaml)
