;;; treesitter-context-yaml.el --- Show context information around current point -*- lexical-binding: t; -*-

(require 'treesitter-context-common)

(defconst treesitter-context--yaml-node-types '("block_mapping_pair")
  "Node types should be showed.")

(defconst treesitter-context--yaml-query
  '((block_mapping_pair key: (_) :anchor ":" (_) @context.end) @context)
  "Query patterns to capture desired nodes.")

(cl-defmethod treesitter-context-collect-contexts (&context (major-mode yaml-ts-mode))
  "Collect all of current node's parent nodes."
  (treesitter-context-collect-contexts-base treesitter-context--yaml-node-types treesitter-context--yaml-query 4))

(add-to-list 'treesitter-context--supported-mode 'yaml-ts-mode t)

(provide 'treesitter-context-yaml)
