(require 'tuareg)
(require 'treesit)

(defgroup tuareg-treesit-bridge nil
  "Create Tree-sitter parsers in Tuareg buffers for tools like Combobulate."
  :group 'tuareg)

(defcustom tuareg-treesit-bridge-enable t
  "Whether to auto-create a Tree-sitter parser in Tuareg buffers."
  :type 'boolean
  :group 'tuareg-treesit-bridge)

(defcustom tuareg-treesit-bridge-install-missing-grammars nil
  "If non-nil, attempt to install OCaml grammars when missing."
  :type 'boolean
  :group 'tuareg-treesit-bridge)

(defun tuareg-treesit-bridge--ensure-grammars ()
  (when (and tuareg-treesit-bridge-install-missing-grammars
             (not (treesit-language-available-p 'ocaml)))
    (add-to-list 'treesit-language-source-alist
                 '(ocaml "https://github.com/tree-sitter/tree-sitter-ocaml"
                         "grammars/ocaml/src" "grammars/interface/src"))
    (ignore-errors
      (treesit-install-language-grammar 'ocaml))))

(defun tuareg-treesit-bridge--maybe-create-parser ()
  "Create a Tree-sitter parser in the current Tuareg buffer if absent."
  (when (and tuareg-treesit-bridge-enable
             (derived-mode-p 'tuareg-mode)
             (fboundp 'treesit-available-p) (treesit-available-p))
    (tuareg-treesit-bridge--ensure-grammars)
    (when (treesit-language-available-p 'ocaml)
      (unless (treesit-parser-list)
        (treesit-parser-create
         (if (and buffer-file-name (string-match-p "\\.mli\\'" buffer-file-name))
             'ocaml_interface
           'ocaml))))))

(add-hook 'tuareg-mode-hook #'tuareg-treesit-bridge--maybe-create-parser)

(provide 'tuareg-treesit)
;;; tuareg-treesit.el ends here
