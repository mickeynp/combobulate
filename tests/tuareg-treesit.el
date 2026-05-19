(require 'tuareg)
(require 'treesit)

(defun tuareg-treesit-bridge-create-parser ()
  "Create a Tree-sitter parser in the current Tuareg buffer if absent.
Assumes the OCaml Tree-sitter grammars are already installed by the user."
  (when (and (derived-mode-p 'tuareg-mode)
             (fboundp 'treesit-available-p) 
             (treesit-available-p)
             (treesit-language-available-p 'ocaml))
    (unless (treesit-parser-list)
      (treesit-parser-create
       (if (and buffer-file-name (string-match-p "\\.mli\\'" buffer-file-name))
           'ocaml-interface
         'ocaml)))))

(add-hook 'tuareg-mode-hook #'tuareg-treesit-bridge-create-parser)

(provide 'tuareg-treesit)
;;; tuareg-treesit.el ends here