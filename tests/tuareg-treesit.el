(require 'tuareg)
(require 'treesit)

(defun tuareg-treesit-setup ()
  "Use the correct tree-sitter parser based on file extension."
  (let ((correct-lang
         (if (and buffer-file-name
                  (string-match-p "\\.mli\\'" buffer-file-name))
             'ocaml-interface
           'ocaml)))
    (when (treesit-language-available-p correct-lang)
      (mapc #'treesit-parser-delete (treesit-parser-list))
      (treesit-parser-create correct-lang)
      (when (bound-and-true-p combobulate-mode)
        (combobulate-mode -1)
        (combobulate-mode 1))
      (font-lock-ensure)
      (message "tuareg-treesit: using %s" correct-lang))))

(add-hook 'tuareg-mode-hook #'tuareg-treesit-setup)

(provide 'tuareg-treesit)
;;; tuareg-treesit.el ends here