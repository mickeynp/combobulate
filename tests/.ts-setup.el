;;-*- mode: emacs-lisp; -*-

(let ((treesit-language-source-alist
       '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
         (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
         (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile" "v0.2.0"))
         (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
         (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
         (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
         (toml "https://github.com/tree-sitter/tree-sitter-toml")
         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
         (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))))
  (message "* Installing tree-sitter grammars...")
  (pcase-dolist (`(,lang . ,rest) treesit-language-source-alist)
    (if (treesit-language-available-p lang)
        (message "** %s already installed" lang)
      (message "** Installing %s..." lang)
      (treesit-install-language-grammar lang))))

;;; Helper for the batch emacs command
(setq load-prefer-newer t)
(princ (format "Default directory is `%s'\n" default-directory))
(load-library "tests/html-ts-mode/html-ts-mode.el")

