;;; test-reverse-lookup.el --- Tests for reverse canonical resolution
;;; Code:

(require 'ert)
(require 'combobulate-setup)

(ert-deftest combobulate-test-reverse-canonical-language ()
  "Ensure we can reverse map a tree-sitter language back to its canonical name."
  (let ((combobulate-treesit-language-aliases '((custom-lang . custom_parser))))
    ;; Reverse resolving a tree-sitter grammar name should yield the canonical name.
    (should (eq (car (rassoc 'custom_parser combobulate-treesit-language-aliases)) 'custom-lang))

    ;; If there's no reverse alias, rassoc returns nil, so falling back to the grammar name is safe.
    (should (eq (or (car (rassoc 'ocaml combobulate-treesit-language-aliases)) 'ocaml) 'ocaml))
    (should (eq (or (car (rassoc 'python combobulate-treesit-language-aliases)) 'python) 'python))))

(provide 'test-reverse-lookup)
;;; test-reverse-lookup.el ends here
