;;; test-alias-lookup.el --- Tests for Combobulate language aliasing
;;; Code:

(require 'ert)
(require 'combobulate-setup)

(ert-deftest combobulate-test-resolve-treesit-language-global ()
  "Ensure `combobulate-resolve-treesit-language' successfully resolves the globally defined aliases out-of-the-box."
  ;; Without any local mocking, it must pull from the globally defined defcustom map.
  ;; The primary language requiring this in Combobulate is `ocaml-interface`.
  (should (eq (combobulate-resolve-treesit-language 'ocaml-interface) 'ocaml_interface))
  
  ;; Other languages should remain unchanged out of the box because they have canonical tree-sitter names
  (should (eq (combobulate-resolve-treesit-language 'ocaml) 'ocaml))
  (should (eq (combobulate-resolve-treesit-language 'python) 'python))
  (should (eq (combobulate-resolve-treesit-language 'javascript) 'javascript))
  (should (eq (combobulate-resolve-treesit-language 'go) 'go)))

(ert-deftest combobulate-test-resolve-treesit-language-custom ()
  "Ensure users can dynamically inject their own alias rules."
  (let ((combobulate-treesit-language-aliases '((ocaml-interface . ocaml_interface)
                                                (custom-ts-lang . some_custom_c_parser))))
    (should (eq (combobulate-resolve-treesit-language 'ocaml-interface) 'ocaml_interface))
    (should (eq (combobulate-resolve-treesit-language 'custom-ts-lang) 'some_custom_c_parser))))

(provide 'test-alias-lookup)
;;; test-alias-lookup.el ends here
