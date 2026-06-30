;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-html-combobulate-navigate-next--attributes-3
    ()

  "Test `combobulate' with `fixtures/sibling/attributes.html' in `html-ts-mode' mode."
  (combobulate-test
      (:language html :mode html-ts-mode :fixture
		 "fixtures/sibling/attributes.html")
    :tags '(combobulate html html-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--block-6
    ()

  "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/block.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--class_type_expressions-3
    ()

  "Test `combobulate' with `fixtures/sibling/class_type_expressions.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/class_type_expressions.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--class_type_expressions-3
    ()

  "Test `combobulate' with `fixtures/sibling/class_type_expressions.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/class_type_expressions.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--class_value_expressions-3
    ()

  "Test `combobulate' with `fixtures/sibling/class_value_expressions.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/class_value_expressions.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--class_value_expressions-3
    ()

  "Test `combobulate' with `fixtures/sibling/class_value_expressions.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/class_value_expressions.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-next--component-jsx-5
    ()

  "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/sibling/component-jsx.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-go-combobulate-navigate-next--const-declaration-2
    ()

  "Test `combobulate' with `fixtures/sibling/const-declaration.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/const-declaration.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-next--css-declaration-4
    ()

  "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
		 "fixtures/sibling/css-declaration.css")
    :tags '(combobulate css css-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-next--css-function-arg-3
    ()

  "Test `combobulate' with `fixtures/sibling/css-function-arg.css' in `css-ts-mode' mode."
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
		 "fixtures/sibling/css-function-arg.css")
    :tags '(combobulate css css-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-next--css-nested-statements-3
    ()

  "Test `combobulate' with `fixtures/sibling/css-nested-statements.css' in `css-ts-mode' mode."
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
		 "fixtures/sibling/css-nested-statements.css")
    :tags '(combobulate css css-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-next--css-property-3
    ()

  "Test `combobulate' with `fixtures/sibling/css-property.css' in `css-ts-mode' mode."
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
		 "fixtures/sibling/css-property.css")
    :tags '(combobulate css css-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--def-block-5
    ()

  "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/def-block.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-next--def-function-block-6
    ()

  "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/sibling/def-function-block.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-next--def-function-object-args-3
    ()

  "Test `combobulate' with `fixtures/sibling/def-function-object-args.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/sibling/def-function-object-args.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-next--def-function-type-args-3
    ()

  "Test `combobulate' with `fixtures/sibling/def-function-type-args.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/sibling/def-function-type-args.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--def-parameters-9
    ()

  "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/def-parameters.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)))


(ert-deftest
    combobulate-test-toml-combobulate-navigate-next--document-level-7
    ()

  "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
  (combobulate-test
      (:language toml :mode toml-ts-mode :fixture
		 "fixtures/sibling/document-level.toml")
    :tags '(combobulate toml toml-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-html-combobulate-navigate-next--elements-7 ()

  "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
  (combobulate-test
      (:language html :mode html-ts-mode :fixture
		 "fixtures/sibling/elements.html")
    :tags '(combobulate html html-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-go-combobulate-navigate-next--expr-switch-3
    ()

  "Test `combobulate' with `fixtures/sibling/expr-switch.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/expr-switch.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--extension_points-7
    ()

  "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/extension_points.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--extension_points-5
    ()

  "Test `combobulate' with `fixtures/sibling/extension_points.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/extension_points.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--for-loop-3
    ()

  "Test `combobulate' with `fixtures/sibling/for-loop.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/for-loop.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-next--gadt-3
    ()

  "Test `combobulate' with `fixtures/sibling/gadt.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/gadt.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--gadt-3
    ()

  "Test `combobulate' with `fixtures/sibling/gadt.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/gadt.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-go-combobulate-navigate-next--import-declaration-2
    ()

  "Test `combobulate' with `fixtures/sibling/import-declaration.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/import-declaration.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-go-combobulate-navigate-next--inside-case-1
    ()

  "Test `combobulate' with `fixtures/sibling/inside-case.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/inside-case.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)))


(ert-deftest
    combobulate-test-toml-combobulate-navigate-next--inside-table-3
    ()

  "Test `combobulate' with `fixtures/sibling/inside-table.toml' in `toml-ts-mode' mode."
  (combobulate-test
      (:language toml :mode toml-ts-mode :fixture
		 "fixtures/sibling/inside-table.toml")
    :tags '(combobulate toml toml-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--let_and_in-3
    ()

  "Test `combobulate' with `fixtures/sibling/let_and_in.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/let_and_in.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--let_bindings-11
    ()

  "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/let_bindings.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--literal-6
    ()

  "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/literal.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--map-2
    ()

  "Test `combobulate' with `fixtures/sibling/map.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/map.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--module-statements-8
    ()

  "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/module-statements.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-next--module-statements-5
    ()

  "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/sibling/module-statements.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--module-6
    ()

  "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/module.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--module_sig_a-4
    ()

  "Test `combobulate' with `fixtures/sibling/module_sig_a.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/module_sig_a.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--module_sig_a-4
    ()

  "Test `combobulate' with `fixtures/sibling/module_sig_a.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/module_sig_a.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--module_sig_b-3
    ()

  "Test `combobulate' with `fixtures/sibling/module_sig_b.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/module_sig_b.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--module_signatures-5
    ()

  "Test `combobulate' with `fixtures/sibling/module_signatures.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/module_signatures.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--module_string-2
    ()

  "Test `combobulate' with `fixtures/sibling/module_string.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/module_string.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--module_type_expressions-6
    ()

  "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/module_type_expressions.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--module_type_expressions-6
    ()

  "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/module_type_expressions.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--module_value_expressions-6
    ()

  "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/module_value_expressions.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--modules-11
    ()

  "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/modules.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--modules-10
    ()

  "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/modules.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--nested-blocks-3
    ()

  "Test `combobulate' with `fixtures/sibling/nested-blocks.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/nested-blocks.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--nested_type_records-2
    ()

  "Test `combobulate' with `fixtures/sibling/nested_type_records.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/nested_type_records.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--nested_type_records-2
    ()

  "Test `combobulate' with `fixtures/sibling/nested_type_records.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/nested_type_records.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--patterns-18
    ()

  "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/patterns.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)
    (combobulate-test-go-to-marker 11) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 12)
    (combobulate-test-go-to-marker 12) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 13)
    (combobulate-test-go-to-marker 13) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 14)
    (combobulate-test-go-to-marker 14) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 15)
    (combobulate-test-go-to-marker 15) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 16)
    (combobulate-test-go-to-marker 16) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 17)
    (combobulate-test-go-to-marker 17) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 18)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--patterns-5
    ()

  "Test `combobulate' with `fixtures/sibling/patterns.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/patterns.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--program-7
    ()

  "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/program.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-dict-7
    ()

  "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-dict.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-list-5
    ()

  "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-list.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-match-case-3
    ()

  "Test `combobulate' with `fixtures/sibling/python-match-case.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-match-case.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-set-3
    ()

  "Test `combobulate' with `fixtures/sibling/python-set.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-set.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-string-6
    ()

  "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-string.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-tuple-pattern-5
    ()

  "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-tuple-pattern.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-next--python-tuple-5
    ()

  "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/sibling/python-tuple.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--signature_attributes-3
    ()

  "Test `combobulate' with `fixtures/sibling/signature_attributes.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/signature_attributes.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--switch-3
    ()

  "Test `combobulate' with `fixtures/sibling/switch.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/switch.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-go-combobulate-navigate-next--type-declaration-2
    ()

  "Test `combobulate' with `fixtures/sibling/type-declaration.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/type-declaration.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--type_and_bindings-3
    ()

  "Test `combobulate' with `fixtures/sibling/type_and_bindings.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/type_and_bindings.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--type_declarations-16
    ()

  "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/type_declarations.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)
    (combobulate-test-go-to-marker 11) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 12)
    (combobulate-test-go-to-marker 12) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 13)
    (combobulate-test-go-to-marker 13) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 14)
    (combobulate-test-go-to-marker 14) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 15)
    (combobulate-test-go-to-marker 15) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 16)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--type_declarations-16
    ()

  "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/type_declarations.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)
    (combobulate-test-go-to-marker 11) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 12)
    (combobulate-test-go-to-marker 12) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 13)
    (combobulate-test-go-to-marker 13) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 14)
    (combobulate-test-go-to-marker 14) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 15)
    (combobulate-test-go-to-marker 15) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 16)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--type_expressions-8
    ()

  "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/type_expressions.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--type_expressions-8
    ()

  "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/type_expressions.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--type_records-4
    ()

  "Test `combobulate' with `fixtures/sibling/type_records.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/type_records.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--type_records-4
    ()

  "Test `combobulate' with `fixtures/sibling/type_records.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/type_records.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-toml-combobulate-navigate-next--value-array-3
    ()

  "Test `combobulate' with `fixtures/sibling/value-array.toml' in `toml-ts-mode' mode."
  (combobulate-test
      (:language toml :mode toml-ts-mode :fixture
		 "fixtures/sibling/value-array.toml")
    :tags '(combobulate toml toml-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-next--value_expressions-49
    ()

  "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/sibling/value_expressions.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)
    (combobulate-test-go-to-marker 11) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 12)
    (combobulate-test-go-to-marker 12) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 13)
    (combobulate-test-go-to-marker 13) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 14)
    (combobulate-test-go-to-marker 14) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 15)
    (combobulate-test-go-to-marker 15) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 16)
    (combobulate-test-go-to-marker 16) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 17)
    (combobulate-test-go-to-marker 17) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 18)
    (combobulate-test-go-to-marker 18) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 19)
    (combobulate-test-go-to-marker 19) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 20)
    (combobulate-test-go-to-marker 20) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 21)
    (combobulate-test-go-to-marker 21) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 22)
    (combobulate-test-go-to-marker 22) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 23)
    (combobulate-test-go-to-marker 23) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 24)
    (combobulate-test-go-to-marker 24) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 25)
    (combobulate-test-go-to-marker 25) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 26)
    (combobulate-test-go-to-marker 26) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 27)
    (combobulate-test-go-to-marker 27) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 28)
    (combobulate-test-go-to-marker 28) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 29)
    (combobulate-test-go-to-marker 29) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 30)
    (combobulate-test-go-to-marker 30) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 31)
    (combobulate-test-go-to-marker 31) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 32)
    (combobulate-test-go-to-marker 32) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 33)
    (combobulate-test-go-to-marker 33) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 34)
    (combobulate-test-go-to-marker 34) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 35)
    (combobulate-test-go-to-marker 35) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 36)
    (combobulate-test-go-to-marker 36) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 37)
    (combobulate-test-go-to-marker 37) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 38)
    (combobulate-test-go-to-marker 38) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 39)
    (combobulate-test-go-to-marker 39) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 40)
    (combobulate-test-go-to-marker 40) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 41)
    (combobulate-test-go-to-marker 41) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 42)
    (combobulate-test-go-to-marker 42) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 43)
    (combobulate-test-go-to-marker 43) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 44)
    (combobulate-test-go-to-marker 44) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 45)
    (combobulate-test-go-to-marker 45) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 46)
    (combobulate-test-go-to-marker 46) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 47)
    (combobulate-test-go-to-marker 47) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 48)
    (combobulate-test-go-to-marker 48) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 49)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--value_expressions-42
    ()

  "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/value_expressions.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)
    (combobulate-test-go-to-marker 11) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 12)
    (combobulate-test-go-to-marker 12) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 13)
    (combobulate-test-go-to-marker 13) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 14)
    (combobulate-test-go-to-marker 14) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 15)
    (combobulate-test-go-to-marker 15) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 16)
    (combobulate-test-go-to-marker 16) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 17)
    (combobulate-test-go-to-marker 17) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 18)
    (combobulate-test-go-to-marker 18) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 19)
    (combobulate-test-go-to-marker 19) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 20)
    (combobulate-test-go-to-marker 20) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 21)
    (combobulate-test-go-to-marker 21) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 22)
    (combobulate-test-go-to-marker 22) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 23)
    (combobulate-test-go-to-marker 23) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 24)
    (combobulate-test-go-to-marker 24) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 25)
    (combobulate-test-go-to-marker 25) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 26)
    (combobulate-test-go-to-marker 26) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 27)
    (combobulate-test-go-to-marker 27) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 28)
    (combobulate-test-go-to-marker 28) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 29)
    (combobulate-test-go-to-marker 29) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 30)
    (combobulate-test-go-to-marker 30) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 31)
    (combobulate-test-go-to-marker 31) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 32)
    (combobulate-test-go-to-marker 32) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 33)
    (combobulate-test-go-to-marker 33) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 34)
    (combobulate-test-go-to-marker 34) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 35)
    (combobulate-test-go-to-marker 35) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 36)
    (combobulate-test-go-to-marker 36) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 37)
    (combobulate-test-go-to-marker 37) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 38)
    (combobulate-test-go-to-marker 38) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 39)
    (combobulate-test-go-to-marker 39) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 40)
    (combobulate-test-go-to-marker 40) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 41)
    (combobulate-test-go-to-marker 41) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 42)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-next--value_specification-11
    ()

  "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/sibling/value_specification.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 10)
    (combobulate-test-go-to-marker 10) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 11)))


(ert-deftest
    combobulate-test-go-combobulate-navigate-next--var-declaration-2
    ()

  "Test `combobulate' with `fixtures/sibling/var-declaration.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture
		 "fixtures/sibling/var-declaration.go")
    :tags '(combobulate go go-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-yaml-combobulate-navigate-next--yaml-block-mapping-pairs-2
    ()

  "Test `combobulate' with `fixtures/sibling/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
		 "fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags '(combobulate yaml yaml-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-yaml-combobulate-navigate-next--yaml-block-mapping-3
    ()

  "Test `combobulate' with `fixtures/sibling/yaml-block-mapping.yaml' in `yaml-ts-mode' mode."
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
		 "fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(combobulate yaml yaml-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-yaml-combobulate-navigate-next--yaml-sequence-2
    ()

  "Test `combobulate' with `fixtures/sibling/yaml-sequence.yaml' in `yaml-ts-mode' mode."
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
		 "fixtures/sibling/yaml-sequence.yaml")
    :tags '(combobulate yaml yaml-ts-mode combobulate-navigate-next)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-next)
    (combobulate-test-assert-at-marker 2)))


