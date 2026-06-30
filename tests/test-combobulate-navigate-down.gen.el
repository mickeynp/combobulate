;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-json-combobulate-navigate-down--array-4
    ()

  "Test `combobulate' with `fixtures/down/array.json' in `json-ts-mode' mode."
  (combobulate-test
      (:language json :mode json-ts-mode :fixture
		 "fixtures/down/array.json")
    :tags '(combobulate json json-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-down--blocks-7
    ()

  "Test `combobulate' with `fixtures/down/blocks.yaml' in `yaml-ts-mode' mode."
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
		 "fixtures/down/blocks.yaml")
    :tags '(combobulate yaml yaml-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--class_members-4
    ()

  "Test `combobulate' with `fixtures/down/class_members.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/class_members.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--class_type_members-4
    ()

  "Test `combobulate' with `fixtures/down/class_type_members.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/class_type_members.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--class_type_members-4
    ()

  "Test `combobulate' with `fixtures/down/class_type_members.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/class_type_members.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--class_types-1
    ()

  "Test `combobulate' with `fixtures/down/class_types.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/class_types.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--class_types-1
    ()

  "Test `combobulate' with `fixtures/down/class_types.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/class_types.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--class_types_b-1
    ()

  "Test `combobulate' with `fixtures/down/class_types_b.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/class_types_b.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--class_types_b-4
    ()

  "Test `combobulate' with `fixtures/down/class_types_b.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/class_types_b.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--classes-2 ()

  "Test `combobulate' with `fixtures/down/classes.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/classes.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--classes-2
    ()

  "Test `combobulate' with `fixtures/down/classes.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/classes.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-down--declaration-4
    ()

  "Test `combobulate' with `fixtures/down/declaration.css' in `css-ts-mode' mode."
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
		 "fixtures/down/declaration.css")
    :tags '(combobulate css css-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-down--decorator_class-7
    ()

  "Test `combobulate' with `fixtures/down/decorator_class.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/down/decorator_class.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-down--decorator_function-5
    ()

  "Test `combobulate' with `fixtures/down/decorator_function.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/down/decorator_function.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-html-combobulate-navigate-down--down-attributes-3
    ()

  "Test `combobulate' with `fixtures/down/down-attributes.html' in `html-ts-mode' mode."
  (combobulate-test
      (:language html :mode html-ts-mode :fixture
		 "fixtures/down/down-attributes.html")
    :tags '(combobulate html html-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-html-combobulate-navigate-down--elements-3 ()

  "Test `combobulate' with `fixtures/down/elements.html' in `html-ts-mode' mode."
  (combobulate-test
      (:language html :mode html-ts-mode :fixture
		 "fixtures/down/elements.html")
    :tags '(combobulate html html-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-down--function-5
    ()

  "Test `combobulate' with `fixtures/down/function.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
		 "fixtures/down/function.py")
    :tags
    '(combobulate python python-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--function_body-2
    ()

  "Test `combobulate' with `fixtures/down/function_body.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/function_body.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--functors-2
    ()

  "Test `combobulate' with `fixtures/down/functors.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/functors.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--functors-2
    ()

  "Test `combobulate' with `fixtures/down/functors.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/functors.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-down--if-statements-10
    ()

  "Test `combobulate' with `fixtures/down/if-statements.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/down/if-statements.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 9)
    (combobulate-test-go-to-marker 9) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 10)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-down--jsx-with-attributes-6
    ()

  "Test `combobulate' with `fixtures/down/jsx-with-attributes.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/down/jsx-with-attributes.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--match_body-2
    ()

  "Test `combobulate' with `fixtures/down/match_body.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/match_body.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--matches-2 ()

  "Test `combobulate' with `fixtures/down/matches.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/matches.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-down--module-statements-7
    ()

  "Test `combobulate' with `fixtures/down/module-statements.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/down/module-statements.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 7)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--module_sig_a-4
    ()

  "Test `combobulate' with `fixtures/down/module_sig_a.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/module_sig_a.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--module_sig_a-4
    ()

  "Test `combobulate' with `fixtures/down/module_sig_a.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/module_sig_a.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--module_sig_b-4
    ()

  "Test `combobulate' with `fixtures/down/module_sig_b.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/module_sig_b.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--module_signature-4
    ()

  "Test `combobulate' with `fixtures/down/module_signature.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/module_signature.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--module_string-1
    ()

  "Test `combobulate' with `fixtures/down/module_string.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/module_string.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-down--nested-jsx-8
    ()

  "Test `combobulate' with `fixtures/down/nested-jsx.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/down/nested-jsx.tsx")
    :tags '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 8)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--nested_matches-2
    ()

  "Test `combobulate' with `fixtures/down/nested_matches.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/nested_matches.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))))


(ert-deftest combobulate-test-json-combobulate-navigate-down--object-9
    ()

  "Test `combobulate' with `fixtures/down/object.json' in `json-ts-mode' mode."
  (combobulate-test
      (:language json :mode json-ts-mode :fixture
		 "fixtures/down/object.json")
    :tags '(combobulate json json-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)
    (combobulate-test-go-to-marker 5) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 6)
    (combobulate-test-go-to-marker 6) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 7)
    (combobulate-test-go-to-marker 7) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 8)
    (combobulate-test-go-to-marker 8) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 9)))


(ert-deftest combobulate-test-toml-combobulate-navigate-down--table-3
    ()

  "Test `combobulate' with `fixtures/down/table.toml' in `toml-ts-mode' mode."
  (combobulate-test
      (:language toml :mode toml-ts-mode :fixture
		 "fixtures/down/table.toml")
    :tags '(combobulate toml toml-ts-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)))


(ert-deftest
    combobulate-test-ocaml-combobulate-navigate-down--type_declarations-2
    ()

  "Test `combobulate' with `fixtures/down/type_declarations.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/down/type_declarations.ml")
    :tags '(combobulate ocaml tuareg-mode combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--type_declarations-5
    ()

  "Test `combobulate' with `fixtures/down/type_declarations.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/type_declarations.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (combobulate-test-go-to-marker 1) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 2)
    (combobulate-test-go-to-marker 2) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 3)
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)
    (combobulate-test-go-to-marker 4) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 5)))


(ert-deftest
    combobulate-test-ocaml-interface-combobulate-navigate-down--variant_type-4
    ()

  "Test `combobulate' with `fixtures/down/variant_type.mli' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml-interface :mode tuareg-mode :fixture
		 "fixtures/down/variant_type.mli")
    :tags
    '(combobulate ocaml-interface tuareg-mode
		  combobulate-navigate-down)
    (should-error (progn (combobulate-navigate-down)))
    (should-error (progn (combobulate-navigate-down)))
    (combobulate-test-go-to-marker 3) (combobulate-navigate-down)
    (combobulate-test-assert-at-marker 4)))


