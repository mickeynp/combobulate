;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-html-combobulate-navigate-previous--attributes-1 ()
 "Test `combobulate' with `fixtures/sibling/attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--block-1 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-previous--component-jsx-1 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--const-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/const-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/const-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-css-combobulate-navigate-previous--css-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-css-combobulate-navigate-previous--css-function-arg-1 ()
 "Test `combobulate' with `fixtures/sibling/css-function-arg.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-function-arg.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-css-combobulate-navigate-previous--css-nested-statements-1 ()
 "Test `combobulate' with `fixtures/sibling/css-nested-statements.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-nested-statements.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-css-combobulate-navigate-previous--css-property-1 ()
 "Test `combobulate' with `fixtures/sibling/css-property.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-property.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--def-block-1 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-previous--def-function-block-1 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-previous--def-function-object-args-1 ()
 "Test `combobulate' with `fixtures/sibling/def-function-object-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-object-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-previous--def-function-type-args-1 ()
 "Test `combobulate' with `fixtures/sibling/def-function-type-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-type-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--def-parameters-1 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-toml-combobulate-navigate-previous--document-level-1 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-html-combobulate-navigate-previous--elements-1 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-java-combobulate-navigate-previous--example-1 ()
 "Test `combobulate' with `fixtures/sibling/example.java' in `java-ts-mode' mode."
	     (combobulate-test
		 (:language java :mode java-ts-mode :fixture "fixtures/sibling/example.java")
	       :tags
	       '(combobulate java java-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--expr-switch-1 ()
 "Test `combobulate' with `fixtures/sibling/expr-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/expr-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--for-loop-1 ()
 "Test `combobulate' with `fixtures/sibling/for-loop.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/for-loop.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--import-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/import-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/import-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--inside-case-1 ()
 "Test `combobulate' with `fixtures/sibling/inside-case.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/inside-case.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)))


(ert-deftest combobulate-test-toml-combobulate-navigate-previous--inside-table-1 ()
 "Test `combobulate' with `fixtures/sibling/inside-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/inside-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--literal-1 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--map-1 ()
 "Test `combobulate' with `fixtures/sibling/map.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/map.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--module-statements-1 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-previous--module-statements-1 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--module-1 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--nested-blocks-1 ()
 "Test `combobulate' with `fixtures/sibling/nested-blocks.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/nested-blocks.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--program-1 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-dict-1 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-list-1 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-match-case-1 ()
 "Test `combobulate' with `fixtures/sibling/python-match-case.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-match-case.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-set-1 ()
 "Test `combobulate' with `fixtures/sibling/python-set.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-set.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-string-1 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-tuple-pattern-1 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-python-combobulate-navigate-previous--python-tuple-1 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--switch-1 ()
 "Test `combobulate' with `fixtures/sibling/switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--type-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/type-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/type-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-toml-combobulate-navigate-previous--value-array-1 ()
 "Test `combobulate' with `fixtures/sibling/value-array.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/value-array.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-go-combobulate-navigate-previous--var-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/var-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/var-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-previous--yaml-block-mapping-pairs-1 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-previous--yaml-block-mapping-1 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-previous--yaml-sequence-1 ()
 "Test `combobulate' with `fixtures/sibling/yaml-sequence.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-sequence.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-previous)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-previous)
	       (combobulate-test-assert-at-marker 1)))


