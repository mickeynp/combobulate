;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-html-combobulate-navigate-next--attributes-3 ()
 "Test `combobulate' with `fixtures/sibling/attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--block-6 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-next--component-jsx-5 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--const-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/const-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/const-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


(ert-deftest combobulate-test-css-combobulate-navigate-next--css-declaration-4 ()
 "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)))


(ert-deftest combobulate-test-css-combobulate-navigate-next--css-function-arg-3 ()
 "Test `combobulate' with `fixtures/sibling/css-function-arg.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-function-arg.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-css-combobulate-navigate-next--css-nested-statements-3 ()
 "Test `combobulate' with `fixtures/sibling/css-nested-statements.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-nested-statements.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-css-combobulate-navigate-next--css-property-3 ()
 "Test `combobulate' with `fixtures/sibling/css-property.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-property.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--def-block-5 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-next--def-function-block-6 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-next--def-function-object-args-3 ()
 "Test `combobulate' with `fixtures/sibling/def-function-object-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-object-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-next--def-function-type-args-3 ()
 "Test `combobulate' with `fixtures/sibling/def-function-type-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-type-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--def-parameters-9 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 9)))


(ert-deftest combobulate-test-toml-combobulate-navigate-next--document-level-7 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 7)))


(ert-deftest combobulate-test-html-combobulate-navigate-next--elements-7 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 7)))


(ert-deftest combobulate-test-java-combobulate-navigate-next--example-5 ()
 "Test `combobulate' with `fixtures/sibling/example.java' in `java-ts-mode' mode."
	     (combobulate-test
		 (:language java :mode java-ts-mode :fixture "fixtures/sibling/example.java")
	       :tags
	       '(combobulate java java-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--expr-switch-3 ()
 "Test `combobulate' with `fixtures/sibling/expr-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/expr-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--for-loop-3 ()
 "Test `combobulate' with `fixtures/sibling/for-loop.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/for-loop.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--import-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/import-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/import-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--inside-case-1 ()
 "Test `combobulate' with `fixtures/sibling/inside-case.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/inside-case.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)))


(ert-deftest combobulate-test-toml-combobulate-navigate-next--inside-table-3 ()
 "Test `combobulate' with `fixtures/sibling/inside-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/inside-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--literal-6 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--map-2 ()
 "Test `combobulate' with `fixtures/sibling/map.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/map.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--module-statements-8 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 8)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-next--module-statements-5 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--module-6 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--nested-blocks-3 ()
 "Test `combobulate' with `fixtures/sibling/nested-blocks.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/nested-blocks.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--program-7 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 7)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-dict-7 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 7)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-list-5 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-match-case-3 ()
 "Test `combobulate' with `fixtures/sibling/python-match-case.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-match-case.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-set-3 ()
 "Test `combobulate' with `fixtures/sibling/python-set.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-set.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-string-6 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-tuple-pattern-5 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-python-combobulate-navigate-next--python-tuple-5 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--switch-3 ()
 "Test `combobulate' with `fixtures/sibling/switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--type-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/type-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/type-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


(ert-deftest combobulate-test-toml-combobulate-navigate-next--value-array-3 ()
 "Test `combobulate' with `fixtures/sibling/value-array.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/value-array.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-go-combobulate-navigate-next--var-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/var-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/var-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-next--yaml-block-mapping-pairs-2 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-next--yaml-block-mapping-3 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-next--yaml-sequence-2 ()
 "Test `combobulate' with `fixtures/sibling/yaml-sequence.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-sequence.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-next)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-next)
	       (combobulate-test-assert-at-marker 2)))


