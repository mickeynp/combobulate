;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-json-combobulate-navigate-down--array-4 ()
 "Test `combobulate' with `fixtures/down/array.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/down/array.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)))


(ert-deftest combobulate-test-yaml-combobulate-navigate-down--blocks-7 ()
 "Test `combobulate' with `fixtures/down/blocks.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/down/blocks.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 7)))


(ert-deftest combobulate-test-css-combobulate-navigate-down--declaration-4 ()
 "Test `combobulate' with `fixtures/down/declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/down/declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)))


(ert-deftest combobulate-test-html-combobulate-navigate-down--down-attributes-3 ()
 "Test `combobulate' with `fixtures/down/down-attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/down/down-attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-html-combobulate-navigate-down--elements-3 ()
 "Test `combobulate' with `fixtures/down/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/down/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)))


(ert-deftest combobulate-test-python-combobulate-navigate-down--function-5 ()
 "Test `combobulate' with `fixtures/down/function.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/down/function.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-down--if-statements-10 ()
 "Test `combobulate' with `fixtures/down/if-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/down/if-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 9)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 10)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-down--jsx-with-attributes-6 ()
 "Test `combobulate' with `fixtures/down/jsx-with-attributes.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/down/jsx-with-attributes.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 6)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-down--module-statements-7 ()
 "Test `combobulate' with `fixtures/down/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/down/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 7)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-down--nested-jsx-8 ()
 "Test `combobulate' with `fixtures/down/nested-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/down/nested-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 8)))


(ert-deftest combobulate-test-json-combobulate-navigate-down--object-9 ()
 "Test `combobulate' with `fixtures/down/object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/down/object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-navigate-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-down)
	       (combobulate-test-assert-at-marker 9)))


