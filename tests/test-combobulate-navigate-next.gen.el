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


