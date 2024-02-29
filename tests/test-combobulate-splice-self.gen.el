;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-yaml-combobulate-splice-self--block-pairs-1 ()
 "Test `combobulate' with `fixtures/splice/block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/block-pairs.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-splice-self--block-pairs-2 ()
 "Test `combobulate' with `fixtures/splice/block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/block-pairs.yaml[@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-splice-self--block-pairs-3 ()
 "Test `combobulate' with `fixtures/splice/block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/block-pairs.yaml[@3~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-splice-self--block-pairs-4 ()
 "Test `combobulate' with `fixtures/splice/block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/block-pairs.yaml[@4~after].yaml")))


(ert-deftest combobulate-test-tsx-combobulate-splice-self--jsx-elements-1 ()
 "Test `combobulate' with `fixtures/splice/jsx-elements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/splice/jsx-elements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/jsx-elements.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-splice-self--jsx-elements-2 ()
 "Test `combobulate' with `fixtures/splice/jsx-elements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/splice/jsx-elements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/jsx-elements.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-splice-self--jsx-elements-3 ()
 "Test `combobulate' with `fixtures/splice/jsx-elements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/splice/jsx-elements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/jsx-elements.tsx[@3~after].tsx")))


(ert-deftest combobulate-test-html-combobulate-splice-self--nested-elements-1 ()
 "Test `combobulate' with `fixtures/splice/nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-elements.html[@1~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-self--nested-elements-2 ()
 "Test `combobulate' with `fixtures/splice/nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-elements.html[@2~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-self--nested-elements-3 ()
 "Test `combobulate' with `fixtures/splice/nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-elements.html[@3~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-self--nested-elements-4 ()
 "Test `combobulate' with `fixtures/splice/nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-elements.html[@4~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-self--nested-elements-5 ()
 "Test `combobulate' with `fixtures/splice/nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-elements.html[@5~after].html")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-1 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-2 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-3 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-4 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-5 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-6 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-self--nested-python-7 ()
 "Test `combobulate' with `fixtures/splice/nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/nested-python.py[@7~after].py")))


(ert-deftest combobulate-test-json-combobulate-splice-self--object-1 ()
 "Test `combobulate' with `fixtures/splice/object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/splice/object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/object.json[@1~after].json")))


(ert-deftest combobulate-test-json-combobulate-splice-self--object-2 ()
 "Test `combobulate' with `fixtures/splice/object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/splice/object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/object.json[@2~after].json")))


(ert-deftest combobulate-test-json-combobulate-splice-self--object-3 ()
 "Test `combobulate' with `fixtures/splice/object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/splice/object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-splice-self)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self/object.json[@3~after].json")))


