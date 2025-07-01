;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-yaml-combobulate-splice-up--choice-0-block-pairs-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/choice-0-block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-block-pairs.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-splice-up--choice-0-block-pairs-2 ()
 "Test `combobulate' with `fixtures/splice/choice-0-block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/choice-0-block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-block-pairs.yaml[@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-splice-up--choice-0-block-pairs-3 ()
 "Test `combobulate' with `fixtures/splice/choice-0-block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/choice-0-block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-block-pairs.yaml[@3~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-splice-up--choice-0-block-pairs-4 ()
 "Test `combobulate' with `fixtures/splice/choice-0-block-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/splice/choice-0-block-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-block-pairs.yaml[@4~after].yaml")))


(ert-deftest combobulate-test-tsx-combobulate-splice-up--choice-0-jsx-elements-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-jsx-elements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/splice/choice-0-jsx-elements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-jsx-elements.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-splice-up--choice-0-jsx-elements-2 ()
 "Test `combobulate' with `fixtures/splice/choice-0-jsx-elements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/splice/choice-0-jsx-elements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-jsx-elements.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-splice-up--choice-0-jsx-elements-3 ()
 "Test `combobulate' with `fixtures/splice/choice-0-jsx-elements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/splice/choice-0-jsx-elements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-jsx-elements.tsx[@3~after].tsx")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-messy-splice-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-messy-splice.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-messy-splice.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-messy-splice.py[@1~after].py")))


(ert-deftest combobulate-test-html-combobulate-splice-up--choice-0-nested-elements-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/choice-0-nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-elements.html[@1~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-up--choice-0-nested-elements-2 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/choice-0-nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-elements.html[@2~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-up--choice-0-nested-elements-3 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/choice-0-nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-elements.html[@3~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-up--choice-0-nested-elements-4 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/choice-0-nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-elements.html[@4~after].html")))


(ert-deftest combobulate-test-html-combobulate-splice-up--choice-0-nested-elements-5 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/splice/choice-0-nested-elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-elements.html[@5~after].html")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-2 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-3 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-4 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-5 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-6 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-splice-up--choice-0-nested-python-7 ()
 "Test `combobulate' with `fixtures/splice/choice-0-nested-python.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/choice-0-nested-python.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-nested-python.py[@7~after].py")))


(ert-deftest combobulate-test-json-combobulate-splice-up--choice-0-object-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/splice/choice-0-object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-object.json[@1~after].json")))


(ert-deftest combobulate-test-json-combobulate-splice-up--choice-0-object-2 ()
 "Test `combobulate' with `fixtures/splice/choice-0-object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/splice/choice-0-object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-object.json[@2~after].json")))


(ert-deftest combobulate-test-json-combobulate-splice-up--choice-0-object-3 ()
 "Test `combobulate' with `fixtures/splice/choice-0-object.json' in `json-ts-mode' mode."
	     (combobulate-test
		 (:language json :mode json-ts-mode :fixture "fixtures/splice/choice-0-object.json")
	       :tags
	       '(combobulate json json-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-object.json[@3~after].json")))


(ert-deftest combobulate-test-toml-combobulate-splice-up--choice-0-pairs-in-table-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-pairs-in-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/splice/choice-0-pairs-in-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-pairs-in-table.toml[@1~after].toml")))


(ert-deftest combobulate-test-go-combobulate-splice-up--choice-0-vars-1 ()
 "Test `combobulate' with `fixtures/splice/choice-0-vars.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-0-vars.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-0-vars.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-splice-up--choice-1-for-loop-body-1 ()
 "Test `combobulate' with `fixtures/splice/choice-1-for-loop-body.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-1-for-loop-body.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-1-for-loop-body.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-splice-up--choice-1-inside-case-1 ()
 "Test `combobulate' with `fixtures/splice/choice-1-inside-case.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-1-inside-case.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-1-inside-case.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-splice-up--choice-1-switch-1 ()
 "Test `combobulate' with `fixtures/splice/choice-1-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-1-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/choice-1-switch.go[@1~after].go")))


(ert-deftest combobulate-test-typescript-combobulate-splice-up--console-1 ()
 "Test `combobulate' with `fixtures/splice/console.ts' in `typescript-ts-mode' mode."
	     (combobulate-test
		 (:language typescript :mode typescript-ts-mode :fixture "fixtures/splice/console.ts")
	       :tags
	       '(combobulate typescript typescript-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/console.ts[@1~after].ts")))


(ert-deftest combobulate-test-typescript-combobulate-splice-up--console-2 ()
 "Test `combobulate' with `fixtures/splice/console.ts' in `typescript-ts-mode' mode."
	     (combobulate-test
		 (:language typescript :mode typescript-ts-mode :fixture "fixtures/splice/console.ts")
	       :tags
	       '(combobulate typescript typescript-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/console.ts[@2~after].ts")))


(ert-deftest combobulate-test-python-combobulate-splice-up--messy-splice-1 ()
 "Test `combobulate' with `fixtures/splice/messy-splice.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/splice/messy-splice.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/messy-splice.py[@1~after].py")))


(ert-deftest combobulate-test-toml-combobulate-splice-up--pairs-in-table-1 ()
 "Test `combobulate' with `fixtures/splice/pairs-in-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/splice/pairs-in-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-splice-up)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0 0 0))
		 (combobulate-splice-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-up/pairs-in-table.toml[@1~after].toml")))


