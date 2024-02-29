;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-html-combobulate-clone-dwim--attributes-1 ()
 "Test `combobulate' with `fixtures/clone/attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/clone/attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/attributes.html[@1~after].html")))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--component-jsx-1 ()
 "Test `combobulate' with `fixtures/clone/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 1)
		  (combobulate-with-stubbed-proffer-choices
		      (:choices
		       '(0 0 0 0))
		    (combobulate-clone-node-dwim))
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/component-jsx.tsx[@1~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--component-jsx-2 ()
 "Test `combobulate' with `fixtures/clone/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-with-stubbed-proffer-choices
		      (:choices
		       '(0 0 0 0))
		    (combobulate-clone-node-dwim))
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/component-jsx.tsx[@2~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--component-jsx-3 ()
 "Test `combobulate' with `fixtures/clone/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-with-stubbed-proffer-choices
		      (:choices
		       '(0 0 0 0))
		    (combobulate-clone-node-dwim))
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/component-jsx.tsx[@3~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--component-jsx-4 ()
 "Test `combobulate' with `fixtures/clone/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 4)
		  (combobulate-with-stubbed-proffer-choices
		      (:choices
		       '(0 0 0 0))
		    (combobulate-clone-node-dwim))
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/component-jsx.tsx[@4~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--component-jsx-5 ()
 "Test `combobulate' with `fixtures/clone/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-with-stubbed-proffer-choices
		      (:choices
		       '(0 0 0 0))
		    (combobulate-clone-node-dwim))
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/component-jsx.tsx[@5~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--component-jsx-6 ()
 "Test `combobulate' with `fixtures/clone/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/component-jsx.tsx[@6~after].tsx")))


(ert-deftest combobulate-test-css-combobulate-clone-dwim--css-declaration-1 ()
 "Test `combobulate' with `fixtures/clone/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/clone/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/css-declaration.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-clone-dwim--css-function-arg-1 ()
 "Test `combobulate' with `fixtures/clone/css-function-arg.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/clone/css-function-arg.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/css-function-arg.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-clone-dwim--css-nested-statements-1 ()
 "Test `combobulate' with `fixtures/clone/css-nested-statements.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/clone/css-nested-statements.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/css-nested-statements.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-clone-dwim--css-property-1 ()
 "Test `combobulate' with `fixtures/clone/css-property.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/clone/css-property.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/css-property.css[@1~after].css")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--def-block-1 ()
 "Test `combobulate' with `fixtures/clone/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/def-block.py[@1~after].py")))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--def-function-block-1 ()
 "Test `combobulate' with `fixtures/clone/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/def-function-block.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--def-function-object-args-1 ()
 "Test `combobulate' with `fixtures/clone/def-function-object-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/def-function-object-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/def-function-object-args.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-html-combobulate-clone-dwim--elements-1 ()
 "Test `combobulate' with `fixtures/clone/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/clone/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/elements.html[@1~after].html")))


(ert-deftest combobulate-test-html-combobulate-clone-dwim--elements-2 ()
 "Test `combobulate' with `fixtures/clone/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/clone/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/elements.html[@2~after].html")))


(ert-deftest combobulate-test-html-combobulate-clone-dwim--elements-3 ()
 "Test `combobulate' with `fixtures/clone/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/clone/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/elements.html[@3~after].html")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--module-statements-1 ()
 "Test `combobulate' with `fixtures/clone/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/module-statements.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--module-statements-2 ()
 "Test `combobulate' with `fixtures/clone/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/module-statements.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--module-statements-3 ()
 "Test `combobulate' with `fixtures/clone/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/module-statements.py[@3~after].py")))


(ert-deftest combobulate-test-tsx-combobulate-clone-dwim--module-statements-1 ()
 "Test `combobulate' with `fixtures/clone/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/clone/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/module-statements.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--nested-blocks-1 ()
 "Test `combobulate' with `fixtures/clone/nested-blocks.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/nested-blocks.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/nested-blocks.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-dict-1 ()
 "Test `combobulate' with `fixtures/clone/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-dict.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-dict-2 ()
 "Test `combobulate' with `fixtures/clone/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-dict.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-list-1 ()
 "Test `combobulate' with `fixtures/clone/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-list.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-list-2 ()
 "Test `combobulate' with `fixtures/clone/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-list.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-list-3 ()
 "Test `combobulate' with `fixtures/clone/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-list.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-list-4 ()
 "Test `combobulate' with `fixtures/clone/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-list.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-clone-dwim--python-list-5 ()
 "Test `combobulate' with `fixtures/clone/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/clone/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/python-list.py[@5~after].py")))


(ert-deftest combobulate-test-yaml-combobulate-clone-dwim--yaml-block-mapping-pairs-1 ()
 "Test `combobulate' with `fixtures/clone/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/clone/yaml-block-mapping-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/yaml-block-mapping-pairs.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-clone-dwim--yaml-block-mapping-pairs-2 ()
 "Test `combobulate' with `fixtures/clone/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/clone/yaml-block-mapping-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/yaml-block-mapping-pairs.yaml[@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-clone-dwim--yaml-sequence-1 ()
 "Test `combobulate' with `fixtures/clone/yaml-sequence.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/clone/yaml-sequence.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-clone-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-clone-node-dwim))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/yaml-sequence.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-clone-dwim--yaml-sequence-2 ()
 "Test `combobulate' with `fixtures/clone/yaml-sequence.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/clone/yaml-sequence.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-clone-dwim)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-with-stubbed-proffer-choices
		      (:choices
		       '(0 0 0 0))
		    (combobulate-clone-node-dwim))
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-clone-dwim/yaml-sequence.yaml[@2~after].yaml")))))


