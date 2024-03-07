;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-html-combobulate-kill-dwim--attributes-1 ()
  "Test `combobulate' with `fixtures/kill-node/attributes.html' in `html-ts-mode' mode."
  (combobulate-test
      (:language html :mode html-ts-mode :fixture "fixtures/kill-node/attributes.html")
    :tags
    '(combobulate html html-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/attributes.html[@1~after].html")))


(ert-deftest combobulate-test-go-combobulate-kill-dwim--block-1 ()
  "Test `combobulate' with `fixtures/kill-node/block.go' in `go-ts-mode' mode."
  (combobulate-test
      (:language go :mode go-ts-mode :fixture "fixtures/kill-node/block.go")
    :tags
    '(combobulate go go-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/block.go[@1~after].go")))


(ert-deftest combobulate-test-tsx-combobulate-kill-dwim--component-jsx-1 ()
  "Test `combobulate' with `fixtures/kill-node/component-jsx.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "fixtures/kill-node/component-jsx.tsx")
    :tags
    '(combobulate tsx tsx-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/component-jsx.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-css-combobulate-kill-dwim--css-declaration-1 ()
  "Test `combobulate' with `fixtures/kill-node/css-declaration.css' in `css-ts-mode' mode."
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "fixtures/kill-node/css-declaration.css")
    :tags
    '(combobulate css css-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/css-declaration.css[@1~after].css")))


(ert-deftest combobulate-test-python-combobulate-kill-dwim--python-dict-1 ()
  "Test `combobulate' with `fixtures/kill-node/python-dict.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "fixtures/kill-node/python-dict.py")
    :tags
    '(combobulate python python-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/python-dict.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-kill-dwim--python-match-case-1 ()
  "Test `combobulate' with `fixtures/kill-node/python-match-case.py' in `python-ts-mode' mode."
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "fixtures/kill-node/python-match-case.py")
    :tags
    '(combobulate python python-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/python-match-case.py[@1~after].py")))


(ert-deftest combobulate-test-yaml-combobulate-kill-dwim--yaml-block-mapping-pairs-1 ()
  "Test `combobulate' with `fixtures/kill-node/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "fixtures/kill-node/yaml-block-mapping-pairs.yaml")
    :tags
    '(combobulate yaml yaml-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/yaml-block-mapping-pairs.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-kill-dwim--yaml-sequence-1 ()
  "Test `combobulate' with `fixtures/kill-node/yaml-sequence.yaml' in `yaml-ts-mode' mode."
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "fixtures/kill-node/yaml-sequence.yaml")
    :tags
    '(combobulate yaml yaml-ts-mode combobulate-kill-dwim)
    (combobulate-test-go-to-marker 1)
    (combobulate--with-test-overlays
     (lambda
       (ov)
       (combobulate-kill-node-dwim)))
    (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-kill-dwim/yaml-sequence.yaml[@1~after].yaml")))


