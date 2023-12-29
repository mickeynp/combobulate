;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-tsx-combobulate-navigate-previous-component-jsx
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-previous-css-declaration
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-previous-css-function-arg
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-previous-css-nested-statements
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-css-combobulate-navigate-previous-css-property
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-def-block
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-previous-def-function-block
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-previous-def-function-object-args
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-previous-def-function-type-args
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-def-parameters
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-module-statements
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-nested-blocks
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-dict
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-list
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-match-case
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-set
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-string
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-tuple-pattern
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-python-combobulate-navigate-previous-python-tuple
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-yaml-combobulate-navigate-previous-yaml-block-mapping-pairs
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags '(yaml yaml-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-yaml-combobulate-navigate-previous-yaml-block-mapping
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


(ert-deftest
    combobulate-test-yaml-combobulate-navigate-previous-yaml-sequence
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-sequence.yaml")
    :tags '(yaml yaml-ts-mode combobulate-navigate-previous)
    (combobulate-for-each-marker #'combobulate-navigate-previous
                                 :reverse t)))


