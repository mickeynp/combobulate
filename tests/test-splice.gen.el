;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-up") (goto-marker 1)
    (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-up") (goto-marker 2)
    (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-up") (goto-marker 3)
    (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-up") (goto-marker 4)
    (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-down") (goto-marker 1)
    (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-down") (goto-marker 2)
    (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-down") (goto-marker 3)
    (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-splice-down") (goto-marker 4)
    (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-vanish-node") (goto-marker 1)
    (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-vanish-node") (goto-marker 2)
    (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-vanish-node") (goto-marker 3)
    (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-vanish-node") (goto-marker 4)
    (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-python-combobulate-splice-up-nested-blocks-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-splice-up")
    (goto-marker 1) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-up-nested-blocks-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-splice-up")
    (goto-marker 2) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-up-nested-blocks-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-splice-up")
    (goto-marker 3) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-splice-up"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-down-nested-blocks-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-splice-down")
    (goto-marker 1) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-down-nested-blocks-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-splice-down")
    (goto-marker 2) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-down-nested-blocks-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-splice-down")
    (goto-marker 3) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-splice-down"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-vanish-node-nested-blocks-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-vanish-node")
    (goto-marker 1) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-vanish-node-nested-blocks-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-vanish-node")
    (goto-marker 2) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-vanish-node-nested-blocks-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-vanish-node")
    (goto-marker 3) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-vanish-node"
                                                   "./fixtures/sibling/nested-blocks.py")))


