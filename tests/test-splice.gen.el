;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-1 ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `1'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 1) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/component-jsx.tsx[combobulate-splice-up@1~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-2 ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `2'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 2) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/component-jsx.tsx[combobulate-splice-up@2~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-3 ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `3'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 3) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/component-jsx.tsx[combobulate-splice-up@3~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-up-component-jsx-4 ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `4'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 4) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/component-jsx.tsx[combobulate-splice-up@4~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-1
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `1'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 1) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/component-jsx.tsx[combobulate-splice-down@1~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-2
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `2'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 2) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/component-jsx.tsx[combobulate-splice-down@2~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-3
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `3'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 3) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/component-jsx.tsx[combobulate-splice-down@3~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-splice-down-component-jsx-4
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `4'."
  :tags '(tsx tsx-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 4) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/component-jsx.tsx[combobulate-splice-down@4~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-1
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/component-jsx.tsx' at point marker number `1'."
  :tags '(tsx tsx-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 1) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/component-jsx.tsx[combobulate-vanish-node@1~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-2
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/component-jsx.tsx' at point marker number `2'."
  :tags '(tsx tsx-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 2) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/component-jsx.tsx[combobulate-vanish-node@2~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-3
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/component-jsx.tsx' at point marker number `3'."
  :tags '(tsx tsx-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 3) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/component-jsx.tsx[combobulate-vanish-node@3~after].tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-vanish-node-component-jsx-4
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/component-jsx.tsx' at point marker number `4'."
  :tags '(tsx tsx-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    (goto-marker 4) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/component-jsx.tsx[combobulate-vanish-node@4~after].tsx")))


(ert-deftest
    combobulate-test-python-combobulate-splice-up-nested-blocks-1
    ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/nested-blocks.py' at point marker number `1'."
  :tags '(python python-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 1) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/nested-blocks.py[combobulate-splice-up@1~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-up-nested-blocks-2
    ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/nested-blocks.py' at point marker number `2'."
  :tags '(python python-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 2) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/nested-blocks.py[combobulate-splice-up@2~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-up-nested-blocks-3
    ()
  "Test `combobulate-splice-up' on `./fixtures/sibling/nested-blocks.py' at point marker number `3'."
  :tags '(python python-ts-mode "combobulate-splice-up")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 3) (combobulate-splice-up)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-up/nested-blocks.py[combobulate-splice-up@3~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-down-nested-blocks-1
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/nested-blocks.py' at point marker number `1'."
  :tags '(python python-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 1) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/nested-blocks.py[combobulate-splice-down@1~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-down-nested-blocks-2
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/nested-blocks.py' at point marker number `2'."
  :tags '(python python-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 2) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/nested-blocks.py[combobulate-splice-down@2~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-splice-down-nested-blocks-3
    ()
  "Test `combobulate-splice-down' on `./fixtures/sibling/nested-blocks.py' at point marker number `3'."
  :tags '(python python-ts-mode "combobulate-splice-down")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 3) (combobulate-splice-down)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-splice-down/nested-blocks.py[combobulate-splice-down@3~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-vanish-node-nested-blocks-1
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/nested-blocks.py' at point marker number `1'."
  :tags '(python python-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 1) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/nested-blocks.py[combobulate-vanish-node@1~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-vanish-node-nested-blocks-2
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/nested-blocks.py' at point marker number `2'."
  :tags '(python python-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 2) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/nested-blocks.py[combobulate-vanish-node@2~after].py")))


(ert-deftest
    combobulate-test-python-combobulate-vanish-node-nested-blocks-3
    ()
  "Test `combobulate-vanish-node' on `./fixtures/sibling/nested-blocks.py' at point marker number `3'."
  :tags '(python python-ts-mode "combobulate-vanish-node")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    (goto-marker 3) (combobulate-vanish-node)
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/combobulate-vanish-node/nested-blocks.py[combobulate-vanish-node@3~after].py")))


