;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-number-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1
                                              #'combobulate-drag-down
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-number-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2
                                              #'combobulate-drag-down
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-number-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3
                                              #'combobulate-drag-down
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-number-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 4) (should-error (combobulate-drag-down))
    (combobulate-test-fixture-action-function 4
                                              #'combobulate-drag-down
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-component-jsx-number-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-component-jsx-number-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-component-jsx-number-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up
                                              "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-component-jsx-number-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '("tsx" "tsx-ts-mode" "combobulate-drag-up") (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up
                                              "./fixtures/sibling/component-jsx.tsx")))


