;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-component-jsx-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 4)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/component-jsx.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-declaration-1
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-declaration-2
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-declaration-3
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-declaration-4
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 4)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-declaration-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-declaration-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-declaration-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-declaration-4 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-declaration.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-declaration.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-function-arg-1
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-function-arg.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-function-arg-2
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-function-arg.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-function-arg-3
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 3)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-function-arg.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-function-arg-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-function-arg.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-function-arg-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-function-arg.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-function-arg-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-function-arg.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-function-arg.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-nested-statements-1
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-nested-statements-2
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-down-css-nested-statements-3
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 3)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-nested-statements-1
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-nested-statements-2
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest
    combobulate-test-css-combobulate-drag-up-css-nested-statements-3
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-nested-statements.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-1
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-2
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-3
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode "combobulate-drag-down") (goto-marker 3)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-1
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-2
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-3
    ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture
                 "./fixtures/sibling/css-property.css")
    :tags '(css css-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 6) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-block.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 6) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-block.py")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-block-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-block-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-block-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-block-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-block-5
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 5)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-block-6
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 6)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-block-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-block-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-block-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-block-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-block-5
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-block-6
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-block.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 6)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-object-args-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-object-args-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-object-args-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 3)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-object-args-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-object-args-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-object-args-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-object-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-type-args-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-type-args-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-def-function-type-args-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 3)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-type-args-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-type-args-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-def-function-type-args-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/def-function-type-args.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 6) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-7
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 7) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 7
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-8
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 8) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 8
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-def-parameters-9
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 9) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 9
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 6) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-7
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 7) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 7
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-8
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 8) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 8
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-def-parameters-9
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/def-parameters.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 9) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 9
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/def-parameters.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 6) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-7
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 7) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 7
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-8
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 8) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 8
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-module-statements-9
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 9) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 9
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 6) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-7
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 7) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 7
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-8
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 8) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 8
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-module-statements-9
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/module-statements.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 9) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 9
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.py")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-module-statements-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-module-statements-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-module-statements-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-module-statements-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-down-module-statements-5
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-down") (goto-marker 5)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-module-statements-1
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-module-statements-2
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-module-statements-3
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-module-statements-4
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-tsx-combobulate-drag-up-module-statements-5
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/sibling/module-statements.tsx")
    :tags '(tsx tsx-ts-mode "combobulate-drag-up") (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/module-statements.tsx")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-nested-blocks-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-nested-blocks-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-nested-blocks-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-nested-blocks-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-nested-blocks-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-nested-blocks-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/nested-blocks.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/nested-blocks.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 6) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-dict-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 7) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 7
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 6) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-7
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-dict.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 7) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 7
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-dict.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-list-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-list-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-list-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-list-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-list-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-list.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-list.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-match-case-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-match-case.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-match-case-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-match-case.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-match-case-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-match-case.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-match-case-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-match-case.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-match-case-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-match-case.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-match-case-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-match-case.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-match-case.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-set-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-set.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-set-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-set.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-set-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-set.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-set.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-string-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-string-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-string-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-string-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-string-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-string-6
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 6) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-string-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-string-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-string-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-string-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-string-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-string-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-string.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 6) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 6
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-string.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-pattern-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-pattern-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-pattern-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-pattern-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-pattern-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-pattern-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-pattern-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-pattern-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-pattern-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-pattern-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple-pattern.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-1
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 1) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-2
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 2) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-3
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 3) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-4
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 4) (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-down-python-tuple-5
    ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-down")
    (goto-marker 5) (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 1) (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 2) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 3) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 4) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 4
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-python-combobulate-drag-up-python-tuple-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/sibling/python-tuple.py")
    :tags '(python python-ts-mode "combobulate-drag-up")
    (goto-marker 5) (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 5
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/python-tuple.py")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-pairs-1
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-pairs-2
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 2)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-pairs-1
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-pairs-2
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-1
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-2
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-3
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 3)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-1
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-2
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-3
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 3
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-sequence-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-sequence.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-sequence.yaml")))


(ert-deftest
    combobulate-test-yaml-combobulate-drag-down-yaml-sequence-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-sequence.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-down") (goto-marker 2)
    (should-error (combobulate-drag-down))
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-down"
                                                   "./fixtures/sibling/yaml-sequence.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-sequence-1
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-sequence.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 1)
    (should-error (combobulate-drag-up))
    (combobulate-compare-action-with-fixture-delta 1
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-sequence.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-sequence-2
    ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture
                 "./fixtures/sibling/yaml-sequence.yaml")
    :tags '(yaml yaml-ts-mode "combobulate-drag-up") (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-compare-action-with-fixture-delta 2
                                                   "combobulate-drag-up"
                                                   "./fixtures/sibling/yaml-sequence.yaml")))



