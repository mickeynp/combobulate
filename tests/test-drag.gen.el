;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-number-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-number-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/component-jsx.tsx")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-number-4 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-number-4 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/css-declaration.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-function-arg-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/css-function-arg.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-function-arg-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/css-function-arg.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-function-arg-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/css-function-arg.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-function-arg-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/css-function-arg.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-function-arg-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/css-function-arg.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-function-arg-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/css-function-arg.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-nested-statements-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-nested-statements-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-nested-statements-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-nested-statements-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-nested-statements-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-nested-statements-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/css-nested-statements.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-number-1 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-number-2 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-number-3 ()
  (combobulate-test
      (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    :tags
    '("css" "css-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/css-property.css")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 6)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-down "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 6)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-up "./fixtures/sibling/def-block.py")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-number-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-number-5 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-number-6 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 6)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-down "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-number-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-number-5 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-number-6 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 6)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-up "./fixtures/sibling/def-function-block.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-object-args-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-object-args-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-object-args-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-object-args-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-object-args-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-object-args-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/def-function-object-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-type-args-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-type-args-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-type-args-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-type-args-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-type-args-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-type-args-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/def-function-type-args.tsx")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 6)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 7)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 7 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-8 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 8)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 8 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-number-9 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 9)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 9 #'combobulate-drag-down "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 6)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 7)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 7 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-8 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 8)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 8 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-number-9 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 9)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 9 #'combobulate-drag-up "./fixtures/sibling/def-parameters.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 6)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 7)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 7 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-8 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 8)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 8 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-number-9 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 9)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 9 #'combobulate-drag-down "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 6)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 7)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 7 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-8 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 8)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 8 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-number-9 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 9)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 9 #'combobulate-drag-up "./fixtures/sibling/module-statements.py")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-number-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-number-5 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-number-1 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-number-2 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-number-3 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-number-4 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-number-5 ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    :tags
    '("tsx" "tsx-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/module-statements.tsx")))


(ert-deftest combobulate-test-python-combobulate-drag-down-nested-blocks-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/nested-blocks.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-nested-blocks-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/nested-blocks.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-nested-blocks-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/nested-blocks.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-nested-blocks-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/nested-blocks.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-nested-blocks-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/nested-blocks.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-nested-blocks-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/nested-blocks.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 6)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-number-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 7)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 7 #'combobulate-drag-down "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 6)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-number-7 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 7)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 7 #'combobulate-drag-up "./fixtures/sibling/python-dict.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/python-list.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-match-case-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-match-case.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-match-case-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-match-case.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-match-case-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-match-case.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-match-case-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-match-case.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-match-case-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-match-case.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-match-case-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-match-case.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-set-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-set-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-set-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-set.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 6)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-down "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-number-6 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 6)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 6 #'combobulate-drag-up "./fixtures/sibling/python-string.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/python-tuple-pattern.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 4)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-down "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-down")
    (goto-marker 5)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-down "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-number-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-number-2 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-number-3 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-number-4 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 4)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 4 #'combobulate-drag-up "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-number-5 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    :tags
    '("python" "python-ts-mode" "combobulate-drag-up")
    (goto-marker 5)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 5 #'combobulate-drag-up "./fixtures/sibling/python-tuple.py")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-pairs-number-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-pairs-number-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-pairs-number-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-pairs-number-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/yaml-block-mapping-pairs.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-number-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-number-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-number-3 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 3)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-down "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-number-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-number-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-number-3 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 3)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 3 #'combobulate-drag-up "./fixtures/sibling/yaml-block-mapping.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-sequence-number-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 1)
    (combobulate-drag-down)
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-down "./fixtures/sibling/yaml-sequence.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-sequence-number-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-down")
    (goto-marker 2)
    (should-error
     (combobulate-drag-down))
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-down "./fixtures/sibling/yaml-sequence.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-sequence-number-1 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 1)
    (should-error
     (combobulate-drag-up))
    (combobulate-test-fixture-action-function 1 #'combobulate-drag-up "./fixtures/sibling/yaml-sequence.yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-sequence-number-2 ()
  (combobulate-test
      (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
    :tags
    '("yaml" "yaml-ts-mode" "combobulate-drag-up")
    (goto-marker 2)
    (combobulate-drag-up)
    (combobulate-test-fixture-action-function 2 #'combobulate-drag-up "./fixtures/sibling/yaml-sequence.yaml")))


