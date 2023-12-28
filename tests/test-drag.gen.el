;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[combobulate-drag-down@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[combobulate-drag-down@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[combobulate-drag-down@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-component-jsx-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/component-jsx.tsx' at point marker number `4'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 4)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[combobulate-drag-down@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/component-jsx.tsx[combobulate-drag-up@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/component-jsx.tsx[combobulate-drag-up@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/component-jsx.tsx[combobulate-drag-up@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-component-jsx-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/component-jsx.tsx' at point marker number `4'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/component-jsx.tsx[combobulate-drag-up@4~after].tsx")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-declaration.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[combobulate-drag-down@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-declaration.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[combobulate-drag-down@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-declaration.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[combobulate-drag-down@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-declaration-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-declaration.css' at point marker number `4'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 4)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[combobulate-drag-down@4~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-declaration.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-declaration.css[combobulate-drag-up@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-declaration.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-declaration.css[combobulate-drag-up@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-declaration.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-declaration.css[combobulate-drag-up@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-declaration-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-declaration.css' at point marker number `4'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-declaration.css[combobulate-drag-up@4~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-function-arg-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-function-arg.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-function-arg.css[combobulate-drag-down@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-function-arg-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-function-arg.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-function-arg.css[combobulate-drag-down@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-function-arg-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-function-arg.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-function-arg.css[combobulate-drag-down@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-function-arg-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-function-arg.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-function-arg.css[combobulate-drag-up@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-function-arg-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-function-arg.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-function-arg.css[combobulate-drag-up@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-function-arg-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-function-arg.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-function-arg.css[combobulate-drag-up@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-nested-statements-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-nested-statements.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-nested-statements.css[combobulate-drag-down@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-nested-statements-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-nested-statements.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-nested-statements.css[combobulate-drag-down@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-nested-statements-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-nested-statements.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-nested-statements.css[combobulate-drag-down@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-nested-statements-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-nested-statements.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-nested-statements.css[combobulate-drag-up@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-nested-statements-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-nested-statements.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-nested-statements.css[combobulate-drag-up@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-nested-statements-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-nested-statements.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-nested-statements.css[combobulate-drag-up@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-property.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-property.css[combobulate-drag-down@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-property.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-property.css[combobulate-drag-down@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down-css-property-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/css-property.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-property.css[combobulate-drag-down@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-property.css' at point marker number `1'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-property.css[combobulate-drag-up@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-property.css' at point marker number `2'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-property.css[combobulate-drag-up@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-up-css-property-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/css-property.css' at point marker number `3'." :tags
	     '(css css-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/css-property.css[combobulate-drag-up@3~after].css")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-block.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-block.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-block.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-block.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-block.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-block-6 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-block.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 6)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[combobulate-drag-down@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-block.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-block.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-block.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-block.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-block.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-block.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-block.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-block.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-block.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-block.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-block-6 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-block.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
	       (goto-marker 6)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-block.py[combobulate-drag-up@6~after].py")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-block.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[combobulate-drag-down@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-block.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[combobulate-drag-down@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-block.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[combobulate-drag-down@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-block.tsx' at point marker number `4'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[combobulate-drag-down@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-block.tsx' at point marker number `5'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[combobulate-drag-down@5~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-block-6 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-block.tsx' at point marker number `6'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 6)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[combobulate-drag-down@6~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-block.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-block.tsx[combobulate-drag-up@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-block.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-block.tsx[combobulate-drag-up@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-block.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-block.tsx[combobulate-drag-up@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-block.tsx' at point marker number `4'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-block.tsx[combobulate-drag-up@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-block.tsx' at point marker number `5'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-block.tsx[combobulate-drag-up@5~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-block-6 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-block.tsx' at point marker number `6'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
	       (goto-marker 6)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-block.tsx[combobulate-drag-up@6~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-object-args-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-object-args.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-object-args.tsx[combobulate-drag-down@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-object-args-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-object-args.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-object-args.tsx[combobulate-drag-down@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-object-args-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-object-args.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-object-args.tsx[combobulate-drag-down@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-object-args-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-object-args.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-object-args.tsx[combobulate-drag-up@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-object-args-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-object-args.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-object-args.tsx[combobulate-drag-up@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-object-args-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-object-args.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-object-args.tsx[combobulate-drag-up@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-type-args-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-type-args.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-type-args.tsx[combobulate-drag-down@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-type-args-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-type-args.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-type-args.tsx[combobulate-drag-down@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-def-function-type-args-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-function-type-args.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-type-args.tsx[combobulate-drag-down@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-type-args-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-type-args.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-type-args.tsx[combobulate-drag-up@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-type-args-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-type-args.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-type-args.tsx[combobulate-drag-up@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-def-function-type-args-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-function-type-args.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-function-type-args.tsx[combobulate-drag-up@3~after].tsx")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-6 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-7 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `7'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-8 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `8'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@8~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-def-parameters-9 () "Test `combobulate-drag-down' on `./fixtures/sibling/def-parameters.py' at point marker number `9'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 9)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[combobulate-drag-down@9~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-6 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 6)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-7 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `7'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 7)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-8 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `8'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 8)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@8~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-def-parameters-9 () "Test `combobulate-drag-up' on `./fixtures/sibling/def-parameters.py' at point marker number `9'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
	       (goto-marker 9)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/def-parameters.py[combobulate-drag-up@9~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-6 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-7 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `7'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-8 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `8'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@8~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-module-statements-9 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.py' at point marker number `9'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 9)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[combobulate-drag-down@9~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 1)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-6 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 6)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-7 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `7'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 7)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-8 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `8'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 8)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@8~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-module-statements-9 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.py' at point marker number `9'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
	       (goto-marker 9)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.py[combobulate-drag-up@9~after].py")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[combobulate-drag-down@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[combobulate-drag-down@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[combobulate-drag-down@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.tsx' at point marker number `4'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[combobulate-drag-down@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down-module-statements-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/module-statements.tsx' at point marker number `5'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 5)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[combobulate-drag-down@5~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.tsx' at point marker number `1'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.tsx[combobulate-drag-up@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.tsx' at point marker number `2'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.tsx[combobulate-drag-up@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.tsx' at point marker number `3'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.tsx[combobulate-drag-up@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.tsx' at point marker number `4'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.tsx[combobulate-drag-up@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-up-module-statements-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/module-statements.tsx' at point marker number `5'." :tags
	     '(tsx tsx-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/module-statements.tsx[combobulate-drag-up@5~after].tsx")))


(ert-deftest combobulate-test-python-combobulate-drag-down-nested-blocks-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/nested-blocks.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested-blocks.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-nested-blocks-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/nested-blocks.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested-blocks.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-nested-blocks-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/nested-blocks.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested-blocks.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-nested-blocks-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/nested-blocks.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/nested-blocks.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-nested-blocks-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/nested-blocks.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/nested-blocks.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-nested-blocks-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/nested-blocks.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/nested-blocks.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/nested-blocks.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-6 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-dict-7 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-dict.py' at point marker number `7'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 7)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[combobulate-drag-down@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-6 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 6)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-dict-7 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-dict.py' at point marker number `7'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
	       (goto-marker 7)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-dict.py[combobulate-drag-up@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-list.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-list.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-list.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-list.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-list-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-list.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 5)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-list.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-list.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-list.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-list.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-list.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-list.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-list.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-list.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-list-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-list.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-list.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-match-case-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-match-case.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-match-case.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-match-case-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-match-case.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-match-case.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-match-case-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-match-case.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-match-case.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-match-case-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-match-case.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-match-case.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-match-case-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-match-case.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-match-case.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-match-case-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-match-case.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-match-case.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-set-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-set.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-set.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-set-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-set.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-set.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-set-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-set.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-set.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-set.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-set.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-set.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-set.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-set-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-set.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-set.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-string.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-string.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-string.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-string.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-string.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 5)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-string-6 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-string.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 6)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[combobulate-drag-down@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-string.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-string.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-string.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-string.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-string.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-string.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-string.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-string.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-string.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 5)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-string.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-string-6 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-string.py' at point marker number `6'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-string.py")
	       (goto-marker 6)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-string.py[combobulate-drag-up@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-pattern-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 5)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple-pattern.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple-pattern.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple-pattern.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple-pattern.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-pattern-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple-pattern.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple-pattern.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[combobulate-drag-down@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[combobulate-drag-down@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[combobulate-drag-down@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-4 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[combobulate-drag-down@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down-python-tuple-5 () "Test `combobulate-drag-down' on `./fixtures/sibling/python-tuple.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 5)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[combobulate-drag-down@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple.py' at point marker number `1'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple.py[combobulate-drag-up@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple.py' at point marker number `2'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple.py[combobulate-drag-up@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple.py' at point marker number `3'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple.py[combobulate-drag-up@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-4 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple.py' at point marker number `4'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 4)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple.py[combobulate-drag-up@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-up-python-tuple-5 () "Test `combobulate-drag-up' on `./fixtures/sibling/python-tuple.py' at point marker number `5'." :tags
	     '(python python-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
	       (goto-marker 5)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/python-tuple.py[combobulate-drag-up@5~after].py")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-pairs-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-block-mapping-pairs.yaml' at point marker number `1'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping-pairs.yaml[combobulate-drag-down@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-pairs-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-block-mapping-pairs.yaml' at point marker number `2'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       (goto-marker 2)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping-pairs.yaml[combobulate-drag-down@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-pairs-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-block-mapping-pairs.yaml' at point marker number `1'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-block-mapping-pairs.yaml[combobulate-drag-up@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-pairs-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-block-mapping-pairs.yaml' at point marker number `2'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-block-mapping-pairs.yaml[combobulate-drag-up@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-block-mapping.yaml' at point marker number `1'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping.yaml[combobulate-drag-down@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-block-mapping.yaml' at point marker number `2'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
	       (goto-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping.yaml[combobulate-drag-down@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-block-mapping-3 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-block-mapping.yaml' at point marker number `3'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
	       (goto-marker 3)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping.yaml[combobulate-drag-down@3~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-block-mapping.yaml' at point marker number `1'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-block-mapping.yaml[combobulate-drag-up@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-block-mapping.yaml' at point marker number `2'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-block-mapping.yaml[combobulate-drag-up@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-block-mapping-3 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-block-mapping.yaml' at point marker number `3'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
	       (goto-marker 3)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-block-mapping.yaml[combobulate-drag-up@3~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-sequence-1 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-sequence.yaml' at point marker number `1'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
	       (goto-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-sequence.yaml[combobulate-drag-down@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down-yaml-sequence-2 () "Test `combobulate-drag-down' on `./fixtures/sibling/yaml-sequence.yaml' at point marker number `2'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-down")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
	       (goto-marker 2)
	       (should-error
		(combobulate-drag-down))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-sequence.yaml[combobulate-drag-down@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-sequence-1 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-sequence.yaml' at point marker number `1'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
	       (goto-marker 1)
	       (should-error
		(combobulate-drag-up))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-sequence.yaml[combobulate-drag-up@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-up-yaml-sequence-2 () "Test `combobulate-drag-up' on `./fixtures/sibling/yaml-sequence.yaml' at point marker number `2'." :tags
	     '(yaml yaml-ts-mode "combobulate-drag-up")
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
	       (goto-marker 2)
	       (combobulate-drag-up)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-up/yaml-sequence.yaml[combobulate-drag-up@2~after].yaml")))


