;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-env-1 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-env' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-env")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-env[@1~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-env-2 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-env' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-env")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-env[@2~after].")))))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-1 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@1~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-2 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@2~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-3 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@3~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-4 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@4~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-5 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@5~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-6 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@6~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-7 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@7~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-8 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@8~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-9 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@9~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-instructions-10 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-instructions' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-instructions")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 10)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-instructions[@10~after].")))))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-label-1 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-label' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-label")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-label[@1~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-label-2 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-label' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-label")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-label[@2~after].")))))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-shell_fragment-1 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-shell_fragment' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-shell_fragment")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-shell_fragment[@1~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-shell_fragment-2 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-shell_fragment' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-shell_fragment")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-shell_fragment[@2~after].")))


(ert-deftest combobulate-test-dockerfile-combobulate-drag-down--Dockerfile-shell_fragment-3 ()
 "Test `combobulate' with `fixtures/sibling/Dockerfile-shell_fragment' in `dockerfile-ts-mode' mode."
	     (combobulate-test
		 (:language dockerfile :mode dockerfile-ts-mode :fixture "fixtures/sibling/Dockerfile-shell_fragment")
	       :tags
	       '(combobulate dockerfile dockerfile-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/Dockerfile-shell_fragment[@3~after].")))))


(ert-deftest combobulate-test-html-combobulate-drag-down--attributes-1 ()
 "Test `combobulate' with `fixtures/sibling/attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/attributes.html[@1~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--attributes-2 ()
 "Test `combobulate' with `fixtures/sibling/attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/attributes.html[@2~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--attributes-3 ()
 "Test `combobulate' with `fixtures/sibling/attributes.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/attributes.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/attributes.html[@3~after].html")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--block-1 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/block.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--block-2 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/block.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--block-3 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/block.go[@3~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--block-4 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/block.go[@4~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--block-5 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/block.go[@5~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--block-6 ()
 "Test `combobulate' with `fixtures/sibling/block.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/block.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/block.go[@6~after].go")))))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--component-jsx-1 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--component-jsx-2 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--component-jsx-3 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--component-jsx-4 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--component-jsx-5 ()
 "Test `combobulate' with `fixtures/sibling/component-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/component-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/component-jsx.tsx[@5~after].tsx")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--const-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/const-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/const-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/const-declaration.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--const-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/const-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/const-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/const-declaration.go[@2~after].go")))))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-declaration-3 ()
 "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[@3~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-declaration-4 ()
 "Test `combobulate' with `fixtures/sibling/css-declaration.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-declaration.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 4)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-declaration.css[@4~after].css")))))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-function-arg-1 ()
 "Test `combobulate' with `fixtures/sibling/css-function-arg.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-function-arg.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-function-arg.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-function-arg-2 ()
 "Test `combobulate' with `fixtures/sibling/css-function-arg.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-function-arg.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-function-arg.css[@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-function-arg-3 ()
 "Test `combobulate' with `fixtures/sibling/css-function-arg.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-function-arg.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-function-arg.css[@3~after].css")))))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-nested-statements-1 ()
 "Test `combobulate' with `fixtures/sibling/css-nested-statements.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-nested-statements.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-nested-statements.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-nested-statements-2 ()
 "Test `combobulate' with `fixtures/sibling/css-nested-statements.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-nested-statements.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-nested-statements.css[@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-nested-statements-3 ()
 "Test `combobulate' with `fixtures/sibling/css-nested-statements.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-nested-statements.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-nested-statements.css[@3~after].css")))))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-property-1 ()
 "Test `combobulate' with `fixtures/sibling/css-property.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-property.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-property.css[@1~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-property-2 ()
 "Test `combobulate' with `fixtures/sibling/css-property.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-property.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-property.css[@2~after].css")))


(ert-deftest combobulate-test-css-combobulate-drag-down--css-property-3 ()
 "Test `combobulate' with `fixtures/sibling/css-property.css' in `css-ts-mode' mode."
	     (combobulate-test
		 (:language css :mode css-ts-mode :fixture "fixtures/sibling/css-property.css")
	       :tags
	       '(combobulate css css-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/css-property.css[@3~after].css")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-block-1 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-block-2 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-block-3 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-block-4 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-block-5 ()
 "Test `combobulate' with `fixtures/sibling/def-block.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-block.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-block.py[@5~after].py")))))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-block-1 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-block-2 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-block-3 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-block-4 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-block-5 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[@5~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-block-6 ()
 "Test `combobulate' with `fixtures/sibling/def-function-block.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-block.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-block.tsx[@6~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-object-args-1 ()
 "Test `combobulate' with `fixtures/sibling/def-function-object-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-object-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-object-args.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-object-args-2 ()
 "Test `combobulate' with `fixtures/sibling/def-function-object-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-object-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-object-args.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-object-args-3 ()
 "Test `combobulate' with `fixtures/sibling/def-function-object-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-object-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-object-args.tsx[@3~after].tsx")))))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-type-args-1 ()
 "Test `combobulate' with `fixtures/sibling/def-function-type-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-type-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-type-args.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-type-args-2 ()
 "Test `combobulate' with `fixtures/sibling/def-function-type-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-type-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-type-args.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--def-function-type-args-3 ()
 "Test `combobulate' with `fixtures/sibling/def-function-type-args.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/def-function-type-args.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-function-type-args.tsx[@3~after].tsx")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-1 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-2 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-3 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-4 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-5 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-6 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-7 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-8 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@8~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--def-parameters-9 ()
 "Test `combobulate' with `fixtures/sibling/def-parameters.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/def-parameters.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 9)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/def-parameters.py[@9~after].py")))))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-1 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@1~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-2 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@2~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-3 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@3~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-4 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@4~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-5 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@5~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-6 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@6~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--document-level-7 ()
 "Test `combobulate' with `fixtures/sibling/document-level.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/document-level.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 7)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/document-level.toml[@7~after].toml")))))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-1 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@1~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-2 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@2~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-3 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@3~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-4 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@4~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-5 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@5~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-6 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@6~after].html")))


(ert-deftest combobulate-test-html-combobulate-drag-down--elements-7 ()
 "Test `combobulate' with `fixtures/sibling/elements.html' in `html-ts-mode' mode."
	     (combobulate-test
		 (:language html :mode html-ts-mode :fixture "fixtures/sibling/elements.html")
	       :tags
	       '(combobulate html html-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 7)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/elements.html[@7~after].html")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--expr-switch-1 ()
 "Test `combobulate' with `fixtures/sibling/expr-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/expr-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/expr-switch.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--expr-switch-2 ()
 "Test `combobulate' with `fixtures/sibling/expr-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/expr-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/expr-switch.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--expr-switch-3 ()
 "Test `combobulate' with `fixtures/sibling/expr-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/expr-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/expr-switch.go[@3~after].go")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--for-loop-1 ()
 "Test `combobulate' with `fixtures/sibling/for-loop.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/for-loop.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/for-loop.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--for-loop-2 ()
 "Test `combobulate' with `fixtures/sibling/for-loop.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/for-loop.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/for-loop.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--for-loop-3 ()
 "Test `combobulate' with `fixtures/sibling/for-loop.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/for-loop.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/for-loop.go[@3~after].go")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--import-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/import-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/import-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/import-declaration.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--import-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/import-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/import-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/import-declaration.go[@2~after].go")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--inside-case-1 ()
 "Test `combobulate' with `fixtures/sibling/inside-case.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/inside-case.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/inside-case.go[@1~after].go")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--inside-table-1 ()
 "Test `combobulate' with `fixtures/sibling/inside-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/inside-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/inside-table.toml[@1~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--inside-table-2 ()
 "Test `combobulate' with `fixtures/sibling/inside-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/inside-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/inside-table.toml[@2~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--inside-table-3 ()
 "Test `combobulate' with `fixtures/sibling/inside-table.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/inside-table.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/inside-table.toml[@3~after].toml")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--literal-1 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/literal.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--literal-2 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/literal.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--literal-3 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/literal.go[@3~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--literal-4 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/literal.go[@4~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--literal-5 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/literal.go[@5~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--literal-6 ()
 "Test `combobulate' with `fixtures/sibling/literal.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/literal.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/literal.go[@6~after].go")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--map-1 ()
 "Test `combobulate' with `fixtures/sibling/map.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/map.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/map.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--map-2 ()
 "Test `combobulate' with `fixtures/sibling/map.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/map.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/map.go[@2~after].go")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-1 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-2 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-3 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-4 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-5 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-6 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-7 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@7~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--module-statements-8 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/module-statements.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 8)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.py[@8~after].py")))))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--module-statements-1 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[@1~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--module-statements-2 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[@2~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--module-statements-3 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[@3~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--module-statements-4 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[@4~after].tsx")))


(ert-deftest combobulate-test-tsx-combobulate-drag-down--module-statements-5 ()
 "Test `combobulate' with `fixtures/sibling/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/sibling/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module-statements.tsx[@5~after].tsx")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--module-1 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--module-2 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--module-3 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module.go[@3~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--module-4 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module.go[@4~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--module-5 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module.go[@5~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--module-6 ()
 "Test `combobulate' with `fixtures/sibling/module.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/module.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module.go[@6~after].go")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--nested-blocks-1 ()
 "Test `combobulate' with `fixtures/sibling/nested-blocks.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/nested-blocks.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested-blocks.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--nested-blocks-2 ()
 "Test `combobulate' with `fixtures/sibling/nested-blocks.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/nested-blocks.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested-blocks.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--nested-blocks-3 ()
 "Test `combobulate' with `fixtures/sibling/nested-blocks.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/nested-blocks.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested-blocks.py[@3~after].py")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-1 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-2 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-3 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@3~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-4 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@4~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-5 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@5~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-6 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@6~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--program-7 ()
 "Test `combobulate' with `fixtures/sibling/program.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/program.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 7)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/program.go[@7~after].go")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-1 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-2 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-3 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-4 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-5 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@5~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-6 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@6~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-dict-7 ()
 "Test `combobulate' with `fixtures/sibling/python-dict.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-dict.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 7)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-dict.py[@7~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-list-1 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-list-2 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-list-3 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-list-4 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-list-5 ()
 "Test `combobulate' with `fixtures/sibling/python-list.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-list.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-list.py[@5~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-match-case-1 ()
 "Test `combobulate' with `fixtures/sibling/python-match-case.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-match-case.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-match-case.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-match-case-2 ()
 "Test `combobulate' with `fixtures/sibling/python-match-case.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-match-case.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-match-case.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-match-case-3 ()
 "Test `combobulate' with `fixtures/sibling/python-match-case.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-match-case.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-match-case.py[@3~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-set-1 ()
 "Test `combobulate' with `fixtures/sibling/python-set.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-set.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-set.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-set-2 ()
 "Test `combobulate' with `fixtures/sibling/python-set.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-set.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-set.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-set-3 ()
 "Test `combobulate' with `fixtures/sibling/python-set.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-set.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-set.py[@3~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-string-1 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-string-2 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-string-3 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-string-4 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-string-5 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[@5~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-string-6 ()
 "Test `combobulate' with `fixtures/sibling/python-string.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-string.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-string.py[@6~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-pattern-1 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-pattern-2 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-pattern-3 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-pattern-4 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-pattern-5 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple-pattern.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple-pattern.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple-pattern.py[@5~after].py")))))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-1 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[@1~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-2 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[@2~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-3 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[@3~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-4 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[@4~after].py")))


(ert-deftest combobulate-test-python-combobulate-drag-down--python-tuple-5 ()
 "Test `combobulate' with `fixtures/sibling/python-tuple.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/sibling/python-tuple.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/python-tuple.py[@5~after].py")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--switch-1 ()
 "Test `combobulate' with `fixtures/sibling/switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/switch.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--switch-2 ()
 "Test `combobulate' with `fixtures/sibling/switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/switch.go[@2~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--switch-3 ()
 "Test `combobulate' with `fixtures/sibling/switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/switch.go[@3~after].go")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--type-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/type-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/type-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type-declaration.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--type-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/type-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/type-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type-declaration.go[@2~after].go")))))


(ert-deftest combobulate-test-toml-combobulate-drag-down--value-array-1 ()
 "Test `combobulate' with `fixtures/sibling/value-array.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/value-array.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value-array.toml[@1~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--value-array-2 ()
 "Test `combobulate' with `fixtures/sibling/value-array.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/value-array.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value-array.toml[@2~after].toml")))


(ert-deftest combobulate-test-toml-combobulate-drag-down--value-array-3 ()
 "Test `combobulate' with `fixtures/sibling/value-array.toml' in `toml-ts-mode' mode."
	     (combobulate-test
		 (:language toml :mode toml-ts-mode :fixture "fixtures/sibling/value-array.toml")
	       :tags
	       '(combobulate toml toml-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value-array.toml[@3~after].toml")))))


(ert-deftest combobulate-test-go-combobulate-drag-down--var-declaration-1 ()
 "Test `combobulate' with `fixtures/sibling/var-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/var-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/var-declaration.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-drag-down--var-declaration-2 ()
 "Test `combobulate' with `fixtures/sibling/var-declaration.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/sibling/var-declaration.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/var-declaration.go[@2~after].go")))))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-block-mapping-pairs-1 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping-pairs.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-block-mapping-pairs-2 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping-pairs.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping-pairs.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping-pairs.yaml[@2~after].yaml")))))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-block-mapping-1 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-block-mapping-2 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping.yaml[@2~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-block-mapping-3 ()
 "Test `combobulate' with `fixtures/sibling/yaml-block-mapping.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-block-mapping.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-block-mapping.yaml[@3~after].yaml")))))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-sequence-1 ()
 "Test `combobulate' with `fixtures/sibling/yaml-sequence.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-sequence.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-sequence.yaml[@1~after].yaml")))


(ert-deftest combobulate-test-yaml-combobulate-drag-down--yaml-sequence-2 ()
 "Test `combobulate' with `fixtures/sibling/yaml-sequence.yaml' in `yaml-ts-mode' mode."
	     (combobulate-test
		 (:language yaml :mode yaml-ts-mode :fixture "fixtures/sibling/yaml-sequence.yaml")
	       :tags
	       '(combobulate yaml yaml-ts-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/yaml-sequence.yaml[@2~after].yaml")))))


