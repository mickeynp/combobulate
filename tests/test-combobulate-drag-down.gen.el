;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--class_type_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/class_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/class_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_type_expressions.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--class_type_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/class_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/class_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_type_expressions.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--class_type_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/class_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/class_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_type_expressions.ml[@3~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--class_type_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/class_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/class_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_type_expressions.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--class_type_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/class_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/class_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_type_expressions.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--class_type_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/class_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/class_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_type_expressions.mli[@3~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--class_value_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/class_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/class_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_value_expressions.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--class_value_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/class_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/class_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_value_expressions.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--class_value_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/class_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/class_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_value_expressions.ml[@3~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--class_value_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/class_value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/class_value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_value_expressions.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--class_value_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/class_value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/class_value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_value_expressions.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--class_value_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/class_value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/class_value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/class_value_expressions.mli[@3~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-1 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-2 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-3 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-4 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-5 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-6 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--extension_points-7 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/extension_points.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 7)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.ml[@7~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--extension_points-1 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/extension_points.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--extension_points-2 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/extension_points.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--extension_points-3 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/extension_points.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--extension_points-4 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/extension_points.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--extension_points-5 ()
 "Test `combobulate' with `fixtures/sibling/extension_points.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/extension_points.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/extension_points.mli[@5~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--gadt-1 ()
 "Test `combobulate' with `fixtures/sibling/gadt.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/gadt.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/gadt.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--gadt-2 ()
 "Test `combobulate' with `fixtures/sibling/gadt.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/gadt.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/gadt.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--gadt-3 ()
 "Test `combobulate' with `fixtures/sibling/gadt.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/gadt.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/gadt.ml[@3~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--gadt-1 ()
 "Test `combobulate' with `fixtures/sibling/gadt.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/gadt.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/gadt.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--gadt-2 ()
 "Test `combobulate' with `fixtures/sibling/gadt.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/gadt.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/gadt.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--gadt-3 ()
 "Test `combobulate' with `fixtures/sibling/gadt.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/gadt.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/gadt.mli[@3~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_and_in-1 ()
 "Test `combobulate' with `fixtures/sibling/let_and_in.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_and_in.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_and_in.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_and_in-2 ()
 "Test `combobulate' with `fixtures/sibling/let_and_in.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_and_in.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_and_in.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_and_in-3 ()
 "Test `combobulate' with `fixtures/sibling/let_and_in.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_and_in.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_and_in.ml[@3~after].ml")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-1 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-2 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-3 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-4 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-5 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-6 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-7 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-8 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@8~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-9 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@9~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-10 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@10~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--let_bindings-11 ()
 "Test `combobulate' with `fixtures/sibling/let_bindings.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/let_bindings.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 11)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/let_bindings.ml[@11~after].ml")))))


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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_a-1 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_a.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_a-2 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_a.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_a-3 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_a.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_a-4 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_a.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 4)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.ml[@4~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_sig_a-1 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_sig_a.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_sig_a-2 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_sig_a.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_sig_a-3 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_sig_a.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_sig_a-4 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_a.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_sig_a.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 4)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_a.mli[@4~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_b-1 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_b.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_b.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_b.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_b-2 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_b.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_b.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_b.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_sig_b-3 ()
 "Test `combobulate' with `fixtures/sibling/module_sig_b.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_sig_b.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_sig_b.ml[@3~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_signatures-1 ()
 "Test `combobulate' with `fixtures/sibling/module_signatures.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_signatures.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_signatures.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_signatures-2 ()
 "Test `combobulate' with `fixtures/sibling/module_signatures.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_signatures.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_signatures.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_signatures-3 ()
 "Test `combobulate' with `fixtures/sibling/module_signatures.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_signatures.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_signatures.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_signatures-4 ()
 "Test `combobulate' with `fixtures/sibling/module_signatures.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_signatures.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_signatures.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_signatures-5 ()
 "Test `combobulate' with `fixtures/sibling/module_signatures.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_signatures.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_signatures.mli[@5~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_string-1 ()
 "Test `combobulate' with `fixtures/sibling/module_string.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_string.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_string.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_string-2 ()
 "Test `combobulate' with `fixtures/sibling/module_string.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_string.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_string.ml[@2~after].ml")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_type_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_type_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_type_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_type_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_type_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_type_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.ml[@6~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_type_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_type_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_type_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_type_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_type_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--module_type_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/module_type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/module_type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_type_expressions.mli[@6~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_value_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_value_expressions.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_value_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_value_expressions.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_value_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_value_expressions.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_value_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_value_expressions.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_value_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_value_expressions.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--module_value_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/module_value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/module_value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 6)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/module_value_expressions.ml[@6~after].ml")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-1 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-2 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-3 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-4 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-5 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-6 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-7 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-8 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@8~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-9 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@9~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-10 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@10~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--modules-11 ()
 "Test `combobulate' with `fixtures/sibling/modules.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/modules.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 11)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.ml[@11~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-1 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-2 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-3 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-4 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-5 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-6 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-7 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@7~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-8 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@8~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-9 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@9~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--modules-10 ()
 "Test `combobulate' with `fixtures/sibling/modules.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/modules.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 10)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/modules.mli[@10~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--nested_type_records-1 ()
 "Test `combobulate' with `fixtures/sibling/nested_type_records.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/nested_type_records.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested_type_records.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--nested_type_records-2 ()
 "Test `combobulate' with `fixtures/sibling/nested_type_records.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/nested_type_records.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested_type_records.ml[@2~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--nested_type_records-1 ()
 "Test `combobulate' with `fixtures/sibling/nested_type_records.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/nested_type_records.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested_type_records.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--nested_type_records-2 ()
 "Test `combobulate' with `fixtures/sibling/nested_type_records.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/nested_type_records.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 2)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/nested_type_records.mli[@2~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-1 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-2 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-3 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-4 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-5 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-6 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-7 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-8 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@8~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-9 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@9~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-10 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@10~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-11 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 11)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@11~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-12 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 12)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@12~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-13 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 13)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@13~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-14 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 14)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@14~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-15 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 15)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@15~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-16 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 16)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@16~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-17 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 17)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@17~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--patterns-18 ()
 "Test `combobulate' with `fixtures/sibling/patterns.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/patterns.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 18)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.ml[@18~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--patterns-1 ()
 "Test `combobulate' with `fixtures/sibling/patterns.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/patterns.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--patterns-2 ()
 "Test `combobulate' with `fixtures/sibling/patterns.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/patterns.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--patterns-3 ()
 "Test `combobulate' with `fixtures/sibling/patterns.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/patterns.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--patterns-4 ()
 "Test `combobulate' with `fixtures/sibling/patterns.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/patterns.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--patterns-5 ()
 "Test `combobulate' with `fixtures/sibling/patterns.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/patterns.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 5)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/patterns.mli[@5~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--signature_attributes-1 ()
 "Test `combobulate' with `fixtures/sibling/signature_attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/signature_attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/signature_attributes.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--signature_attributes-2 ()
 "Test `combobulate' with `fixtures/sibling/signature_attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/signature_attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/signature_attributes.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--signature_attributes-3 ()
 "Test `combobulate' with `fixtures/sibling/signature_attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/signature_attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/signature_attributes.mli[@3~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_and_bindings-1 ()
 "Test `combobulate' with `fixtures/sibling/type_and_bindings.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_and_bindings.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_and_bindings.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_and_bindings-2 ()
 "Test `combobulate' with `fixtures/sibling/type_and_bindings.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_and_bindings.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_and_bindings.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_and_bindings-3 ()
 "Test `combobulate' with `fixtures/sibling/type_and_bindings.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_and_bindings.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 3)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_and_bindings.mli[@3~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-1 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-2 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-3 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-4 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-5 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-6 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-7 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-8 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@8~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-9 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@9~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-10 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@10~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-11 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 11)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@11~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-12 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 12)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@12~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-13 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 13)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@13~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-14 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 14)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@14~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-15 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 15)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@15~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_declarations-16 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 16)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.ml[@16~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-1 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-2 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-3 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-4 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-5 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-6 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-7 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@7~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-8 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@8~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-9 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@9~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-10 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@10~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-11 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 11)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@11~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-12 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 12)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@12~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-13 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 13)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@13~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-14 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 14)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@14~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-15 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 15)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@15~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_declarations-16 ()
 "Test `combobulate' with `fixtures/sibling/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 16)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_declarations.mli[@16~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-7 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_expressions-8 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 8)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.ml[@8~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-7 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@7~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_expressions-8 ()
 "Test `combobulate' with `fixtures/sibling/type_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 8)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_expressions.mli[@8~after].mli")))))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_records-1 ()
 "Test `combobulate' with `fixtures/sibling/type_records.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_records.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_records-2 ()
 "Test `combobulate' with `fixtures/sibling/type_records.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_records.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_records-3 ()
 "Test `combobulate' with `fixtures/sibling/type_records.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_records.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--type_records-4 ()
 "Test `combobulate' with `fixtures/sibling/type_records.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/type_records.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 4)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.ml[@4~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_records-1 ()
 "Test `combobulate' with `fixtures/sibling/type_records.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_records.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_records-2 ()
 "Test `combobulate' with `fixtures/sibling/type_records.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_records.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_records-3 ()
 "Test `combobulate' with `fixtures/sibling/type_records.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_records.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--type_records-4 ()
 "Test `combobulate' with `fixtures/sibling/type_records.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/type_records.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 4)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/type_records.mli[@4~after].mli")))))


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


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-7 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-8 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@8~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-9 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@9~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-10 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@10~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-11 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 11)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@11~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-12 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 12)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@12~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-13 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 13)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@13~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-14 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 14)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@14~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-15 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 15)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@15~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-16 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 16)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@16~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-17 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 17)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@17~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-18 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 18)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@18~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-19 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 19)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@19~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-20 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 20)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@20~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-21 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 21)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@21~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-22 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 22)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@22~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-23 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 23)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@23~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-24 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 24)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@24~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-25 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 25)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@25~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-26 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 26)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@26~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-27 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 27)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@27~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-28 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 28)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@28~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-29 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 29)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@29~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-30 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 30)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@30~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-31 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 31)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@31~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-32 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 32)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@32~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-33 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 33)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@33~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-34 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 34)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@34~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-35 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 35)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@35~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-36 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 36)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@36~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-37 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 37)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@37~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-38 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 38)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@38~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-39 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 39)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@39~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-40 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 40)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@40~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-41 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 41)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@41~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-42 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 42)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@42~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-43 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 43)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@43~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-44 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 44)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@44~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-45 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 45)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@45~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-46 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 46)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@46~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-47 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 47)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@47~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-48 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 48)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@48~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-drag-down--value_expressions-49 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/sibling/value_expressions.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 49)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.ml[@49~after].ml")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-1 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-2 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-3 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-4 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-5 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-6 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-7 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@7~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-8 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@8~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-9 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@9~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-10 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@10~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-11 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 11)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@11~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-12 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 12)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@12~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-13 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 13)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@13~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-14 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 14)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@14~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-15 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 15)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@15~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-16 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 16)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@16~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-17 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 17)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@17~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-18 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 18)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@18~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-19 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 19)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@19~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-20 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 20)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@20~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-21 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 21)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@21~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-22 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 22)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@22~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-23 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 23)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@23~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-24 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 24)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@24~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-25 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 25)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@25~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-26 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 26)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@26~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-27 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 27)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@27~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-28 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 28)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@28~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-29 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 29)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@29~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-30 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 30)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@30~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-31 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 31)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@31~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-32 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 32)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@32~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-33 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 33)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@33~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-34 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 34)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@34~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-35 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 35)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@35~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-36 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 36)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@36~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-37 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 37)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@37~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-38 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 38)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@38~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-39 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 39)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@39~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-40 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 40)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@40~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-41 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 41)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@41~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_expressions-42 ()
 "Test `combobulate' with `fixtures/sibling/value_expressions.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_expressions.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 42)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_expressions.mli[@42~after].mli")))))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-1 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-2 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-3 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-4 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-5 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-6 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-7 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@7~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-8 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@8~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-9 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@9~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-10 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (combobulate-test-go-to-marker 10)
	       (combobulate-drag-down)
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@10~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-drag-down--value_specification-11 ()
 "Test `combobulate' with `fixtures/sibling/value_specification.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/sibling/value_specification.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-drag-down)
	       (should-error
		(progn
		  (combobulate-test-go-to-marker 11)
		  (combobulate-drag-down)
		  (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-drag-down/value_specification.mli[@11~after].mli")))))


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


