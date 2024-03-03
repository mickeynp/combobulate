;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-go-combobulate-splice-self-offset-1--choice-1-for-loop-body-1 ()
 "Test `combobulate' with `fixtures/splice/choice-1-for-loop-body.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-1-for-loop-body.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-self-offset-1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(1 1 1 1 1 1 1))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self-offset-1/choice-1-for-loop-body.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-splice-self-offset-1--choice-1-inside-case-1 ()
 "Test `combobulate' with `fixtures/splice/choice-1-inside-case.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-1-inside-case.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-self-offset-1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(1 1 1 1 1 1 1))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self-offset-1/choice-1-inside-case.go[@1~after].go")))


(ert-deftest combobulate-test-go-combobulate-splice-self-offset-1--choice-1-switch-1 ()
 "Test `combobulate' with `fixtures/splice/choice-1-switch.go' in `go-ts-mode' mode."
	     (combobulate-test
		 (:language go :mode go-ts-mode :fixture "fixtures/splice/choice-1-switch.go")
	       :tags
	       '(combobulate go go-ts-mode combobulate-splice-self-offset-1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(1 1 1 1 1 1 1))
		 (combobulate-splice-self))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-splice-self-offset-1/choice-1-switch.go[@1~after].go")))


