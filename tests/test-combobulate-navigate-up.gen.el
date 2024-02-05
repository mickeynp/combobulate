;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-tsx-combobulate-navigate-up--module-statements-8 ()
 "Test `combobulate' with `fixtures/up-down/module-statements.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/up-down/module-statements.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 8)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-up--nested-jsx-8 ()
 "Test `combobulate' with `fixtures/up-down/nested-jsx.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/up-down/nested-jsx.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 7)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 8)))


