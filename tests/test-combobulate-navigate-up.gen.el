;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-c-combobulate-navigate-up--nested-1 ()
 "Test `combobulate' with `fixtures/up/nested.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/up/nested.c")
	       :tags
	       '(combobulate c c-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 5)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


