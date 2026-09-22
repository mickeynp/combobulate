;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-c-combobulate-navigate-end-of-defun--items-5 ()
 "Test `combobulate' with `fixtures/defun-end/items.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/defun-end/items.c")
	       :tags
	       '(combobulate c c-mode combobulate-navigate-end-of-defun)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-end-of-defun)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-end-of-defun)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-end-of-defun)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-end-of-defun)
	       (combobulate-test-assert-at-marker 5)))


