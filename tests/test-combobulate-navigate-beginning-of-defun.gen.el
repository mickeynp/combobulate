;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-c-combobulate-navigate-beginning-of-defun--items-1 ()
 "Test `combobulate' with `fixtures/defun/items.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/defun/items.c")
	       :tags
	       '(combobulate c c-mode combobulate-navigate-beginning-of-defun)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 4)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 1)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 1)))


