;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-ocaml-combobulate-navigate-beginning-of-defun--items-1 ()
 "Test `combobulate' with `fixtures/defun/items.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/defun/items.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-beginning-of-defun)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 5)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-beginning-of-defun--items-1 ()
 "Test `combobulate' with `fixtures/defun/items.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/defun/items.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-beginning-of-defun)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 6)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-navigate-beginning-of-defun)
	       (combobulate-test-assert-at-marker 5)
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
	       (combobulate-test-assert-at-marker 1)))


