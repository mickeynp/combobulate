;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-c-combobulate-mark-defun--declarations-1 ()
 "Test `combobulate' with `fixtures/mark-defun/declarations.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/mark-defun/declarations.c")
	       :tags
	       '(combobulate c c-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/declarations.c[@1~after].c")))


(ert-deftest combobulate-test-c-combobulate-mark-defun--declarations-2 ()
 "Test `combobulate' with `fixtures/mark-defun/declarations.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/mark-defun/declarations.c")
	       :tags
	       '(combobulate c c-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/declarations.c[@2~after].c")))


(ert-deftest combobulate-test-c-combobulate-mark-defun--declarations-3 ()
 "Test `combobulate' with `fixtures/mark-defun/declarations.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/mark-defun/declarations.c")
	       :tags
	       '(combobulate c c-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/declarations.c[@3~after].c")))


(ert-deftest combobulate-test-c-combobulate-mark-defun--declarations-4 ()
 "Test `combobulate' with `fixtures/mark-defun/declarations.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/mark-defun/declarations.c")
	       :tags
	       '(combobulate c c-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/declarations.c[@4~after].c")))


(ert-deftest combobulate-test-c-combobulate-mark-defun--declarations-5 ()
 "Test `combobulate' with `fixtures/mark-defun/declarations.c' in `c-mode' mode."
	     (combobulate-test
		 (:language c :mode c-mode :fixture "fixtures/mark-defun/declarations.c")
	       :tags
	       '(combobulate c c-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/declarations.c[@5~after].c")))


