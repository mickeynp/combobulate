;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--attributes-1 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/attributes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--attributes-2 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/attributes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--attributes-3 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/attributes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--attributes-4 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/attributes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--attributes-5 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/attributes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--attributes-6 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/attributes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--attributes-1 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--attributes-2 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--attributes-3 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--attributes-4 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--attributes-5 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--attributes-6 ()
 "Test `combobulate' with `fixtures/mark-defun/attributes.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/attributes.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/attributes.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-1 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-2 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-3 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@3~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-4 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@4~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-5 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@5~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-6 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@6~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-7 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@7~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-8 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@8~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-defun--signature-9 ()
 "Test `combobulate' with `fixtures/mark-defun/signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-defun/signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/signature.mli[@9~after].mli")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-1 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-2 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-3 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-4 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-5 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-6 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 6)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@6~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-7 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 7)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@7~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-defun--structure-8 ()
 "Test `combobulate' with `fixtures/mark-defun/structure.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-defun/structure.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-defun)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-mark-defun)
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-defun/structure.ml[@8~after].ml")))


