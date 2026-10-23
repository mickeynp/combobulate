;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-ocaml-combobulate-mark-node-dwim--constructs-1 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-node/constructs.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.ml[@1~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-node-dwim--constructs-2 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-node/constructs.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.ml[@2~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-node-dwim--constructs-3 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-node/constructs.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.ml[@3~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-node-dwim--constructs-4 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-node/constructs.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.ml[@4~after].ml")))


(ert-deftest combobulate-test-ocaml-combobulate-mark-node-dwim--constructs-5 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/mark-node/constructs.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 5)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.ml[@5~after].ml")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-node-dwim--constructs-1 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-node/constructs.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 1)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.mli[@1~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-node-dwim--constructs-2 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-node/constructs.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.mli[@2~after].mli")))


(ert-deftest combobulate-test-ocaml-interface-combobulate-mark-node-dwim--constructs-3 ()
 "Test `combobulate' with `fixtures/mark-node/constructs.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/mark-node/constructs.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-mark-node-dwim)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-with-stubbed-proffer-choices
		   (:choices
		    '(0 0 0 0))
		 (combobulate-mark-node-dwim))
	       (delete-region
		(region-beginning)
		(region-end))
	       (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-mark-node-dwim/constructs.mli[@3~after].mli")))


