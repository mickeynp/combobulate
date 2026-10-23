;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--class_members-1 ()
 "Test `combobulate' with `fixtures/up/class_members.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/class_members.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--class_type_members-1 ()
 "Test `combobulate' with `fixtures/up/class_type_members.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/class_type_members.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--class_type_members-1 ()
 "Test `combobulate' with `fixtures/up/class_type_members.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/class_type_members.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--class_types_b-1 ()
 "Test `combobulate' with `fixtures/up/class_types_b.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/class_types_b.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--class_types_b-1 ()
 "Test `combobulate' with `fixtures/up/class_types_b.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/class_types_b.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--classes-1 ()
 "Test `combobulate' with `fixtures/up/classes.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/classes.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--function_body-1 ()
 "Test `combobulate' with `fixtures/up/function_body.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/function_body.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--functor_sig-1 ()
 "Test `combobulate' with `fixtures/up/functor_sig.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/functor_sig.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--functor_sig-1 ()
 "Test `combobulate' with `fixtures/up/functor_sig.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/functor_sig.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--functor_type-1 ()
 "Test `combobulate' with `fixtures/up/functor_type.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/functor_type.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--functor_type-1 ()
 "Test `combobulate' with `fixtures/up/functor_type.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/functor_type.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--functors-1 ()
 "Test `combobulate' with `fixtures/up/functors.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/functors.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 7)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--functors-1 ()
 "Test `combobulate' with `fixtures/up/functors.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/functors.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 9)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 8)
	       (combobulate-test-go-to-marker 8)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 7)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--if_then_else-1 ()
 "Test `combobulate' with `fixtures/up/if_then_else.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/if_then_else.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--match_case_body-1 ()
 "Test `combobulate' with `fixtures/up/match_case_body.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/match_case_body.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--matches-1 ()
 "Test `combobulate' with `fixtures/up/matches.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/matches.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--module_sig_a-1 ()
 "Test `combobulate' with `fixtures/up/module_sig_a.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/module_sig_a.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--module_signature-1 ()
 "Test `combobulate' with `fixtures/up/module_signature.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/module_signature.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
	       (combobulate-test-go-to-marker 4)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 3)
	       (combobulate-test-go-to-marker 3)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 2)
	       (combobulate-test-go-to-marker 2)
	       (combobulate-navigate-up)
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--module_string-1 ()
 "Test `combobulate' with `fixtures/up/module_string.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/module_string.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--poly_variant-1 ()
 "Test `combobulate' with `fixtures/up/poly_variant.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/poly_variant.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--poly_variant-1 ()
 "Test `combobulate' with `fixtures/up/poly_variant.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/poly_variant.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--type_declarations-1 ()
 "Test `combobulate' with `fixtures/up/type_declarations.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/type_declarations.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--type_declarations-1 ()
 "Test `combobulate' with `fixtures/up/type_declarations.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/type_declarations.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-combobulate-navigate-up--variant_type-1 ()
 "Test `combobulate' with `fixtures/up/variant_type.ml' in `tuareg-mode' mode."
	     (combobulate-test
		 (:language ocaml :mode tuareg-mode :fixture "fixtures/up/variant_type.ml")
	       :tags
	       '(combobulate ocaml tuareg-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


(ert-deftest combobulate-test-ocaml-interface-combobulate-navigate-up--variant_type-1 ()
 "Test `combobulate' with `fixtures/up/variant_type.mli' in `tuareg-interface-mode' mode."
	     (combobulate-test
		 (:language ocaml-interface :mode tuareg-interface-mode :fixture "fixtures/up/variant_type.mli")
	       :tags
	       '(combobulate ocaml-interface tuareg-interface-mode combobulate-navigate-up)
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
	       (combobulate-test-assert-at-marker 1)))


