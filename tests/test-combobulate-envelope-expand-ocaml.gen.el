;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-ocaml-combobulate-envelope-expand-ocaml-ocaml-begin-end-with-region-blank-1
    ()

  "Test `combobulate' with `fixtures/envelope/blank.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/envelope/blank.ml")
    :tags
    '(combobulate ocaml tuareg-mode combobulate-envelope-expand-ocaml)
    (combobulate-test-go-to-marker 1)
    (let
	((combobulate-envelope-proffer-choices '(0))
	 (combobulate-envelope-prompt-actions 'nil)
	 (combobulate-envelope-expansion-actions 'nil)
	 (combobulate-envelope-registers
	  '((region . "print_endline \"Hello, World!\"")))
	 (instructions '("begin" n> @ r> n> "end" > n>)))
      (combobulate-with-stubbed-prompt-expansion
	  (combobulate-with-stubbed-envelope-prompt
	      (combobulate-with-stubbed-proffer-choices
		  (:choices combobulate-envelope-proffer-choices)
		(combobulate-test-go-to-marker 1)
		(combobulate-envelope-expand-instructions instructions)
		(combobulate-compare-action-with-fixture-delta
		 "./fixture-deltas/combobulate-envelope-expand-ocaml/blank.ml[ocaml-begin-end-with-region@1~after].ml")))))))


(ert-deftest
    combobulate-test-ocaml-combobulate-envelope-expand-ocaml-ocaml-let-binding-blank-1
    ()

  "Test `combobulate' with `fixtures/envelope/blank.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/envelope/blank.ml")
    :tags
    '(combobulate ocaml tuareg-mode combobulate-envelope-expand-ocaml)
    (combobulate-test-go-to-marker 1)
    (let
	((combobulate-envelope-proffer-choices '(0))
	 (combobulate-envelope-prompt-actions '("my_var"))
	 (combobulate-envelope-expansion-actions 'nil)
	 (combobulate-envelope-registers '((region . "42 + 1")))
	 (instructions
	  '("let " (p name "Name") " =" n> @ r> n> "in" n> @ n>)))
      (combobulate-with-stubbed-prompt-expansion
	  (combobulate-with-stubbed-envelope-prompt
	      (combobulate-with-stubbed-proffer-choices
		  (:choices combobulate-envelope-proffer-choices)
		(combobulate-test-go-to-marker 1)
		(combobulate-envelope-expand-instructions instructions)
		(combobulate-compare-action-with-fixture-delta
		 "./fixture-deltas/combobulate-envelope-expand-ocaml/blank.ml[ocaml-let-binding@1~after].ml")))))))


(ert-deftest
    combobulate-test-ocaml-combobulate-envelope-expand-ocaml-ocaml-for-loop-blank-1
    ()

  "Test `combobulate' with `fixtures/envelope/blank.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/envelope/blank.ml")
    :tags
    '(combobulate ocaml tuareg-mode combobulate-envelope-expand-ocaml)
    (combobulate-test-go-to-marker 1)
    (let
	((combobulate-envelope-proffer-choices '(0))
	 (combobulate-envelope-prompt-actions '("i" "0" "10"))
	 (combobulate-envelope-expansion-actions 'nil)
	 (combobulate-envelope-registers '((region . "print_int i")))
	 (instructions
	  '("for " (p var "Variable") " = " (p start "Start") " to "
	    (p end "End") " do" n> @ r> n> "done" > n>)))
      (combobulate-with-stubbed-prompt-expansion
	  (combobulate-with-stubbed-envelope-prompt
	      (combobulate-with-stubbed-proffer-choices
		  (:choices combobulate-envelope-proffer-choices)
		(combobulate-test-go-to-marker 1)
		(combobulate-envelope-expand-instructions instructions)
		(combobulate-compare-action-with-fixture-delta
		 "./fixture-deltas/combobulate-envelope-expand-ocaml/blank.ml[ocaml-for-loop@1~after].ml")))))))


(ert-deftest
    combobulate-test-ocaml-combobulate-envelope-expand-ocaml-ocaml-match-statement-single-case-blank-1
    ()

  "Test `combobulate' with `fixtures/envelope/blank.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/envelope/blank.ml")
    :tags
    '(combobulate ocaml tuareg-mode combobulate-envelope-expand-ocaml)
    (combobulate-test-go-to-marker 1)
    (let
	((combobulate-envelope-proffer-choices '(1 0))
	 (combobulate-envelope-prompt-actions '("my_list" "[]"))
	 (combobulate-envelope-expansion-actions 'nil)
	 (combobulate-envelope-registers '((region . "0")))
	 (instructions
	  '("match " (p expr "Expression") " with" n> "| "
	    (p pat "Pattern") " ->" n> @ r> n>
	    (choice* :missing nil :rest
		     ("| " (p pat2 "Next Pattern") " ->" n> @ n>)
		     :name "add-pattern"))))
      (combobulate-with-stubbed-prompt-expansion
	  (combobulate-with-stubbed-envelope-prompt
	      (combobulate-with-stubbed-proffer-choices
		  (:choices combobulate-envelope-proffer-choices)
		(combobulate-test-go-to-marker 1)
		(combobulate-envelope-expand-instructions instructions)
		(combobulate-compare-action-with-fixture-delta
		 "./fixture-deltas/combobulate-envelope-expand-ocaml/blank.ml[ocaml-match-statement-single-case@1~after].ml")))))))


(ert-deftest
    combobulate-test-ocaml-combobulate-envelope-expand-ocaml-ocaml-match-statement-multiple-cases-blank-1
    ()

  "Test `combobulate' with `fixtures/envelope/blank.ml' in `tuareg-mode' mode."
  (combobulate-test
      (:language ocaml :mode tuareg-mode :fixture
		 "fixtures/envelope/blank.ml")
    :tags
    '(combobulate ocaml tuareg-mode combobulate-envelope-expand-ocaml)
    (combobulate-test-go-to-marker 1)
    (let
	((combobulate-envelope-proffer-choices '(0 1))
	 (combobulate-envelope-prompt-actions
	  '("my_list" "[]" "x :: xs"))
	 (combobulate-envelope-expansion-actions 'nil)
	 (combobulate-envelope-registers '((region . "0")))
	 (instructions
	  '("match " (p expr "Expression") " with" n> "| "
	    (p pat "Pattern") " ->" n> @ r> n>
	    (choice* :missing nil :rest
		     ("| " (p pat2 "Next Pattern") " ->" n> @ n>)
		     :name "add-pattern"))))
      (combobulate-with-stubbed-prompt-expansion
	  (combobulate-with-stubbed-envelope-prompt
	      (combobulate-with-stubbed-proffer-choices
		  (:choices combobulate-envelope-proffer-choices)
		(combobulate-test-go-to-marker 1)
		(combobulate-envelope-expand-instructions instructions)
		(combobulate-compare-action-with-fixture-delta
		 "./fixture-deltas/combobulate-envelope-expand-ocaml/blank.ml[ocaml-match-statement-multiple-cases@1~after].ml")))))))


