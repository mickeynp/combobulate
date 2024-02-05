;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice*-with-complex-missing-field-consequence-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(1 0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((region . "<div>Some jsx element</div>")))
		    (instructions
		     '("{" @ "null" > n > " ? " @
		       (choice* :name "consequence" :missing
				("null")
				:rest
				(r>))
		       n > " : "
		       (choice* :name "alternative" :missing
				("<"
				 (p other "SOME TAG")
				 "/>")
				:rest
				(r>))
		       n > "}" >)))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice*-with-complex-missing-field-consequence@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice*-with-complex-missing-field-alt-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0 0))
		    (combobulate-envelope-prompt-actions
		     '("mytag"))
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((region . "<div>Some jsx element</div>")))
		    (instructions
		     '("{" @ "null" > n > " ? " @
		       (choice* :name "consequence" :missing
				("null")
				:rest
				(r>))
		       n > " : "
		       (choice* :name "alternative" :missing
				("<"
				 (p other "SOME TAG")
				 "/>")
				:rest
				(r>))
		       n > "}" >)))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice*-with-complex-missing-field-alt@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice-simple-0-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("{a + "
		       (choice "1")
		       (choice "2")
		       "}")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice-simple-0@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice-simple-1-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(1))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("{a + "
		       (choice "1")
		       (choice "2")
		       "}")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice-simple-1@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice-has-choice-0-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0 0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("{a + "
		       (choice "1"
			       (choice "1.1")
			       (choice "1.2"))
		       (choice "2")
		       "}")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice-has-choice-0@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice-has-choice-1-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0 1))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("{a + "
		       (choice "1"
			       (choice "1.1")
			       (choice "1.2"))
		       (choice "2")
		       "}")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice-has-choice-1@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice-has-choice-2-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(1 0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("{a + "
		       (choice "1"
			       (choice "1.1")
			       (choice "1.2"))
		       (choice "2")
		       "}")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice-has-choice-2@1~after].tsx")))))))


(ert-deftest combobulate-test-tsx-combobulate-envelope-expand-instructions-tsx-choice-has-choice-3-component-1 ()
 "Test `combobulate' with `fixtures/envelope/component.tsx' in `tsx-ts-mode' mode."
	     (combobulate-test
		 (:language tsx :mode tsx-ts-mode :fixture "fixtures/envelope/component.tsx")
	       :tags
	       '(combobulate tsx tsx-ts-mode combobulate-envelope-expand-instructions-tsx)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(1 1))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("{a + "
		       (choice "1"
			       (choice "1.1")
			       (choice "1.2"))
		       (choice "2")
		       "}")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions-tsx/component.tsx[choice-has-choice-3@1~after].tsx")))))))


