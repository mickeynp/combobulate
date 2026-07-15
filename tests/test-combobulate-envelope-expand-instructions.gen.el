;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-string-basic-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("test string")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[string-basic@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-string-multiple-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a" "b" "c")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[string-multiple@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-newline-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a = 1" n "b = 1")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[newline@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-newline-and-indent-simple-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a = 1" n> "b = 1")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[newline-and-indent-simple@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-newline-and-indent-inside-block-then-outside-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("if 1:" n> "b = 1" n "c = 1")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[newline-and-indent-inside-block-then-outside@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-newline-and-indent-inside-block-both-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("if 1:" n> "b = 1" n> "c = 1" n> "while True:" n> "d = 3")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[newline-and-indent-inside-block-both@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-save-column-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("def Foo():" n>
		       (save-column "try:" n> "do_something()" n)
		       "except:" n> "pass")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[save-column@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-save-column-nested-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("def Foo():" n>
		       (save-column "try:" n>
				    (save-column "with some_stuff() as foo:" n> "pass")
				    n)
		       "except:" n> "pass")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[save-column-nested@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-insert-region-register-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '(r)))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[insert-region-register@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-insert-region-register-then-indent-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((region . "random = 1")))
		    (instructions
		     '("if True:" n> r)))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[insert-region-register-then-indent@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-insert-region-register-2-then-indent-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((some-register . "my_register = 1")))
		    (instructions
		     '("if True:" n>
		       (r some-register))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[insert-region-register-2-then-indent@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-insert-missing-register-with-default-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("if True:" n>
		       (r some-register "foo"))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[insert-missing-register-with-default@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-prompt-register-once-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((some-prompt . "foo")))
		    (instructions
		     '("a = "
		       (p some-prompt "Pick a value"))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[prompt-register-once@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-prompt-register-reused-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((some-prompt . "this is a prompt value")))
		    (instructions
		     '("a = "
		       (p some-prompt "Pick a value")
		       n> "b = "
		       (f some-prompt))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[prompt-register-reused@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-prompt-manual-input-once-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("simulated prompt value"))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a = "
		       (p some-prompt "Pick a value"))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[prompt-manual-input-once@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-prompt-manual-input-twice-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("simulated prompt value" "second value"))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a = "
		       (p some-prompt "Pick a value")
		       n> "b = "
		       (p another-prompt "Pick a second value"))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[prompt-manual-input-twice@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-prompt-manual-keyboard-quit-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("foo" keyboard-quit))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a = "
		       (p some-prompt "Pick a value")
		       n> "b = "
		       (p another-prompt "Pick a value"))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[prompt-manual-keyboard-quit@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-field-before-prompt-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("blah"))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("a = "
		       (f some-prompt)
		       n> "b = "
		       (p some-prompt "Pick a value"))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[field-before-prompt@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-prompt-and-field-transformers-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("MiXeD"))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("prompt="
		       (p value "Value" upcase)
		       n "plain="
		       (f value)
		       n "field="
		       (f value downcase))))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[prompt-and-field-transformers@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-block-explicit-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("value"))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("outer["
		       (b "inner="
			  (p nested "Nested")
			  ",field="
			  (f nested))
		       "]")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[block-explicit@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-point-marker-advances-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("left" @> "right")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (let
			       ((envelope-start
				 (point-marker)))
			     (combobulate-envelope-expand-instructions instructions)
			     (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[point-marker-advances@1~after].py")
			     (should
			      (=
			       (point)
			       (+ envelope-start 9))))))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-point-integer-stays-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("left" @@ "right")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (let
			       ((envelope-start
				 (point-marker)))
			     (combobulate-envelope-expand-instructions instructions)
			     (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[point-integer-stays@1~after].py")
			     (should
			      (=
			       (point)
			       (+ envelope-start 4))))))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-deindent-one-level-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("if True:" n> "if False:" n> "nested = 1" n> < "outer = 2")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[deindent-one-level@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-repeat-reject-immediately-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions
		     '(no))
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("before" n
		       (repeat "item = "
			       (p item "Item")
			       n)
		       "after")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[repeat-reject-immediately@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-repeat-accept-once-then-reject-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("one"))
		    (combobulate-envelope-prompt-expansion-actions
		     '(yes no))
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("before" n
		       (repeat "item = "
			       (p item "Item")
			       n)
		       "after")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[repeat-accept-once-then-reject@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-repeat-1-accept-once-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices 'nil)
		    (combobulate-envelope-prompt-actions
		     '("only"))
		    (combobulate-envelope-prompt-expansion-actions
		     '(yes))
		    (combobulate-envelope-registers 'nil)
		    (instructions
		     '("before" n
		       (repeat-1 "item = "
				 (p item "Item")
				 n)
		       "after")))
		 (combobulate-with-stubbed-prompt-expansion
		     (combobulate-with-stubbed-envelope-prompt
			 (combobulate-with-stubbed-proffer-choices
			     (:choices combobulate-envelope-proffer-choices)
			   (combobulate-test-go-to-marker 1)
			   (combobulate-envelope-expand-instructions instructions)
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[repeat-1-accept-once@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-choicestar-with-complex-missing-field-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0 0))
		    (combobulate-envelope-prompt-actions
		     '("mytag"))
		    (combobulate-envelope-prompt-expansion-actions 'nil)
		    (combobulate-envelope-registers
		     '((region . "<div>Some jsx element</div>")))
		    (instructions
		     '("{" @ "null" > n > " ? " @
		       (choice* :name "consequence" :missing
				("null")
				:rest
				(r>))
		       n "  : "
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
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[choicestar-with-complex-missing-field@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-choice-simple-0-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(0))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
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
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[choice-simple-0@1~after].py")))))))


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-choice-simple-1-blank-1 ()
 "Test `combobulate' with `fixtures/envelope/blank.py' in `python-ts-mode' mode."
	     (combobulate-test
		 (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
	       :tags
	       '(combobulate python python-ts-mode combobulate-envelope-expand-instructions)
	       (combobulate-test-go-to-marker 1)
	       (let
		   ((combobulate-envelope-proffer-choices
		     '(1))
		    (combobulate-envelope-prompt-actions 'nil)
		    (combobulate-envelope-prompt-expansion-actions 'nil)
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
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[choice-simple-1@1~after].py")))))))


