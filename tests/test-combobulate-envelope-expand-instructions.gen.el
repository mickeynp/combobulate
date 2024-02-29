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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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
		    (combobulate-envelope-expansion-actions 'nil)
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


(ert-deftest combobulate-test-python-combobulate-envelope-expand-instructions-choice*-with-complex-missing-field-blank-1 ()
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
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[choice*-with-complex-missing-field@1~after].py")))))))


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
			   (combobulate-compare-action-with-fixture-delta "./fixture-deltas/combobulate-envelope-expand-instructions/blank.py[choice-simple-1@1~after].py")))))))


