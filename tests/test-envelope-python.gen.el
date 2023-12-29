;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-python-repeat-simple-blank-1 ()
  "Test `repeat-simple' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "repeat-simple")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-prompt-actions '("blah"))
                 (combobulate-envelope-prompt-expansion-actions
                  '(yes yes no)))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '((repeat "if 1:" n> "a = "
                           (p repeating-prompt "Pick a value") n>
                           "b = 1" n>)))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/repeat-simple/blank.py[repeat-simple@1~after].py")))


(ert-deftest combobulate-test-python-field-before-prompt-blank-1
    ()
  "Test `field-before-prompt' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "field-before-prompt")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let ((combobulate-envelope-prompt-actions '("blah")))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("a = " (f some-prompt) n> "b = "
                   (p some-prompt "Pick a value")))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/field-before-prompt/blank.py[field-before-prompt@1~after].py")))


(ert-deftest
    combobulate-test-python-prompt-manual-keyboard-quit-blank-1 ()
  "Test `prompt-manual-keyboard-quit' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "prompt-manual-keyboard-quit")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-prompt-actions
                  '("foo" keyboard-quit)))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("a = " (p some-prompt "Pick a value") n> "b = "
                   (p another-prompt "Pick a value")))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/prompt-manual-keyboard-quit/blank.py[prompt-manual-keyboard-quit@1~after].py")))


(ert-deftest combobulate-test-python-prompt-manual-input-twice-blank-1
    ()
  "Test `prompt-manual-input-twice' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "prompt-manual-input-twice")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-prompt-actions
                  '("simulated prompt value" "second value")))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("a = " (p some-prompt "Pick a value") n> "b = "
                   (p another-prompt "Pick a second value")))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/prompt-manual-input-twice/blank.py[prompt-manual-input-twice@1~after].py")))


(ert-deftest combobulate-test-python-prompt-manual-input-once-blank-1
    ()
  "Test `prompt-manual-input-once' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "prompt-manual-input-once")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-prompt-actions
                  '("simulated prompt value")))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("a = " (p some-prompt "Pick a value")))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/prompt-manual-input-once/blank.py[prompt-manual-input-once@1~after].py")))


(ert-deftest combobulate-test-python-prompt-register-reused-blank-1
    ()
  "Test `prompt-register-reused' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "prompt-register-reused")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-registers
                  '((some-prompt . "this is a prompt value"))))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("a = " (p some-prompt "Pick a value") n> "b = "
                   (f some-prompt)))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/prompt-register-reused/blank.py[prompt-register-reused@1~after].py")))


(ert-deftest combobulate-test-python-prompt-register-once-blank-1
    ()
  "Test `prompt-register-once' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "prompt-register-once")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-registers
                  '((some-prompt . "foo"))))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("a = " (p some-prompt "Pick a value")))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/prompt-register-once/blank.py[prompt-register-once@1~after].py")))


(ert-deftest
    combobulate-test-python-insert-missing-register-with-default-blank-1
    ()
  "Test `insert-missing-register-with-default' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags
  '(python python-ts-mode "insert-missing-register-with-default")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let ((combobulate-envelope-registers))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("if True:" n> (r some-register "foo")))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/insert-missing-register-with-default/blank.py[insert-missing-register-with-default@1~after].py")))


(ert-deftest
    combobulate-test-python-insert-region-register-2-then-indent-blank-1
    ()
  "Test `insert-region-register-2-then-indent' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags
  '(python python-ts-mode "insert-region-register-2-then-indent")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-registers
                  '((some-register . "my_register = 1"))))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("if True:" n> (r some-register)))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/insert-region-register-2-then-indent/blank.py[insert-region-register-2-then-indent@1~after].py")))


(ert-deftest
    combobulate-test-python-insert-region-register-then-indent-blank-1
    ()
  "Test `insert-region-register-then-indent' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "insert-region-register-then-indent")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-registers
                  '((region . "random = 1"))))
              (combobulate-with-stubbed-proffer-choices
                  (:choices
                   (if (boundp 'combobulate-envelope-proffer-choices)
                       combobulate-envelope-proffer-choices
                     nil))
                (combobulate-envelope-expand-instructions
                 '("if True:" n> r))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/insert-region-register-then-indent/blank.py[insert-region-register-then-indent@1~after].py")))


(ert-deftest combobulate-test-python-insert-region-register-blank-1
    ()
  "Test `insert-region-register' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "insert-region-register")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions '(r))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/insert-region-register/blank.py[insert-region-register@1~after].py")))


(ert-deftest combobulate-test-python-save-column-nested-blank-1 ()
  "Test `save-column-nested' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "save-column-nested")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions
     '("def Foo():" n>
       (save-column "try:" n>
                    (save-column "with some_stuff() as foo:" n> "pass"
                                 n>))
       "except:" n> "pass"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/save-column-nested/blank.py[save-column-nested@1~after].py")))


(ert-deftest combobulate-test-python-save-column-blank-1 ()
  "Test `save-column' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "save-column")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions
     '("def Foo():" n> (save-column "try:" n> "do_something()" n>)
       "except:" n> "pass"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/save-column/blank.py[save-column@1~after].py")))


(ert-deftest
    combobulate-test-python-newline-and-indent-inside-block-both-blank-1
    ()
  "Test `newline-and-indent-inside-block-both' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags
  '(python python-ts-mode "newline-and-indent-inside-block-both")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions
     '("if 1:" n> "b = 1" n> "c = 1" n> "while True:" n> "d = 3"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/newline-and-indent-inside-block-both/blank.py[newline-and-indent-inside-block-both@1~after].py")))


(ert-deftest
    combobulate-test-python-newline-and-indent-inside-block-then-outside-blank-1
    ()
  "Test `newline-and-indent-inside-block-then-outside' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags
  '(python python-ts-mode
           "newline-and-indent-inside-block-then-outside")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions
     '("if 1:" n> "b = 1" n "c = 1"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/newline-and-indent-inside-block-then-outside/blank.py[newline-and-indent-inside-block-then-outside@1~after].py")))


(ert-deftest combobulate-test-python-newline-and-indent-simple-blank-1
    ()
  "Test `newline-and-indent-simple' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "newline-and-indent-simple")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions '("a = 1" n> "b = 1"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/newline-and-indent-simple/blank.py[newline-and-indent-simple@1~after].py")))


(ert-deftest combobulate-test-python-newline-blank-1 ()
  "Test `newline' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "newline")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions '("a = 1" n "b = 1"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/newline/blank.py[newline@1~after].py")))


(ert-deftest combobulate-test-python-string-multiple-blank-1 ()
  "Test `string-multiple' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "string-multiple")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions '("a" "b" "c"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/string-multiple/blank.py[string-multiple@1~after].py")))


(ert-deftest combobulate-test-python-string-basic-blank-1 ()
  "Test `string-basic' on `./fixtures/envelope/blank.py' at point marker number `1'."
  :tags '(python python-ts-mode "string-basic")
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelope/blank.py")
    (goto-marker 1) delete-markers
    (combobulate-envelope-expand-instructions '("test string"))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/string-basic/blank.py[string-basic@1~after].py")))


