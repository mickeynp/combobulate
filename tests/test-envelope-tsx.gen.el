;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-tsx-choice-simple-1-component-1 ()
  "Test `choice-simple-1' on `./fixtures/envelope/component.tsx' at point marker number `1'."
  :tags '(tsx tsx-ts-mode "choice-simple-1")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/envelope/component.tsx")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let ((combobulate-envelope-proffer-choices '(1)))
              (combobulate-with-stubbed-proffer-choices
                  (:choices combobulate-envelope-proffer-choices)
                (combobulate-envelope-expand-instructions
                 '("{a + " (choice "1") (choice "2") "}"))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/choice-simple-1/component.tsx[choice-simple-1@1~after].tsx")))


(ert-deftest combobulate-test-tsx-choice-simple-0-component-1 ()
  "Test `choice-simple-0' on `./fixtures/envelope/component.tsx' at point marker number `1'."
  :tags '(tsx tsx-ts-mode "choice-simple-0")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/envelope/component.tsx")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let ((combobulate-envelope-proffer-choices '(0)))
              (combobulate-with-stubbed-proffer-choices
                  (:choices combobulate-envelope-proffer-choices)
                (combobulate-envelope-expand-instructions
                 '("{a + " (choice "1") (choice "2") "}"))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/choice-simple-0/component.tsx[choice-simple-0@1~after].tsx")))


(ert-deftest
    combobulate-test-tsx-choice*-with-complex-missing-field-component-1
    ()
  "Test `choice*-with-complex-missing-field' on `./fixtures/envelope/component.tsx' at point marker number `1'."
  :tags '(tsx tsx-ts-mode "choice*-with-complex-missing-field")
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/envelope/component.tsx")
    (goto-marker 1) delete-markers
    (combobulate-with-stubbed-prompt-expansion
        (combobulate-with-stubbed-envelope-prompt
            (let
                ((combobulate-envelope-proffer-choices '(0))
                 (combobulate-envelope-registers
                  '((region . "<div>Some jsx element</div>")))
                 (combobulate-envelope-prompt-actions '("mytag")))
              (combobulate-with-stubbed-proffer-choices
                  (:choices combobulate-envelope-proffer-choices)
                (combobulate-envelope-expand-instructions
                 '("{" @ "null" > n > " ? "
                   @ (choice* :name "consequence" :missing ("null") :rest (r>))
                   n > " : "
                   (choice* :name "alternative" :missing
                            ("<" (p other "SOME TAG") "/>") :rest
                            (r>))
                   n > "}" >))))))
    (combobulate-compare-action-with-fixture-delta
     "./fixture-deltas/envelope/choice*-with-complex-missing-field/component.tsx[choice*-with-complex-missing-field@1~after].tsx")))


