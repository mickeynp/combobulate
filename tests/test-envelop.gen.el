;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-python-string-multiple-blank-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelop/blank.py")
    :tags '(python python-ts-mode "string-multiple") (goto-marker 1)
    (combobulate-envelope-expand-instructions '("a" "b" "c"))
    (combobulate-compare-action-with-fixture-delta 1 "string-multiple"
                                                   "./fixtures/envelop/blank.py")))


(ert-deftest combobulate-test-python-string-basic-blank-1 ()
  (combobulate-test
      (:language python :mode python-ts-mode :fixture
                 "./fixtures/envelop/blank.py")
    :tags '(python python-ts-mode "string-basic") (goto-marker 1)
    (combobulate-envelope-expand-instructions '("test string"))
    (combobulate-compare-action-with-fixture-delta 1 "string-basic"
                                                   "./fixtures/envelop/blank.py")))


