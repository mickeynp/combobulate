;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-html-combobulate-navigate-sequence-previous--element-1
    ()

  "Test `combobulate' with `fixtures/sequence/element.html' in `html-ts-mode' mode."
  (combobulate-test
      (:language html :mode html-ts-mode :fixture
		 "fixtures/sequence/element.html")
    :tags
    '(combobulate html html-ts-mode
		  combobulate-navigate-sequence-previous)
    (combobulate-test-go-to-marker 1)
    (combobulate-navigate-sequence-previous)
    (combobulate-test-assert-at-marker 1)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-sequence-previous--jsx-1
    ()

  "Test `combobulate' with `fixtures/sequence/jsx.tsx' in `tsx-ts-mode' mode."
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
		 "fixtures/sequence/jsx.tsx")
    :tags
    '(combobulate tsx tsx-ts-mode
		  combobulate-navigate-sequence-previous)
    (combobulate-test-go-to-marker 1)
    (combobulate-navigate-sequence-previous)
    (combobulate-test-assert-at-marker 1)))


