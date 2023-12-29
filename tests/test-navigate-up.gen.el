;; This file is generated auto generated. Do not edit directly.

(require 'combobulate)

(require 'combobulate-test-prelude)

(ert-deftest
    combobulate-test-tsx-combobulate-navigate-up-module-statements
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/up-down/module-statements.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-up)
    (combobulate-for-each-marker #'combobulate-navigate-up :reverse
                                 nil)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-up-list-maybe-module-statements
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/up-down/module-statements.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-up-list-maybe)
    (combobulate-for-each-marker #'combobulate-navigate-up-list-maybe
                                 :reverse nil)))


(ert-deftest combobulate-test-tsx-combobulate-navigate-up-nested-jsx
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/up-down/nested-jsx.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-up)
    (combobulate-for-each-marker #'combobulate-navigate-up :reverse
                                 nil)))


(ert-deftest
    combobulate-test-tsx-combobulate-navigate-up-list-maybe-nested-jsx
    ()
  (combobulate-test
      (:language tsx :mode tsx-ts-mode :fixture
                 "./fixtures/up-down/nested-jsx.tsx")
    :tags '(tsx tsx-ts-mode combobulate-navigate-up-list-maybe)
    (combobulate-for-each-marker #'combobulate-navigate-up-list-maybe
                                 :reverse nil)))


