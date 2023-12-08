;;; test-sibling.el --- tests for sibling navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for sibling and sibling-related features.
;;

;;; Code:


;;; Python

(ert-deftest combobulate-test-sibling-python-def-block ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-block.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-def-parameters ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/def-parameters.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-module-statements ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/module-statements.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

;;; Typescript

(ert-deftest combobulate-test-sibling-typescript-component-jsx ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/component-jsx.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-module-statements ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/module-statements.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-def-function-block ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-block.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-def-function-object-args ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-object-args.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-def-function-type-args ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/sibling/def-function-type-args.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-css-property ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-property.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-css-declaration ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-declaration.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


(ert-deftest combobulate-test-sibling-css-css-nested-statements ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-nested-statements.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-css-css-function-arg ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/sibling/css-function-arg.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


(ert-deftest combobulate-test-sibling-yaml-yaml-block-mapping ()
  (combobulate-test (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping.yaml")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-yaml-yaml-block-mapping-pairs ()
  (combobulate-test (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-block-mapping-pairs.yaml")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-yaml-yaml-sequence ()
  (combobulate-test (:language yaml :mode yaml-ts-mode :fixture "./fixtures/sibling/yaml-sequence.yaml")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


(ert-deftest combobulate-test-sibling-python-python-list ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-list.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-tuple ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-tuple-pattern ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-tuple-pattern.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-set ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-set.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-dict ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-dict.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-match-case ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/python-match-case.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


