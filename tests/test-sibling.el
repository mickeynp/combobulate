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


(cl-defun combobulate-for-each-marker (action-fn &key (reverse nil))
  "Execute ACTION-FN for each marker in the current buffer.

The point is first moved to the start of the first marker. After
that, all remaining point markers are visited in order of their
given number. ACTION-FN is executed *before* each marker is
visited; then, the new position of point is checked to ensure it
matches the position of the next marker.

If REVERSE is non-nil, execute ACTION-FN in reverse order."
  (let ((ordered-ovs))
    (dotimes (number (length combobulate-test-point-overlays))
      (when-let (ov (combobulate--test-get-overlay-by-number (1+ number)))
        (push ov ordered-ovs)))
    (unless reverse
      (setq ordered-ovs (reverse ordered-ovs)))
    (let ((first-ov (pop ordered-ovs)))
      (goto-char (overlay-start first-ov))
      (should (= (point) (overlay-start first-ov)))
      (dolist (ov ordered-ovs)
        (funcall action-fn)
        (should (= (point) (overlay-start ov)))))))

;;; Python

(ert-deftest combobulate-test-sibling-python-def-block ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/def-block.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-def-parameters ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/def-parameters.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-module-statements ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/module-statements.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

;;; Typescript

(ert-deftest combobulate-test-sibling-typescript-component-jsx ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/component-jsx.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-module-statements ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/module-statements.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-def-function-block ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/def-function-block.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-def-function-object-args ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/def-function-object-args.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-typescript-def-function-type-args ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/def-function-type-args.tsx")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-css-property ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/css-property.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-css-declaration ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/css-declaration.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


(ert-deftest combobulate-test-sibling-css-css-nested-statements ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/css-nested-statements.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-css-css-function-arg ()
  (combobulate-test (:language css :mode css-ts-mode :fixture "./fixtures/css-function-arg.css")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


(ert-deftest combobulate-test-sibling-yaml-yaml-block-mapping ()
  (combobulate-test (:language yaml :mode yaml-ts-mode :fixture "./fixtures/yaml-block-mapping.yaml")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-yaml-yaml-block-mapping-pairs ()
  (combobulate-test (:language yaml :mode yaml-ts-mode :fixture "./fixtures/yaml-block-mapping-pairs.yaml")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-yaml-yaml-sequence ()
  (combobulate-test (:language yaml :mode yaml-ts-mode :fixture "./fixtures/yaml-sequence.yaml")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


(ert-deftest combobulate-test-sibling-python-python-list ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/python-list.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-tuple ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/python-tuple.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-tuple-pattern ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/python-tuple-pattern.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-set ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/python-set.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-dict ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/python-dict.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))

(ert-deftest combobulate-test-sibling-python-python-match-case ()
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/python-match-case.py")
    (combobulate-for-each-marker #'combobulate-navigate-next)
    (combobulate-for-each-marker #'combobulate-navigate-previous :reverse t)))


