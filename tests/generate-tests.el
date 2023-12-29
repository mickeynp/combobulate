;;; gen-tests.el --- generate tests                  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'combobulate-generate-tests)
(require 'combobulate-generate-fixtures)
(require 'combobulate-generate-envelopes)


(combobulate-test-generate-tests
 "*" "navigate-next"
 (apply-partially #'combobulate-test-generate-movement-ert-test nil)
 '(combobulate-navigate-next)
 "sibling/")

(combobulate-test-generate-tests
 "*" "navigate-previous"
 (apply-partially #'combobulate-test-generate-movement-ert-test t)
 '(combobulate-navigate-previous)
 "sibling/")

;; Generate tests for all dragging nodes up or down.
(combobulate-test-generate-tests
 "*" "drag" #'combobulate-test-execute-fixture-test-fn
 '(combobulate-drag-down combobulate-drag-up)
 "sibling/")

(combobulate-test-generate-tests
 '("./fixtures/sibling/component-jsx.tsx"
   "./fixtures/sibling/def-block"
   "./fixtures/sibling/nested-blocks.py")
 "splice"
 #'combobulate-test-execute-fixture-test-fn
 '(combobulate-splice-up
   combobulate-splice-down
   combobulate-vanish-node))

(combobulate-test-generate-tests
 '("./fixtures/up-down/module-statements.tsx"
   "./fixtures/up-down/nested-jsx.tsx")
 "navigate-up"
 (apply-partially #'combobulate-test-generate-movement-ert-test nil)
 '(combobulate-navigate-up
   combobulate-navigate-up-list-maybe))

(let ((combobulate-test-fixture-delta-subdir "envelope/"))
  (combobulate-test-generate-tests
   '("./fixtures/envelope/blank.py")
   "envelope-python"
   #'combobulate-test-execute-fixture-test-fn
   (make-envelope-tests 'python)
   "envelope/"
   #'combobulate-test-build-envelope-test)
  (combobulate-test-generate-tests
   '("./fixtures/envelope/component.tsx")
   "envelope-tsx"
   #'combobulate-test-execute-fixture-test-fn
   (make-envelope-tests 'tsx)
   "envelope/"
   #'combobulate-test-build-envelope-test))

(provide 'gen-tests)
;;; gen-tests.el ends here
