;;; test-misc.el --- misc. tests for Combobulate     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mickey Petersen

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

(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-combobulate-refactor-raise-uncommitted-error ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (should-error
       (combobulate-refactor (:id 'test)
         (combobulate-test-go-to-marker 1)
         (mark-node-highlighted (combobulate-node-at-point)))
       :type '(combobulate-refactor-uncommitted-changes))
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-combobulate-refactor-rollback ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (combobulate-refactor (:id 'test)
        (combobulate-test-go-to-marker 1)
        (mark-node-highlighted (combobulate-node-at-point))
        (rollback))
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-combobulate-refactor-commit ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (combobulate-refactor (:id 'test)
        (combobulate-test-go-to-marker 1)
        (mark-node-highlighted (combobulate-node-at-point))
        (commit))
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-combobulate-refactor-error ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (should-error
       (combobulate-refactor (:id 'test)
         (combobulate-test-go-to-marker 1)
         (mark-node-highlighted (combobulate-node-at-point))
         (error "Some error"))
       :type 'error)
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-combobulate-refactor-nested-but-different-sessions ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (should-error
       (combobulate-refactor (:id 'test)
         (combobulate-test-go-to-marker 1)
         (mark-node-highlighted (combobulate-node-at-point))
         (combobulate-refactor (:id 'nested)
           (combobulate-test-go-to-marker 1)
           (mark-node-highlighted (combobulate-node-at-point))
           (error "Some error")))
       :type 'error)
      (should-not (alist-get 'test combobulate-refactor--active-sessions))
      (should-not (alist-get 'nested combobulate-refactor--active-sessions)))))

(ert-deftest combobulate-test-combobulate-refactor-nested-same-sessions ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (combobulate-refactor (:id 'test)
        (mark-range-label 1 1 "foo")
        (combobulate-refactor (:id 'test)
          (combobulate-test-go-to-marker 1)
          (mark-node-highlighted (combobulate-node-at-point))
          (combobulate-refactor (:id 'test)
            (combobulate-test-go-to-marker 1)
            (mark-node-highlighted (combobulate-node-at-point)))
          (commit)))
      (should-not (alist-get 'test combobulate-refactor--active-sessions)))))

(ert-deftest combobulate-test-combobulate-refactor-nested-same-sessions-unhandled-error ()
  :tags '(refactor combobulate)
  (combobulate-test (:language python :mode python-ts-mode :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-refactor--active-sessions))
      (condition-case err
          (combobulate-refactor (:id 'test)
            (mark-range-label 1 1 "foo")
            (combobulate-refactor (:id 'test)
              (combobulate-test-go-to-marker 1)
              (mark-node-highlighted (combobulate-node-at-point))
              (combobulate-refactor (:id 'test)
                (combobulate-test-go-to-marker 1)
                (mark-node-highlighted (combobulate-node-at-point)))
              (error "foo")))
        (error nil))
      (should-not (alist-get 'test combobulate-refactor--active-sessions)))))


(provide 'test-misc)
;;; test-misc.el ends here
