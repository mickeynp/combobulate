;;; test-navigation.el --- tests for navigation  -*- lexical-binding: t; -*-

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
(require 'combobulate-navigation)

(ert-deftest combobulate-procedure-test-linear-sibling ()
  :tags '(combobulate navigation)
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/sibling/top-level.py")
    (goto-marker 1)
    (let ((node (combobulate-node-at-point '("comment"))))
      (should (equal (combobulate-linear-siblings node)
                     (combobulate-node-children (combobulate-root-node))))
      )))
