;;; combobulate-generate-envelopes.el --- generate envelope tests  -*- lexical-binding: t; -*-

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



(defun make-envelope-tests ()
  (let ((tests))
    (pcase-dolist (`(,name . ,instruction)
                   '(;; test string insertion
                     ("string-basic" . ("test string"))
                     ("string-multiple" . ("a" "b" "c"))))
      (push (cons name `(combobulate-envelope-expand-instructions
                         ',instruction))
            tests))
    tests))

(defun combobulate-test-build-envelope-test (test-name language mode fixture-filename number action-fn
                                                       command-error action-fn-name)
  (combobulate-test-build-ert-stub
   test-name
   language
   mode
   fixture-filename
   action-fn-name
   `((goto-marker ,number)
     ,action-fn
     (combobulate-compare-action-with-fixture-delta ,number ,action-fn-name ,fixture-filename))))

(combobulate-test-generate-tests
 '("./fixtures/envelop/blank.py")
 "envelop"
 #'combobulate-test-execute-fixture-test-fn
 (make-envelope-tests)
 "envelope/"
 #'combobulate-test-build-envelope-test)

(provide 'combobulate-generate-envelopes)
;;; combobulate-generate-envelopes.el ends here
