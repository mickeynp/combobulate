;;; generate-fixtures.el --- quickly generate ert fixture files  -*- lexical-binding: t; -*-

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

;; Quickly generate ert fixture files

;;; Code:

(require 'combobulate-generate-tests)

(require 'bookmark)
(defconst combobulate-test-fixture-directory-bookmark "Combobulate: Fixtures")

(defun combobulate-make-test-fixture (fn)
  "Create a test fixture file for FN.

The file is created in the directory pointed to by the bookmark
`combobulate-test-fixture-directory-bookmark'.

It will by default use the entire buffer as the
fixture. Narrowing or marking a region will instead use that
snippet as the template for the snippet.

When the snippet is created, a special
`combobulate-test-fixture-mode' minor mode is loaded. It adds a
number of key bindings to aid with the insertion of Combobulate's
test point markers."
  (interactive "sFilename: ")
  (let* ((directory (or (bookmark-get-filename combobulate-test-fixture-directory-bookmark)
                        (error (format "Please create a bookmark named `%s' that points to the Combobulate fixtures directory."
                                       combobulate-test-fixture-directory-bookmark))))
         (src-buf (current-buffer))
         (src-text (save-restriction (progn (when (with-current-buffer src-buf (region-active-p))
                                              (narrow-to-region (region-beginning) (region-end)))
                                            (buffer-substring-no-properties (point-min) (point-max)))))
         (fixture-buf (find-file-noselect (concat directory fn))))
    (with-current-buffer fixture-buf
      (erase-buffer)
      (insert "-*- combobulate-test-point-overlays: nil; eval: (combobulate-test-fixture-mode t); -*-")
      (comment-line 1)
      (forward-line 1)
      (newline 2)
      (insert src-text)
      (message "Insert test point markers with `C-c C-p'")
      (hack-local-variables)
      (hack-local-variables-apply)
      (pop-to-buffer fixture-buf))))


(defun combobulate-test-generate-movement-ert-test (reverse fixture-fn action-fn output-buf)
  "Generate an ert test designed for movement for ACTION-FN on FIXTURE-FN.

The test is generated in OUTPUT-BUF. If REVERSE is non-nil, the
test will be generated for both forward and backward movement."
  (let* ((action-fn-name (symbol-name action-fn))
         (test-name nil)
         (fixture-buf (find-file-noselect fixture-fn))
         (fixture-language) (fixture-major-mode))
    (with-current-buffer fixture-buf
      (setq fixture-language (combobulate-parser-language (car (combobulate-parser-list)))
            fixture-major-mode major-mode))
    (setq test-name (intern (format "combobulate-test-%s-%s-%s"
                                    (symbol-name fixture-language)
                                    action-fn-name
                                    (file-name-base fixture-fn))))
    (with-current-buffer output-buf
      (insert
       (string-replace
        ;; I cannot work out how to get prin1 to print () instead of
        ;; nil.
        "'EMPTY"
        "()"
        (prin1-to-string
         (pp
          `(ert-deftest ,test-name 'EMPTY
             (combobulate-test (:language ,fixture-language :mode ,fixture-major-mode :fixture ,fixture-fn)
               :tags ',(list fixture-language fixture-major-mode action-fn)
               (combobulate-for-each-marker #',action-fn :reverse ,reverse))))
         t))
       "\n" "\n"))))

;;; Generators

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

(provide 'generate-fixtures)
;;; generate-fixtures.el ends here
