;;; generate-tests.el --- generate ert tests from fixtures using code generation  -*- lexical-binding: t; -*-

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

;; Generate ert tests from fixtures using code generation.
;;
;; `combobulate-test-generate-tests' takes a list of functions to
;; execute on a fixture file. The functions are executed in order and
;; for each combobulate point marker placed in the file and the
;; resulting buffer state is saved as a fixture delta file in the
;; fixture deltas directory. Finally, an ert test is generated for the
;; each point marker and for each function.
;;
;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)

(defun combobulate-test-execute-action-from-number (number action-fn)
  "Execute ACTION-FN on the overlay with NUMBER."
  (if (combobulate--test-get-overlay-by-number number)
      (progn
        (combobulate-test-go-to-overlay number)
        (funcall action-fn))
    (signal 'missing-overlay (format "No overlay found for number %s" number))))

(defun combobulate-test-generate-ert-test (output-buf language mode fixture-fn number action-fn command-error)
  "Generate an ert test for ACTION-FN on FIXTURE-FN at NUMBER.

The test is generated in OUTPUT-BUF.

If COMMAND-ERROR is non-nil, the test will be generated as an error
test using `should-error'; otherwise, it is assumed the test must
pass with `should'."
  (let* ((action-fn-name (symbol-name action-fn))
         (subdir (format "../fixture-deltas/%s/" action-fn-name))
         (test-name (intern (format "combobulate-test-%s-%s-%s-number-%s"
                                    (symbol-name language)
                                    action-fn-name
                                    (file-name-base fixture-fn)
                                    number))))
    (with-current-buffer output-buf
      (insert
       (string-replace
        ;; I cannot work out how to get prin1 to print () instead of nil.
        "'EMPTY"
        "()"
        (prin1-to-string
         (pp
          `(ert-deftest ,test-name 'EMPTY
             (combobulate-test (:language ,language :mode ,mode :fixture ,fixture-fn)
               :tags ',(list (symbol-name language) (symbol-name mode) action-fn-name)
               (goto-marker ,number)
               ,(if command-error
                    `(should-error (,action-fn))
                  `(,action-fn))
               (combobulate-test-fixture-action-function ,number #',action-fn ,fixture-fn))))
         t))
       "\n" "\n"))))

(defun combobulate-test-execute-test-fn (fixture-fn action-fn output-buffer)
  "Execute ACTION-FN on FIXTURE-FN and generate an ert test in OUTPUT-BUFFER."
  (let* ((numbers)
         (buf (find-file-noselect fixture-fn))
         (action-name (symbol-name action-fn))
         (command-error nil)
         (subdir (combobulate-test-get-fixture-deltas-directory action-name t)))
    (with-current-buffer buf
      (setq numbers (seq-sort #'< (mapcar (lambda (ov) (overlay-get ov 'combobulate-test-number))
                                          (combobulate--with-test-overlays)))))
    (dolist (number numbers)
      (let* ((after-fn (concat subdir
                               (combobulate-test-generate-fixture-diff-filename
                                fixture-fn action-name number "after")))
             (after-buf))
        (copy-file fixture-fn after-fn t)
        (setq after-buf (find-file-noselect after-fn))
        (message "🔍 Executing %s on %s" action-fn fixture-fn)
        (with-current-buffer after-buf
          (combobulate-mode)
          (combobulate-setup)
          (setq command-error nil)
          (condition-case nil
              (combobulate-test-execute-action-from-number number action-fn)
            (error (progn (setq command-error t)
                          (message "🚫 Action function exited with an error. Generating error test %s on %s" action-fn fixture-fn))))
          (combobulate-test-generate-ert-test
           output-buffer
           (combobulate-parser-language (car (combobulate-parser-list)))
           major-mode
           fixture-fn
           number
           action-fn
           command-error)
          (message "✅ Generated test for %s on %s" action-fn fixture-fn)
          ;; avoid mode-related hook bullshit from triggering on save.
          (fundamental-mode)
          (let ((require-final-newline t))
            (save-buffer 0)
            (save-excursion
              (unless (/= (char-after (1- (point-max))) ?\n)
		(goto-char (point-max))
		(insert ?\n)))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer after-buf))))
    (kill-buffer buf)))


(defun combobulate-test-generate-tests (wildcard-or-list test-name test-executor-fn &rest cmd-fns)
  "Generate tests for CMD-FNS on TEST-NAME.

TEST-EXECUTOR-FN is a function that takes a fixture filename, the
CMD-FNS and an output buffer and builds the fixture delta (if
any) and the ert test.

CMD-FNS is a list of functions to execute on the fixture file.
The functions are executed in order and the resulting buffer
state is compared to the fixture file.  The resulting buffer
state is saved as a fixture delta file in the fixture deltas
directory."
  (interactive)
  (let* (;; required to ensure the right major mode is chosen.
         (major-mode-remap-alist '((python-mode . python-ts-mode)
                                   (css-mode . css-ts-mode)
                                   (typescript-mode . tsx-ts-mode)
                                   (js2-mode . js-ts-mode)
                                   (bash-mode . bash-ts-mode)
                                   (css-mode . css-ts-mode)
                                   (yaml-mode . yaml-ts-mode)
                                   (json-mode . json-ts-mode)))
         (combobulate-flash-node nil)
         (delay-mode-hooks t)
         (target-fn (concat (combobulate-test-get-test-directory) (format "test-%s.gen.el" test-name)))
         (output-buffer (find-file-literally target-fn)))
    (message "Generating tests in %s to `%s'" (buffer-file-name output-buffer) target-fn)
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert ";; This file is generated auto generated. Do not edit directly.\n\n")
      (insert "(require 'combobulate)\n\n")
      (insert "(require 'combobulate-test-prelude)\n\n")
      (emacs-lisp-mode)
      (dolist (fixture-fn
               (pcase wildcard-or-list
                 ((pred stringp) (file-expand-wildcards (concat "./fixtures/" wildcard-or-list)))
                 ((pred listp) wildcard-or-list)
                 (_ (error "Invalid wildcard-or-list: %s" wildcard-or-list))))
        (if (string-match-p "^#" (file-name-base fixture-fn))
            (message "⚠ Skipping fixture as it is an auto save file %s" fixture-fn)
          (dolist (action-fn cmd-fns)
            (funcall test-executor-fn fixture-fn action-fn output-buffer)))
        (save-buffer 0)))))

;;; Generators

;; Generate tests for all dragging nodes up or down.
(combobulate-test-generate-tests "*" "drag" #'combobulate-test-execute-test-fn #'combobulate-drag-down #'combobulate-drag-up)
(combobulate-test-generate-tests
 '("./fixtures/component-jsx.tsx"
   "./fixtures/def-block"
   "./fixtures/nested-blocks.py")
 "splice"
 #'combobulate-test-execute-test-fn
 #'combobulate-splice-up
 #'combobulate-splice-down
 #'combobulate-vanish-node)
(provide 'combobulate-generate-tests)
