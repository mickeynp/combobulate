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
        (if (functionp action-fn)
            (funcall action-fn)
          (eval (macroexpand action-fn))))
    (signal 'missing-overlay (format "No overlay found for number %s" number))))


(defun combobulate-test-generate-ert-test-name (language action-fn-name fixture-filename &optional number)
  "Generate an ert test name for ACTION-FN-NAME on FIXTURE-FILENAME at NUMBER."
  (intern (format (if number "combobulate-test-%s-%s-%s-%s" "combobulate-test-%s-%s-%s")
                  language
                  action-fn-name
                  (file-name-base fixture-filename)
                  number)))

(defvar combobulate--test-build-fixture-delta-filename nil)
(defun combobulate-test-build-ert-stub (test-name language mode fixture-filename action-fn-name number body)
  "Build an ert test for ACTION-FN on FIXTURE-FILENAME at NUMBER.

The test is built from the stub template in `combobulate-test-ert-stub'.

The variable `combobulate--test-build-fixture-delta-filename'
contains the delta filename target."
  (insert
   (thread-last
     (prin1-to-string
      (pp
       `(ert-deftest ,test-name 'EMPTY
          ,(format "Test `%s' on `%s' at point marker number `%s'." action-fn-name fixture-filename number)
          :tags ',(list language mode action-fn-name)
          (combobulate-test (:language ,language :mode ,mode :fixture ,fixture-filename)
            ,@body)))
      t)
     (string-replace
      ;; I cannot work out how to get prin1 to print () instead of nil.
      "'EMPTY"
      "()"
      ))
   "\n" "\n"))

(defun combobulate-test-build-ert-test (test-name language mode fixture-filename number action-fn
                                                  command-error action-fn-name)
  "Build an ert test for ACTION-FN on FIXTURE-FILENAME at NUMBER."
  (combobulate-test-build-ert-stub
   test-name
   language
   mode
   fixture-filename
   action-fn-name
   number
   `((goto-marker ,number)
     ,(if command-error
          `(should-error (,action-fn))
        `(,action-fn))
     (combobulate-compare-action-with-fixture-delta
      ,(combobulate-test-get-fixture-delta-filename fixture-filename action-fn-name number)))))

(defun combobulate-test-execute-fixture-test-fn (test-builder-fn fixture-filename action-fn action-fn-name output-buffer)
  "Execute ACTION-FN with FIXTURE-FILENAME for each overlay and output to OUTPUT-BUFFER."
  (let* ((numbers)
         (buf (find-file-noselect fixture-filename))
         (command-error nil)
         (subdir (combobulate-test-get-fixture-deltas-directory action-fn-name t)))
    (with-current-buffer buf
      (setq numbers (seq-sort #'< (mapcar (lambda (ov) (overlay-get ov 'combobulate-test-number))
                                          (combobulate--with-test-overlays)))))
    (dolist (number numbers)
      (let* ((fixture-delta-filename
              (concat subdir (combobulate-test-generate-fixture-diff-filename
                              fixture-filename action-fn-name number "after")))
             (fixture-delta-buf))
        (copy-file fixture-filename fixture-delta-filename t)
        (setq fixture-delta-buf (find-file-noselect fixture-delta-filename))
        (message "🔍 Executing %s on %s" action-fn fixture-filename)
        (let ((lang-name) (test-name) (lang))
          (with-current-buffer fixture-delta-buf
            (combobulate-mode)
            (combobulate-setup)
            (setq lang (combobulate-parser-language (car (combobulate-parser-list))))
            (setq command-error nil
                  fixture-major-mode major-mode
                  lang-name (symbol-name lang)
                  test-name (combobulate-test-generate-ert-test-name
                             lang-name
                             action-fn-name
                             fixture-filename
                             number))
            (condition-case err-arg
                (combobulate-test-execute-action-from-number number action-fn)
              (error (progn (setq command-error t)
                            (message "🚫 Action function exited with an error: `%s'. Generating error test %s on %s"
                                     err-arg action-fn fixture-filename))))
            (with-current-buffer output-buffer
              (funcall test-builder-fn
                       test-name
                       lang
                       fixture-major-mode
                       fixture-filename
                       number
                       action-fn
                       command-error
                       action-fn-name))
            (message "✅ Generated test for %s on %s" action-fn fixture-filename)
            ;; avoid mode-related hook bullshit from triggering on save.
            (fundamental-mode)
            (let ((require-final-newline t))
              (save-buffer 0))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer fixture-delta-buf))))
    (kill-buffer buf)))



(defun combobulate-test-generate-tests (wildcard-or-list test-name test-executor-fn cmd-fns &optional fixture-sub-dir test-builder-fn)
  "Generate tests for CMD-FNS on TEST-NAME.

TEST-EXECUTOR-FN is a function that takes a fixture filename, the
CMD-FNS and an output buffer and builds the fixture delta (if
any) and the ert test.

WILDCARD-OR-LIST is either a wildcard string or a list of
filenames.  If it is a wildcard string, it is expanded to a list
of filenames.  If it is a list of filenames, it is used as-is.

FIXTURE-SUB-DIR is the subdirectory of the fixtures
directory. Used only when wildcards are used.

CMD-FNS is a list of functions to execute on the fixture file.
The functions are executed in order and the resulting buffer
state is compared to the fixture file.  The resulting buffer
state is saved as a fixture delta file in the fixture deltas
directory."
  (interactive)
  (save-window-excursion
    (save-excursion
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
             (target-fn (concat (combobulate-test-get-test-directory) (format "test-%s.gen.el" test-name)))
             (output-buffer (find-file-literally target-fn)))
        (message "Generating tests in %s to `%s'" (buffer-file-name output-buffer) target-fn)
        ;; assert fixture-sub-dir ends with a slash.
        (with-current-buffer output-buffer
          (erase-buffer)
          (insert ";; This file is generated auto generated. Do not edit directly.\n\n")
          (insert "(require 'combobulate)\n\n")
          (insert "(require 'combobulate-test-prelude)\n\n")
          (emacs-lisp-mode)
          (dolist (fixture-filename
                   (pcase wildcard-or-list
                     ((pred stringp)
                      (unless (string-suffix-p "/" fixture-sub-dir)
                        (error "fixture-sub-dir must end with a slash"))
                      (file-expand-wildcards (concat "./fixtures/" fixture-sub-dir wildcard-or-list)))
                     ((pred listp) wildcard-or-list)
                     (_ (error "Invalid wildcard-or-list: %s" wildcard-or-list))))
            (if (string-match-p "^#" (file-name-base fixture-filename))
                (message "⚠ Skipping fixture as it is an auto save file %s" fixture-filename)
              (pcase-dolist ((or `(,action-name . ,action-fn) `,action-fn) cmd-fns)
                (funcall test-executor-fn (or test-builder-fn #'combobulate-test-build-ert-test) fixture-filename
                         action-fn (or action-name (symbol-name action-fn)) output-buffer)))
            (save-buffer 0)))))))

;;; Generators


(provide 'combobulate-generate-tests)

