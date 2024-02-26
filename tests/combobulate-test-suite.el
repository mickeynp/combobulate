;;; combobulate-test-suite --- code generate combobulate tests  -*- lexical-binding: t; -*-

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

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'eieio)
(eval-when-compile (require 'cl-generic))

(defclass combobulate-test-base ()
  ((fixture-sub-dir :initarg :fixture-sub-dir
                    :initform "fixtures/"
                    :custom string
                    :documentation "The sub-directory to use to find fixture files.")
   (action-body :initarg :action-body
                :initform nil
                :custom (list symbol)
                :documentation "The actions to take in the test.")
   (reverse :initarg :reverse
            :initform nil
            :custom boolean
            :documentation "Whether to reverse the order of the markers.")
   (collection-name :initarg :collection-name
                    :custom string
                    :documentation "Name of the collection for the test suite.")
   (output-buffer :initarg :output-buffer
                  :initform "*combobulate-test-gen*"
                  :custom string
                  :documentation "The buffer to write the test files to.")
   (per-marker :initarg :per-marker
               :initform nil
               :custom boolean
               :documentation "Whether to generate a test for each marker.")
   (harness-factory :initarg :harness-factory
                    :initform nil
                    :custom symbol
                    :documentation "The factory to use to generate the test harness. It must be a `combobulate-test-suite'-derived class.")
   (harness-factory-args :initarg :harness-factory-args
                         :initform nil
                         :custom list
                         :documentation "The arguments to pass to the test harness.")
   (harness-factory-matrix :initarg :harness-factory-matrix
                           :initform nil
                           :custom list
                           :documentation "A matrix of harness-factory-args to use for the test suite.")))

(defclass combobulate-test-harness (combobulate-test-base)
  ((language :initarg :language
             :initform ""
             :custom symbol
             :documentation "The tree-sitter language grammar to in the `combobulate-fixture'.")
   (major-mode :initarg :major-mode
               :initform ""
               :custom symbol
               :documentation "The major mode to use in the `combobulate-fixture'.")
   (fixture-file-name :initarg :fixture-file-name
                      :initform ""
                      :custom string
                      :documentation "The name of the fixture file to use in the `combobulate-fixture'.")

   (marker-number :initarg :marker-number
                  :initform nil
                  :custom (integer (list integer))
                  :documentation "The test marker(s) that are used in this test, if any.")
   (total-markers :initarg :total-markers
                  :initform nil
                  :custom integer
                  :documentation "The total number of markers in the fixture file.")
   (fixture-buffer :initarg :fixture-buffer
                   :initform nil
                   :custom buffer
                   :documentation "The buffer to use for the fixture file.")
   (command-error :initarg :command-error
                  :initform nil
                  :custom boolean
                  :documentation "Whether the action should raise an error.")
   (test-name :initarg :test-name
              :initform ""
              :custom string
              :documentation "The name of the test. May be blank.")
   (suite :initarg :suite
          :initform nil
          :custom combobulate-test-suite
          :documentation "The test suite to use for the test."))
  "A class to generate `ert-deftest' forms for `combobulate'.")


(defun combobulate-test-get-fixture-deltas-directory (target-dir action-subdir &optional ensure-dir)
  "Get the directory for the fixture deltas.

If ACTION-SUBDIR is non-nil, then the directory will be
<fixture-directory>/<action-subdir>/, otherwise it will be
<fixture-directory>/.

If ENSURE-DIR is non-nil, then the directory will be created if it
doesn't exist."
  (let* ((dir (format "./%sfixture-deltas/%s/" target-dir action-subdir)))
    (when ensure-dir
      (make-directory dir t))
    dir))

(cl-defmethod combobulate-test-harness-fixture-file-name ((obj combobulate-test-harness) suffix)
  "Return the name of the test harness."
  (with-slots (collection-name test-name suite language fixture-file-name marker-number) obj
    (concat
     (file-name-nondirectory fixture-file-name)
     "[" (format "%s@%s~%s" (or test-name collection-name) marker-number suffix) "]"
     "."
     (file-name-extension fixture-file-name))))

(cl-defmethod combobulate-test-harness-extend-action-body ((obj combobulate-test-harness))
  (with-slots (command-error action-body fixture-delta-file-name marker-number) obj
    `((combobulate-test-go-to-marker ,marker-number)
      ,(if command-error
           `(should-error ,@action-body)
         `,@action-body))))

(defclass combobulate-test-harness-marker-loop (combobulate-test-harness)
  ()
  "A class to generate `ert-deftest' forms for `combobulate' with a marker loop.")

(cl-defmethod combobulate-test-harness-extend-action-body ((obj combobulate-test-harness-marker-loop))
  (with-slots (action-body command-error fixture-delta-file-name marker-number total-markers reverse) obj
    (princ (format "Marker number: %s / %s\n" marker-number total-markers))
    (when (and (>= marker-number 1) (< marker-number total-markers))
      `(,@(if (member marker-number command-error)
              `((should-error
                 (progn ,@action-body)))
            `((combobulate-test-go-to-marker ,marker-number)
              ,@action-body
              ,(if reverse
                   (if (> marker-number 1)
                       `(combobulate-test-assert-at-marker ,(1- marker-number))
                     `(combobulate-test-assert-at-marker ,marker-number))
                 (if (< marker-number total-markers)
                     `(combobulate-test-assert-at-marker ,(1+ marker-number))
                   `(combobulate-test-assert-at-marker ,marker-number)))))))))


(defclass combobulate-test-harness-with-fixture-delta (combobulate-test-harness)
  ((fixture-delta-file-name :initarg :fixture-delta-file-name
                            :initform ""
                            :custom string
                            :documentation "The name of the fixture delta file to use in the `combobulate-fixture'."))
  "A class to generate `ert-deftest' forms for `combobulate' with a fixture delta.")

(defun combobulate-test-harness-marker-and-error-handler (marker-number command-error body)
  (if command-error
      `((should-error
         (progn (combobulate-test-go-to-marker ,marker-number)
                ,@body)))
    `((combobulate-test-go-to-marker ,marker-number)
      ,@body)))

(cl-defmethod combobulate-test-harness-extend-action-body ((obj combobulate-test-harness-with-fixture-delta))
  "Build the test harness and ert-deftest form for the test."
  (with-slots (command-error action-body fixture-delta-file-name marker-number) obj
    (combobulate-test-harness-marker-and-error-handler marker-number command-error action-body)))

(defclass combobulate-test-harness-envelope (combobulate-test-harness-with-fixture-delta)
  ((mock-proffer-choices :initarg :mock-proffer-choices
                         :initform nil
                         :custom list
                         :documentation "The choices to use, in order, for the envelope proffer choices mock.")
   (mock-prompt-actions :initarg :mock-prompt-actions
                        :initform nil
                        :custom (list (or symbol string))
                        :documentation "The inputs to give the envelope prompt mock, in order.")
   (mock-expansion-actions :initarg :mock-expansion-actions
                           :initform nil
                           :custom (list symbol)
                           :documentation "The inputs to give the envelope choice prompt mock, in order.")
   (instructions :initarg :instructions
                 :initform nil
                 :custom list
                 :documentation "The instructions to use for the envelope mock.")
   (mock-registers :initarg :mock-registers
                   :initform nil
                   :custom (list ((or symbol string) (or symbol string)))
                   :documentation "The registers to use for the envelope register mock."))
  "Generate tests with mocked envelope user action features.")

(cl-defmethod combobulate-test-harness-extend-action-body ((obj combobulate-test-harness-envelope))
  (with-slots (command-error action-body fixture-delta-file-name marker-number
                             mock-proffer-choices mock-prompt-actions mock-expansion-actions mock-registers
                             instructions)
      obj
    (combobulate-test-harness-marker-and-error-handler
     marker-number command-error
     `((let ((combobulate-envelope-proffer-choices ',mock-proffer-choices)
             (combobulate-envelope-prompt-actions ',mock-prompt-actions)
             (combobulate-envelope-expansion-actions ',mock-expansion-actions)
             (combobulate-envelope-registers ',mock-registers)
             (instructions ',instructions))
         (combobulate-with-stubbed-prompt-expansion
             (combobulate-with-stubbed-envelope-prompt
                 (combobulate-with-stubbed-proffer-choices
                     (:choices combobulate-envelope-proffer-choices)
                   ,@(combobulate-test-harness-marker-and-error-handler
                      marker-number command-error
                      action-body)))))))))

(cl-defmethod combobulate-test-harness-test-name ((obj combobulate-test-harness))
  "Return the name of the test harness."
  (with-slots (collection-name test-name suite language fixture-file-name) obj
    (intern (string-join
             (list
              "combobulate-test"
              (symbol-name language)
              collection-name
              test-name
              (file-name-base fixture-file-name)
              (format "%s" (or (oref obj marker-number) "")))
             "-"))))

(cl-defmethod combobulate-test-harness-fixture-directory ((obj combobulate-test-harness))
  "Return the directory where the fixture files are located."
  (oref obj fixture-sub-dir))

(defclass combobulate-test-suite (combobulate-test-base)
  ;; we want a list of glob patterns for the fixture templates we
  ;; should use; a function symbol for the function to call in the
  ;; test
  ((fixture-files :initarg :fixture-files
                  :initform ""
                  :custom (or string (list string))
                  :documentation "The wildcard or list of wildcards to use to find fixture files.")
   (target-dir :initarg :target-dir
               :initform ""
               :custom string
               :documentation "The directory to write the test files to.")))

(cl-defmethod combobulate-test-harness-create-ert-test-form ((obj combobulate-test-harness) &optional skip-extend)
  "Generate a test HARNESS and return the string representation of the test."
  (with-slots (collection-name language major-mode fixture-file-name action-body) obj
    (let ((test-name (combobulate-test-harness-test-name obj))
          (extended-action-body (if skip-extend action-body (combobulate-test-harness-extend-action-body obj)))
          (fixture-language language)
          (fixture-major-mode major-mode)
          (collection (intern collection-name))
          (print-level nil)
          (print-length nil)
          (print-circle nil)
          (docstring (format "Test `combobulate' with `%s' in `%s' mode."
                             fixture-file-name
                             major-mode)))
      (concat
       (string-replace
        ;; I cannot work out how to get prin1 to print () instead of
        ;; nil.
        "'EMPTY"
        "()\n"
        (prin1-to-string
         (pp
          `(ert-deftest ,test-name 'EMPTY
             ,docstring
             (combobulate-test (:language ,fixture-language :mode ,fixture-major-mode :fixture ,fixture-file-name)
               :tags ',(list 'combobulate fixture-language fixture-major-mode collection)
               ,@extended-action-body)))
         t))
       "\n\n"))))

;;; method that generates a test harness for every fixture-files entry
(cl-defmethod combobulate-test-suite-generate-test-suite ((obj combobulate-test-suite))
  "Generate a test harness for every fixture-files entry."
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
                                       (json-mode . json-ts-mode)
                                       (html-mode . html-ts-mode)
                                       (mhtml-mode . html-ts-mode)
                                       (js-json-mode . json-ts-mode)))
             (combobulate-flash-node nil)
             (target-fn (concat (oref obj target-dir) (format "test-%s.gen.el" (oref obj collection-name))))
             (current-directory default-directory)
             (output-buffer (find-file-literally target-fn)))
        (message "Generating tests in %s to `%s'" (buffer-file-name output-buffer) target-fn)
        (setf (oref obj output-buffer) output-buffer)
        (with-current-buffer output-buffer
          (erase-buffer)
          (insert ";; This file is generated auto generated. Do not edit directly.\n\n")
          (insert "(require 'combobulate)\n\n")
          (insert "(require 'combobulate-test-prelude)\n\n")
          (emacs-lisp-mode)
          (let ((wildcards (oref obj fixture-files)))
            (dolist (wildcard (cond ((stringp wildcards) (list wildcards))
                                    (t wildcards)))
              (dolist (fixture-file-name (file-expand-wildcards
                                          (concat current-directory wildcard)))
                ;; skip backup and autosave files
                (unless (string-match-p "\\(?:\\.#\\|~\\)$" fixture-file-name)
                  (combobulate-test-suite-create-harness obj fixture-file-name)))))
          (save-buffer))))))

(defun combobulate-test-execute-action (action)
  (if (functionp action)
      (funcall action)
    (eval (macroexpand action))))

(defun combobulate-test-execute-action-from-number (number action)
  "Execute ACTION on the overlay with NUMBER."
  (if (combobulate--test-get-overlay-by-number number)
      (progn
        (combobulate-test-go-to-overlay number)
        (combobulate-test-execute-action action))
    (signal 'missing-overlay (format "No overlay found for number %s" number))))

(cl-defmethod combobulate-test-harness-run-and-write-test ((obj combobulate-test-harness))
  (with-slots (collection-name fixture-buffer fixture-file-name marker-number output-buffer action-body command-error)
      obj
    (setf (oref obj command-error) nil)
    (let ((ext-body (combobulate-test-harness-extend-action-body obj)))
      (with-current-buffer fixture-buffer
        (combobulate-mode)
        (combobulate-setup)
        (condition-case err-arg
            (progn
              (eval `(progn ,@(macroexpand ext-body)))
              (setf (oref obj command-error) nil))
          (error (progn (setf (oref obj command-error) t)
                        (message "⚠️ Action function exited with an error: `%s'. Generating error test for fixture `%s'."
                                 err-arg fixture-file-name))))
        (with-current-buffer output-buffer
          (insert (combobulate-test-harness-create-ert-test-form obj)))))))

(cl-defmethod combobulate-test-harness-run-and-write-test ((obj combobulate-test-harness-marker-loop))
  (with-slots (collection-name
               total-markers reverse
               fixture-buffer fixture-file-name
               output-buffer action-body command-error)
      obj
    (setf (oref obj command-error) nil)
    (let ((expanded) (current-form) (numbers (number-sequence 1 total-markers)))
      (with-current-buffer fixture-buffer
        (combobulate-mode)
        (combobulate-setup)
        (setf (oref obj total-markers) total-markers)
        (dolist (i (if reverse (reverse numbers) numbers))
          (setf (oref obj marker-number) i)
          (setq current-form (macroexpand (combobulate-test-harness-extend-action-body obj)))
          (when current-form
            (condition-case err-arg
                (eval `(progn ,@current-form))
              (error
               (setq current-form nil)
               (message "⚠️ Action function exited with an error: `%s'. Generating error test for fixture `%s'."
                        err-arg fixture-file-name)
               (setf (oref obj command-error) (cons i (oref obj command-error)))))
            (setq expanded (append expanded (macroexpand (combobulate-test-harness-extend-action-body obj))))))
        (with-current-buffer output-buffer
          (setf (oref obj action-body) expanded)
          (insert (combobulate-test-harness-create-ert-test-form obj t)))))))


(cl-defmethod combobulate-test-harness-run-and-write-test ((obj combobulate-test-harness-with-fixture-delta))
  (with-slots (collection-name fixture-sub-dir fixture-file-name action-body output-buffer marker-number)
      obj
    (let* ((fixture-delta-buf)
           (command-error)
           (ext-body)
           (fixture-delta-dir (combobulate-test-get-fixture-deltas-directory (oref (oref obj suite) target-dir) collection-name t))
           (fixture-delta-file-name
            (concat fixture-delta-dir (combobulate-test-harness-fixture-file-name obj "after"))))
      (copy-file fixture-file-name fixture-delta-file-name t)
      (setq fixture-delta-buf (find-file-noselect fixture-delta-file-name))
      (setf (oref obj fixture-delta-file-name) fixture-delta-file-name)
      ;; set it after we've updated the delta file name.
      (setq ext-body (combobulate-test-harness-extend-action-body obj))
      (with-current-buffer fixture-delta-buf
        (combobulate-mode)
        (combobulate-setup)
        (condition-case err-arg
            (progn
              (eval `(progn ,@(macroexpand ext-body)))
              (setf (oref obj command-error) nil))
          (error (progn (setf (oref obj command-error) t)
                        (message "⚠️ Action function exited with an error: `%s'. Generating error test for fixture `%s'."
                                 err-arg fixture-file-name))))
        (setf (oref obj action-body)
              (append (oref obj action-body)
                      `((combobulate-compare-action-with-fixture-delta ,fixture-delta-file-name))))
        (with-current-buffer output-buffer
          (insert (combobulate-test-harness-create-ert-test-form obj)))
        ;; avoid mode-related hook BS from triggering on save.
        (fundamental-mode)
        (let ((require-final-newline t))
          (save-buffer 0)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer fixture-delta-buf)))))

(cl-defmethod combobulate-test-suite-create-harness ((obj combobulate-test-suite) fixture-file-name)
  (with-current-buffer (oref obj output-buffer)
    (let* ((numbers)
           (fixture-buf (find-file-noselect fixture-file-name))
           (fixture-language) (fixture-major-mode) (lang))
      (with-current-buffer fixture-buf
        (setq lang (car (combobulate-parser-list)))
        (cl-assert (not (null lang)) nil "No language found in `%s' (major mode: `%s')" fixture-file-name major-mode)
        (setq fixture-language (combobulate-parser-language lang))
        (setf fixture-major-mode major-mode)
        (setq numbers (seq-sort #'< (mapcar (lambda (ov) (overlay-get ov 'combobulate-test-number))
                                            (combobulate--with-test-overlays)))))
      (cl-assert (not (null numbers)) nil "No markers found in `%s'" fixture-file-name)
      (unwind-protect
          (dolist (number (if (oref obj reverse) (reverse numbers) numbers))
            (if (or (oref obj per-marker)
                    (and (not (oref obj per-marker)) (= number 1)))
                (let ((matrix-args (or (oref obj harness-factory-matrix)
                                       (list (oref obj harness-factory-args)))))
                  (dolist (harness-factory-args matrix-args)
                    (combobulate-test-harness-run-and-write-test
                     (apply (oref obj harness-factory)
                            (append
                             (list :fixture-file-name (file-relative-name fixture-file-name default-directory)
                                   :fixture-sub-dir (oref obj target-dir)
                                   :fixture-buffer fixture-buf
                                   :language fixture-language
                                   :major-mode fixture-major-mode
                                   :reverse (oref obj reverse)
                                   :suite obj
                                   :action-body (oref obj action-body)
                                   :marker-number number
                                   :total-markers (length numbers)
                                   :output-buffer (oref obj output-buffer)
                                   :collection-name (oref obj collection-name))
                             harness-factory-args)))))))
        (kill-buffer fixture-buf)))))

(provide 'combobulate-test-suite)
;;; combobulate-test-suite ends here
