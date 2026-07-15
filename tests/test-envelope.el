;;; test-envelope.el --- envelope integration tests for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Mickey Petersen

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

;; Integration tests for envelope discovery, applicability, command
;; definition, and execution.

;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-envelope-get-envelope-by-name ()
  :tags '(envelope combobulate)
  (let* ((target '(:name "target" :description "Target envelope"))
         (combobulate-python-envelope-list
          `((:name "first" :description "First envelope")
            ,target)))
    (cl-letf (((symbol-function 'combobulate-primary-language)
               (lambda () 'python)))
      (should (eq (combobulate-get-envelope-by-name "target") target))
      (should-not (combobulate-get-envelope-by-name "missing")))))

(ert-deftest combobulate-test-envelope-get-applicable-nodes ()
  :tags '(envelope combobulate)
  (should-error
   (combobulate-envelope-get-applicable-nodes
    '(:name "invalid" :nodes ("identifier") :shorthand expression))
   :type 'error)
  (with-temp-buffer
    (insert "abcdef")
    (goto-char 3)
    (let* ((early-node (combobulate-proxy-node-make-from-range 2 3 "early"))
           (late-node (combobulate-proxy-node-make-from-range 5 6 "late"))
           (procedure '(:activation-nodes ((:nodes ("call")))))
           (envelope `(:name "applicable"
                       :nodes ("identifier" ,procedure)))
           (calls))
      (cl-letf (((symbol-function 'combobulate-procedure-start)
                 (lambda (pt procedures exhaustive)
                   (push (list pt procedures exhaustive) calls)
                   (cond
                    ((equal procedures (list procedure))
                     (list (combobulate-procedure-result-create
                            :action-node late-node)))
                    ((equal procedures
                            '((:activation-nodes
                               ((:nodes ("identifier"))))))
                     (list (combobulate-procedure-result-create
                            :action-node early-node)))
                    (t (ert-fail (format "Unexpected procedures: %S"
                                         procedures)))))))
        (should (equal (mapcar (lambda (node)
                                 (marker-position
                                  (combobulate-node-start node)))
                               (combobulate-envelope-get-applicable-nodes
                                envelope))
                       '(5 2)))
        (should (= (length calls) 2))
        (should (equal (mapcar #'car calls) '(3 3)))
        (should (seq-every-p (lambda (call) (nth 2 call)) calls))))))

(ert-deftest combobulate-test-envelope-define-envelope ()
  :tags '(envelope combobulate)
  (let ((combobulate-python-envelope-map (make-sparse-keymap))
        (combobulate-python-map (make-sparse-keymap))
        (invoked-envelope))
    (cl-letf (((symbol-function 'combobulate-primary-language)
               (lambda () 'python))
              ((symbol-function 'combobulate-execute-envelope)
               (lambda (name &optional _node _force)
                 (setq invoked-envelope name))))
      (let* ((envelope '(:name "test envelope command"
                         :description "Test envelope command"
                         :template ("test")
                         :key "t"
                         :extra-key "C-c t"))
             (command (combobulate--envelope-get-function-name envelope)))
        (unwind-protect
            (progn
              (should-not (fboundp command))
              (apply #'combobulate-define-envelope envelope)
              (should (commandp command))
              (should (eq (lookup-key combobulate-python-envelope-map
                                      (kbd "t"))
                          command))
              (should (eq (lookup-key combobulate-python-map
                                      (kbd "C-c t"))
                          command))
              (call-interactively command)
              (should (equal invoked-envelope "test envelope command")))
          (when (fboundp command)
            (fmakunbound command)))))))

(ert-deftest combobulate-test-envelope-execute-envelope ()
  :tags '(envelope combobulate)
  (combobulate-test (:language python :mode python-ts-mode
                     :fixture "fixtures/envelope/blank.py")
    (let ((combobulate-python-envelope-list
           '((:name "insert literal"
              :description "Insert a literal"
              :template ("hello"))))
          (combobulate-refactor--active-sessions))
      (combobulate-test-go-to-marker 1)
      (let ((start (point)))
        (combobulate-with-stubbed-proffer-choices (:choices '(0))
          (combobulate-execute-envelope "insert literal"))
        (should (equal (buffer-substring-no-properties start (+ start 5))
                       "hello"))
        (should (= (point) (+ start 5)))
        (should-not combobulate-refactor--active-sessions)))))

(ert-deftest combobulate-test-envelope-execute-envelope-missing ()
  :tags '(envelope combobulate)
  (let ((combobulate-python-envelope-list nil))
    (cl-letf (((symbol-function 'combobulate-primary-language)
               (lambda () 'python)))
      (should-error (combobulate-execute-envelope "missing")
                    :type 'error))))

(provide 'test-envelope)
;;; test-envelope.el ends here
