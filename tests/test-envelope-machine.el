;;; test-envelope-machine.el --- tests for the envelope machine POC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords: convenience, languages, tools

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

;; These tests are intentionally small and state-oriented.  They serve as
;; executable documentation for `combobulate-envelope-machine.el': compilation
;; precomputes inspectable closures, the stack preserves nested control flow,
;; the FIFO queue exposes only ready work, and each call to
;; `combobulate-envelope-machine-step' executes at most one closure.

;;; Code:

(require 'ert)
(require 'combobulate-envelope-machine)

(defun combobulate-test-envelope-machine--step-kinds (plan)
  "Return the kinds of all top-level steps in PLAN."
  (mapcar #'combobulate-envelope-machine-step-kind
          (append (combobulate-envelope-machine-plan-steps plan) nil)))

(defun combobulate-test-envelope-machine--event (runtime kind)
  "Return the first event of KIND recorded by RUNTIME."
  (seq-find (lambda (event)
              (eq kind (plist-get event :kind)))
            (combobulate-envelope-machine-events runtime)))

(ert-deftest combobulate-test-envelope-machine-drains-one-step-at-a-time ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    (let* ((plan (combobulate-envelope-machine-compile '("A" "B")))
           (runtime (combobulate-envelope-machine-start plan)))
      (should (equal (combobulate-test-envelope-machine--step-kinds plan)
                     '(insert insert)))
      (dolist (step (append (combobulate-envelope-machine-plan-steps plan) nil))
        (should (functionp (combobulate-envelope-machine-step-function step)))
        (should (stringp (combobulate-envelope-machine-step-description step))))

      ;; A drain schedules and executes only the first insertion closure.
      (should (eq (combobulate-envelope-machine-step runtime) :ran))
      (should (equal (buffer-string) "A"))
      (should-not (combobulate-envelope-machine-ready-steps runtime))

      ;; The second closure does not run until the next drain.
      (should (eq (combobulate-envelope-machine-step runtime) :ran))
      (should (equal (buffer-string) "AB"))
      (should (eq (combobulate-envelope-machine-step runtime) :done))
      (should (combobulate-envelope-machine-done-p runtime)))))

(ert-deftest combobulate-test-envelope-machine-stack-preserves-nested-order ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    (let* ((plan (combobulate-envelope-machine-compile
                  '("A" (b "B" (b "C") "D") "E")))
           (runtime (combobulate-envelope-machine-start plan)))
      (should (= (combobulate-envelope-machine-stack-depth runtime) 1))

      (should (eq (combobulate-envelope-machine-step runtime) :ran))
      (should (equal (buffer-string) "A"))

      ;; Entering the first block pushes a continuation frame but does not run
      ;; the first instruction in that frame during the same drain.
      (should (eq (combobulate-envelope-machine-step runtime) :ran))
      (should (= (combobulate-envelope-machine-stack-depth runtime) 2))
      (should (equal (buffer-string) "A"))

      (should (eq (combobulate-envelope-machine-step runtime) :ran))
      (should (equal (buffer-string) "AB"))

      ;; The nested block is another independent frame.
      (should (eq (combobulate-envelope-machine-step runtime) :ran))
      (should (= (combobulate-envelope-machine-stack-depth runtime) 3))
      (should (equal (buffer-string) "AB"))

      (should (eq (combobulate-envelope-machine-run runtime) :done))
      (should (equal (buffer-string) "ABCDE"))
      (should (= (combobulate-envelope-machine-stack-depth runtime) 0)))))

(ert-deftest combobulate-test-envelope-machine-save-column-reads-live-buffer ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    ;; Compile before the buffer has the prefix.  A precomputed numeric column
    ;; would therefore be wrong; the capture closure must read it when drained.
    (let ((plan (combobulate-envelope-machine-compile
                 '("xx" (save-column "body" n) "tail"))))
      (insert ">>>")
      (let ((runtime (combobulate-envelope-machine-start plan)))
        (should (eq (combobulate-envelope-machine-run runtime) :done))
        (should (equal (buffer-string) ">>>xxbody\n     tail"))
        (let ((capture
               (combobulate-test-envelope-machine--event runtime 'capture-column)))
          (should capture)
          (should (= (plist-get capture :column) 5)))))))

(ert-deftest combobulate-test-envelope-machine-indents-after-mutation ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    (let* ((observed-buffer)
           (plan (combobulate-envelope-machine-compile '("if:" n> "pass")))
           (runtime
            (combobulate-envelope-machine-start
             plan
             :indent-function
             (lambda ()
               ;; The indentation oracle sees the prefix and newline inserted
               ;; by earlier closures, not the pre-compilation buffer.
               (setq observed-buffer (buffer-string))
               (indent-to 4)))))
      (should (eq (combobulate-envelope-machine-run runtime) :done))
      (should (equal observed-buffer "if:\n"))
      (should (equal (buffer-string) "if:\n    pass"))
      (let ((indent-event
             (combobulate-test-envelope-machine--event
              runtime 'newline-and-indent)))
        (should (= (plist-get indent-event :column) 4))))))

(ert-deftest combobulate-test-envelope-machine-choice-resumes-precompiled-branch ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    (let* ((plan
            (combobulate-envelope-machine-compile
             '("["
               (choice
                (:name "left" :instructions ("L"))
                (:name "right" :instructions ((b "R"))))
               "]")))
           (runtime (combobulate-envelope-machine-start plan)))
      (should (eq (combobulate-envelope-machine-run runtime) :suspended))
      (should (equal (buffer-string) "["))
      (let* ((suspension
              (combobulate-envelope-machine-runtime-suspension runtime))
             (choices
              (combobulate-envelope-machine-suspension-payload suspension)))
        (should (eq (combobulate-envelope-machine-suspension-kind suspension)
                    'choice))
        (should (equal (mapcar #'combobulate-envelope-machine-choice-name choices)
                       '("left" "right")))
        (should (seq-every-p
                 (lambda (choice)
                   (combobulate-envelope-machine-plan-p
                    (combobulate-envelope-machine-choice-plan choice)))
                 choices)))

      ;; Resumption pushes only the selected, already-compiled branch.  The
      ;; enclosing frame waits underneath it and adds the closing bracket last.
      (combobulate-envelope-machine-resume-choice runtime "right")
      (should (= (combobulate-envelope-machine-stack-depth runtime) 2))
      (should (eq (combobulate-envelope-machine-run runtime) :done))
      (should (equal (buffer-string) "[R]")))))

(ert-deftest combobulate-test-envelope-machine-materializes-before-choice-and-prompts-after ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    (let* ((plan
            (combobulate-envelope-machine-compile
             '("if "
               (p True "Bool")
               ":" n>
               (choice
                (:name "body" :instructions ("pass"))
                (:name "alternative" :instructions ("other"))))))
           (runtime
            (combobulate-envelope-machine-start
             plan :indent-function (lambda () (indent-to 4)))))
      ;; Compilation makes the intended lifecycle visible.  Materialization is
      ;; lexical; activation is in the containing block's interaction tail.
      (should
       (equal (combobulate-test-envelope-machine--step-kinds plan)
              '(insert materialize-prompt insert newline-and-indent
                choice activate-prompt)))

      (should (eq (combobulate-envelope-machine-run runtime) :suspended))
      (should (equal (buffer-string) "if True:\n    "))
      (should
       (eq (combobulate-envelope-machine-suspension-kind
            (combobulate-envelope-machine-runtime-suspension runtime))
           'choice))

      ;; The prompt exists during choice suspension, but is visibly disabled.
      (let ((field
             (car (combobulate-envelope-machine-fields-for-tag runtime 'True))))
        (should field)
        (should (equal (combobulate-envelope-machine-field-text field) "True"))
        (should (eq (combobulate-envelope-machine-field-state field) 'disabled))
        (should (eq (overlay-get
                     (combobulate-envelope-machine-field-overlay field) 'face)
                    'shadow)))

      (combobulate-envelope-machine-resume-choice runtime "body")
      (should (eq (combobulate-envelope-machine-run runtime) :suspended))
      (should (equal (buffer-string) "if True:\n    pass"))
      (should
       (eq (combobulate-envelope-machine-suspension-kind
            (combobulate-envelope-machine-runtime-suspension runtime))
           'prompt))

      ;; Activation reuses the field that was materialized before the choice.
      (let ((field
             (car (combobulate-envelope-machine-fields-for-tag runtime 'True))))
        (should (eq (combobulate-envelope-machine-field-state field) 'active))
        (should (eq (overlay-get
                     (combobulate-envelope-machine-field-overlay field) 'face)
                    'highlight)))

      (combobulate-envelope-machine-resume-prompt runtime "condition")
      (should (eq (combobulate-envelope-machine-run runtime) :done))
      (should (equal (buffer-string) "if condition:\n    pass"))
      (let ((field
             (car (combobulate-envelope-machine-fields-for-tag runtime 'True))))
        (should (equal (combobulate-envelope-machine-field-text field)
                       "condition"))
        (should (eq (combobulate-envelope-machine-field-state field) 'complete))
        (should-not
         (overlay-get (combobulate-envelope-machine-field-overlay field)
                      'face))))))

(ert-deftest combobulate-test-envelope-machine-does-not-leak-unselected-prompts ()
  :tags '(envelope-machine envelope combobulate)
  (with-temp-buffer
    (let* ((plan
            (combobulate-envelope-machine-compile
             '((choice
                (:name "prompting"
                 :instructions ((p BranchValue "Branch value")))
                (:name "plain"
                 :instructions ("plain"))))))
           (runtime (combobulate-envelope-machine-start plan)))
      (should (eq (combobulate-envelope-machine-run runtime) :suspended))
      (should-not (combobulate-envelope-machine-runtime-fields runtime))

      (combobulate-envelope-machine-resume-choice runtime "plain")
      (should (eq (combobulate-envelope-machine-run runtime) :done))
      (should (equal (buffer-string) "plain"))
      (should-not (combobulate-envelope-machine-runtime-fields runtime)))))

(provide 'test-envelope-machine)
;;; test-envelope-machine.el ends here
