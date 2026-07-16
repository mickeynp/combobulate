;;; test-envelope-recursive-edit.el --- recursive Envelope POC tests  -*- lexical-binding: t; -*-

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

;; Executable documentation for `combobulate-envelope-recursive-edit.el'.
;; Drivers call the same next/accept/abort/prompt commands bound in the real
;; recursive-edit keymap, but injected no-op exit functions keep ERT entirely
;; noninteractive.

;;; Code:

(require 'ert)
(require 'combobulate-envelope-recursive-edit)

(defun combobulate-test-envelope-recursive--event (runtime kind)
  "Return the first event of KIND recorded by RUNTIME."
  (seq-find (lambda (event)
              (eq kind (plist-get event :kind)))
            (combobulate-envelope-recursive-events runtime)))

(defun combobulate-test-envelope-recursive--events (runtime kind)
  "Return every event of KIND recorded by RUNTIME."
  (seq-filter (lambda (event)
                (eq kind (plist-get event :kind)))
              (combobulate-envelope-recursive-events runtime)))

(defun combobulate-test-envelope-recursive--field (runtime tag)
  "Return TAG's field overlay from RUNTIME's active refactor session."
  (seq-find
   (lambda (overlay)
     (combobulate--refactor-field-has-tag-p overlay tag))
   (combobulate--refactor-get-overlays
    (combobulate-envelope-recursive-runtime-refactor-id runtime))))

(ert-deftest combobulate-test-envelope-recursive-uses-lisp-recursion-for-blocks ()
  :tags '(envelope-recursive envelope combobulate)
  (with-temp-buffer
    (let* ((combobulate-refactor--active-sessions nil)
           (driver-called nil)
           (plan
            (combobulate-envelope-recursive-compile
             '("A" (b "B" (b "C") "D") "E")))
           (runtime
            (combobulate-envelope-recursive-start
             plan
             :driver (lambda () (setq driver-called t))
             :exit-function #'ignore
             :abort-function #'ignore)))
      (should (equal (combobulate-envelope-recursive-step-kinds plan)
                     '(insert enter-plan insert)))
      (should (eq (combobulate-envelope-recursive-run runtime) :done))
      (should (equal (buffer-string) "ABCDE"))
      (should-not driver-called)
      (should (equal
               (mapcar (lambda (event) (plist-get event :name))
                       (combobulate-test-envelope-recursive--events
                        runtime 'enter-plan))
               '("block" "block")))
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-envelope-recursive-reenters-one-refactor-session ()
  :tags '(envelope-recursive envelope combobulate)
  (with-temp-buffer
    (let* ((combobulate-refactor--active-sessions nil)
           (plan
            (combobulate-envelope-recursive-compile
             '("["
               (choice
                (:name "left" :instructions ("L"))
                (:name "right" :instructions ("R")))
               "]")))
           runtime
           observed-session)
      (setq
       runtime
       (combobulate-envelope-recursive-start
        plan
        :exit-function #'ignore
        :abort-function #'ignore
        :driver
        (lambda ()
          (let* ((context combobulate-envelope-recursive-context)
                 (id (combobulate-envelope-recursive-runtime-refactor-id
                      runtime))
                 (session (combobulate-refactor--get-active-session id))
                 (first-preview
                  (combobulate-envelope-recursive-context-preview-overlay
                   context)))
            (should session)
            (setq observed-session session)
            (should (eq (overlay-get first-preview
                                     'combobulate-refactor-session-id)
                        id))
            (should (equal (substring-no-properties
                            (overlay-get first-preview 'before-string))
                           "[left]"))

            ;; TAB re-enters the same session and replaces only the label.
            (combobulate-envelope-recursive-next)
            (should (eq session
                        (combobulate-refactor--get-active-session id)))
            (should-not (overlay-buffer first-preview))
            (let ((second-preview
                   (combobulate-envelope-recursive-context-preview-overlay
                    context)))
              (should (eq (overlay-get second-preview
                                       'combobulate-refactor-session-id)
                          id))
              (should (equal (substring-no-properties
                              (overlay-get second-preview 'before-string))
                             "[right]")))
            (combobulate-envelope-recursive-accept)))))

      (should (eq (combobulate-envelope-recursive-run runtime) :done))
      (should (equal (buffer-string) "[R]"))
      (let ((reentries
             (combobulate-test-envelope-recursive--events
              runtime 'reenter-refactor)))
        (should (>= (length reentries) 2))
        (should
         (seq-every-p
          (lambda (event)
            (and (eq (plist-get event :refactor-id)
                     (combobulate-envelope-recursive-runtime-refactor-id
                      runtime))
                 (eq (plist-get event :session) observed-session)))
          reentries)))
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-envelope-recursive-materializes-before-choice ()
  :tags '(envelope-recursive envelope combobulate)
  (with-temp-buffer
    (let* ((combobulate-refactor--active-sessions nil)
           (interaction-kinds)
           (refactor-ids)
           (field-before-choice)
           (field-during-prompt)
           (plan
            (combobulate-envelope-recursive-compile
             '("if "
               (p True "Bool")
               ":" n>
               (choice
                (:name "body" :instructions ("pass"))
                (:name "alternative" :instructions ("other"))))))
           runtime)
      (should
       (equal (combobulate-envelope-recursive-step-kinds plan)
              '(insert materialize-prompt insert newline-and-indent
                choice activate-prompt)))
      (setq
       runtime
       (combobulate-envelope-recursive-start
        plan
        :indent-function (lambda () (indent-to 4))
        :exit-function #'ignore
        :abort-function #'ignore
        :driver
        (lambda ()
          (let ((kind
                 (combobulate-envelope-recursive-context-kind
                  combobulate-envelope-recursive-context)))
            (push kind interaction-kinds)
            (push (combobulate-envelope-recursive-runtime-refactor-id runtime)
                  refactor-ids)
            (pcase kind
              ('choice
               (setq field-before-choice
                     (combobulate-test-envelope-recursive--field runtime 'True))
               (should field-before-choice)
               (should-not
                (overlay-get field-before-choice
                             'combobulate-refactor-field-enabled))
               (should (eq (overlay-get field-before-choice 'face)
                           'combobulate-refactor-disabled-field-face))
               (should (equal (buffer-string) "if True:\n    "))
               (combobulate-envelope-recursive-accept))
              ('prompt
               (setq field-during-prompt
                     (combobulate-test-envelope-recursive--field runtime 'True))
               (should (eq field-before-choice field-during-prompt))
               (should
                (overlay-get field-during-prompt
                             'combobulate-refactor-field-enabled))
               (should (eq (overlay-get field-during-prompt 'face)
                           'combobulate-refactor-field-face))
               (combobulate-envelope-recursive-set-prompt-value "condition")
               (should (equal (buffer-string) "if condition:\n    pass"))
               (combobulate-envelope-recursive-accept)))))))

      (should (eq (combobulate-envelope-recursive-run runtime) :done))
      (should (equal (nreverse interaction-kinds) '(choice prompt)))
      (should (equal (buffer-string) "if condition:\n    pass"))
      (should (seq-every-p
               (lambda (id)
                 (eq id
                     (combobulate-envelope-recursive-runtime-refactor-id
                      runtime)))
               refactor-ids))
      ;; Committing preserves inserted text but lets the real refactor session
      ;; remove all of its field and preview overlays.
      (should-not combobulate-refactor--active-sessions)
      (should-not (combobulate--refactor-get-overlays)))))

(ert-deftest combobulate-test-envelope-recursive-restores-outer-context ()
  :tags '(envelope-recursive envelope combobulate)
  (with-temp-buffer
    (let* ((combobulate-refactor--active-sessions nil)
           (plan
            (combobulate-envelope-recursive-compile
             '((choice
                (:name "outer" :instructions ("selected"))))))
           runtime
           outer-context
           nested-outcome
           outer-restored)
      (setq
       runtime
       (combobulate-envelope-recursive-start
        plan
        :exit-function #'ignore
        :abort-function #'ignore
        :driver
        (lambda ()
          (pcase combobulate-envelope-recursive--depth
            (1
             (setq outer-context combobulate-envelope-recursive-context)
             (let ((nested
                    (combobulate-envelope-recursive-context-create
                     :runtime runtime
                     :kind 'prompt
                     :prompt-value "initial")))
               (setq nested-outcome
                     (combobulate-envelope-recursive--interact runtime nested)))
             (setq outer-restored
                   (eq combobulate-envelope-recursive-context outer-context))
             (combobulate-envelope-recursive-accept))
            (2
             (should-not
              (eq combobulate-envelope-recursive-context outer-context))
             (combobulate-envelope-recursive-set-prompt-value "nested")
             (combobulate-envelope-recursive-accept))
            (depth (ert-fail (format "Unexpected recursive depth %S" depth)))))))

      (should (eq (combobulate-envelope-recursive-run runtime) :done))
      (should outer-restored)
      (should (eq (combobulate-envelope-recursive-outcome-status nested-outcome)
                  'accept))
      (should (equal
               (combobulate-envelope-recursive-outcome-value nested-outcome)
               "nested"))
      (should (equal (buffer-string) "selected"))
      (should-not combobulate-envelope-recursive-context)
      (should
       (equal
        (mapcar (lambda (event) (plist-get event :depth))
                (combobulate-test-envelope-recursive--events
                 runtime 'enter-interaction))
        '(1 2)))
      (should-not combobulate-refactor--active-sessions))))

(ert-deftest combobulate-test-envelope-recursive-abort-rolls-back-root-session ()
  :tags '(envelope-recursive envelope combobulate)
  (with-temp-buffer
    (insert "seed")
    (let* ((combobulate-refactor--active-sessions nil)
           (plan
            (combobulate-envelope-recursive-compile
             '("before"
               (choice
                (:name "one" :instructions ("one")))
               "after")))
           (runtime
            (combobulate-envelope-recursive-start
             plan
             :driver #'combobulate-envelope-recursive-abort
             :exit-function #'ignore
             :abort-function #'ignore)))
      (should (eq (combobulate-envelope-recursive-run runtime) :aborted))
      (should (equal (buffer-string) "seed"))
      (should (eq (combobulate-envelope-recursive-runtime-status runtime)
                  'aborted))
      (should (combobulate-test-envelope-recursive--event runtime 'abort))
      (should-not combobulate-envelope-recursive-context)
      (should-not combobulate-refactor--active-sessions)
      (should-not (combobulate--refactor-get-overlays)))))

(ert-deftest combobulate-test-envelope-recursive-indents-after-mutation ()
  :tags '(envelope-recursive envelope combobulate)
  (with-temp-buffer
    (let* ((combobulate-refactor--active-sessions nil)
           observed-buffer
           (plan
            (combobulate-envelope-recursive-compile '("if:" n> "pass")))
           (runtime
            (combobulate-envelope-recursive-start
             plan
             :indent-function
             (lambda ()
               (setq observed-buffer (buffer-string))
               (indent-to 4))
             :driver (lambda () (ert-fail "Unexpected interaction"))
             :exit-function #'ignore
             :abort-function #'ignore)))
      (should (eq (combobulate-envelope-recursive-run runtime) :done))
      (should (equal observed-buffer "if:\n"))
      (should (equal (buffer-string) "if:\n    pass"))
      (should-not combobulate-refactor--active-sessions))))

(provide 'test-envelope-recursive-edit)
;;; test-envelope-recursive-edit.el ends here
