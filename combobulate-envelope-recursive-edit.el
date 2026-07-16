;;; combobulate-envelope-recursive-edit.el --- recursive-edit envelope POC  -*- lexical-binding: t; -*-

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

;; This is a second, isolated Envelope architecture experiment.  It exists
;; beside `combobulate-envelope-machine.el' and is not loaded by production
;; Combobulate.
;;
;; The first POC precomputes closure-backed steps, places them on an explicit
;; FIFO ready queue, and keeps nested execution in an explicit continuation
;; stack.  This POC precomputes the same small sort of plan but then leans on
;; two facilities Combobulate and Emacs already provide:
;;
;;  1. Normal Lisp recursion is the continuation stack.
;;  2. `recursive-edit' services interactive choice and prompt operations.
;;
;; One root `combobulate-refactor' session owns the expansion.  Every prompt
;; field, choice label, and interaction command re-enters that already-active
;; session using its ID.  The existing refactor implementation therefore owns
;; overlays, atomic rollback, and final cleanup; this file deliberately does
;; not implement a parallel transaction layer.
;;
;; The recursive command loop is injectable.  Interactive use defaults to
;; `recursive-edit', `exit-recursive-edit', and `abort-recursive-edit'.  ERT can
;; supply a deterministic driver and no-op exit functions, call the same
;; commands a user would call, and inspect the dynamically scoped interaction
;; context without terminal input.
;;
;; Supported instruction subset:
;;
;;   "text"                         insert text
;;   n                              insert a newline
;;   n>                             newline, then indent against live state
;;   (b INSTRUCTION...)             execute a nested plan recursively
;;   (save-column INSTRUCTION...)   capture and restore a live column
;;   (p TAG LABEL)                  materialize now, interact after the block
;;   (choice ALTERNATIVE...)        enter a recursive choice interaction
;;
;; Choice alternatives use the same POC-only shape as the queue version:
;;
;;   (:name "branch-name" :instructions (INSTRUCTION...))
;;
;; Choice previews are intentionally labels owned by the refactor session, not
;; speculative branch expansion.  Missing production features include full
;; `choice*' preview rendering, repeats, region registers, point proffering,
;; transformers, direct in-field editing, and production Envelope integration.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'combobulate-manipulation)

(define-error 'combobulate-envelope-recursive-abort
  "Recursive Envelope interaction aborted")

(cl-defstruct
    (combobulate-envelope-recursive-step
     (:constructor combobulate-envelope-recursive-step-create))
  "One inspectable closure in a recursive Envelope plan."
  kind
  description
  function
  source)

(cl-defstruct
    (combobulate-envelope-recursive-plan
     (:constructor combobulate-envelope-recursive-plan-create))
  "A precomputed vector of recursive Envelope STEPS."
  name
  steps)

(cl-defstruct
    (combobulate-envelope-recursive-choice
     (:constructor combobulate-envelope-recursive-choice-create))
  "One named, precompiled branch PLAN."
  name
  plan)

(cl-defstruct
    (combobulate-envelope-recursive-outcome
     (:constructor combobulate-envelope-recursive-outcome-create))
  "The structured result returned by one recursive interaction."
  status
  value)

(cl-defstruct
    (combobulate-envelope-recursive-context
     (:constructor combobulate-envelope-recursive-context-create))
  "Dynamically scoped state for one recursive interaction level."
  runtime
  kind
  choices
  (index 0)
  prompt-tag
  prompt-marker
  prompt-value
  preview-overlay
  result
  depth)

(cl-defstruct
    (combobulate-envelope-recursive-runtime
     (:constructor combobulate-envelope-recursive-runtime--create))
  "Execution state shared by a plan and its recursive interactions."
  plan
  refactor-id
  registers
  prompt-markers
  event-log
  indent-function
  driver
  exit-function
  abort-function
  status)

(defvar combobulate-envelope-recursive-context nil
  "The interaction context active at the current recursive editing level.

This is dynamically bound by `combobulate-envelope-recursive--interact'.  A
nested interaction shadows it and automatically restores the outer context
when the nested recursive level returns.")

(defvar combobulate-envelope-recursive--depth 0
  "Current recursive Envelope interaction depth.")

(defvar-keymap combobulate-envelope-recursive-map
  :doc "Keymap active during the recursive Envelope POC."
  "TAB" #'combobulate-envelope-recursive-next
  "S-<tab>" #'combobulate-envelope-recursive-previous
  "<backtab>" #'combobulate-envelope-recursive-previous
  "RET" #'combobulate-envelope-recursive-accept
  "C-g" #'combobulate-envelope-recursive-abort
  "C-c C-v" #'combobulate-envelope-recursive-set-prompt-value)

(defun combobulate-envelope-recursive--record (runtime kind &rest properties)
  "Record an event of KIND with PROPERTIES on RUNTIME."
  (push (append (list :kind kind) properties)
        (combobulate-envelope-recursive-runtime-event-log runtime)))

(defun combobulate-envelope-recursive-events (runtime)
  "Return RUNTIME's events in execution order."
  (reverse (combobulate-envelope-recursive-runtime-event-log runtime)))

(defun combobulate-envelope-recursive-step-kinds (plan)
  "Return each top-level step kind in PLAN."
  (mapcar #'combobulate-envelope-recursive-step-kind
          (append (combobulate-envelope-recursive-plan-steps plan) nil)))

(defun combobulate-envelope-recursive--make-step
    (kind description function source)
  "Create an inspectable step of KIND.
DESCRIPTION explains the operation, FUNCTION executes it, and SOURCE is the
instruction that produced it."
  (combobulate-envelope-recursive-step-create
   :kind kind
   :description description
   :function function
   :source source))

(cl-defmacro combobulate-envelope-recursive--reenter
    (runtime operation &rest body)
  "Re-enter RUNTIME's refactor session for OPERATION and execute BODY.

The session cons cell must exist before and after BODY.  Recording its identity
makes same-session re-entry observable in ERT without replacing or imitating
`combobulate-refactor'."
  (declare (indent 2) (debug (form form body)))
  (let ((runtime-value (gensym "runtime-"))
        (id (gensym "id-"))
        (session (gensym "session-"))
        (value (gensym "value-")))
    `(let* ((,runtime-value ,runtime)
            (,id (combobulate-envelope-recursive-runtime-refactor-id
                  ,runtime-value))
            (,session (combobulate-refactor--get-active-session ,id)))
       (unless ,session
         (error "Refactor session %S is not active" ,id))
       (let ((,value
              (combobulate-refactor (:id ,id)
                ,@body)))
         (unless (eq ,session (combobulate-refactor--get-active-session ,id))
           (error "Refactor session %S changed identity during re-entry" ,id))
         (combobulate-envelope-recursive--record
          ,runtime-value 'reenter-refactor
          :operation ,operation
          :refactor-id ,id
          :session ,session)
         ,value))))

(defun combobulate-envelope-recursive--register-value (runtime tag)
  "Return RUNTIME's register value for TAG or a readable default."
  (or (alist-get tag
                  (combobulate-envelope-recursive-runtime-registers runtime)
                  nil nil #'equal)
      (if (symbolp tag) (symbol-name tag) (format "%s" tag))))

(defun combobulate-envelope-recursive--prompt-marker (runtime token)
  "Return the prompt marker stored in RUNTIME for TOKEN."
  (or (alist-get token
                 (combobulate-envelope-recursive-runtime-prompt-markers runtime)
                 nil nil #'eq)
      (error "Prompt token %S has not been materialized" token)))

(defun combobulate-envelope-recursive--materialize-prompt
    (runtime token tag label)
  "Materialize disabled TAG with LABEL in RUNTIME under TOKEN."
  (let ((marker (point-marker))
        (value (combobulate-envelope-recursive--register-value runtime tag)))
    (combobulate-envelope-recursive--reenter runtime 'materialize-prompt
      (mark-field marker tag value)
      (toggle-field marker tag))
    (push (cons token marker)
          (combobulate-envelope-recursive-runtime-prompt-markers runtime))
    (combobulate-envelope-recursive--record
     runtime 'materialize-prompt :tag tag :label label :value value)))

(defun combobulate-envelope-recursive--toggle-prompt
    (runtime marker tag operation)
  "Toggle TAG at MARKER in RUNTIME while recording OPERATION."
  (combobulate-envelope-recursive--reenter runtime operation
    (toggle-field marker tag)))

(defun combobulate-envelope-recursive--update-prompt
    (runtime marker tag value)
  "Update TAG at MARKER in RUNTIME to VALUE through its refactor session."
  (save-excursion
    (goto-char marker)
    (combobulate-envelope-recursive--reenter runtime 'update-prompt
      (dolist (overlay
               (combobulate--refactor-get-overlays
                (combobulate-envelope-recursive-runtime-refactor-id runtime)))
        (combobulate--refactor-update-field overlay tag value
                                             (symbol-name tag))))))

(defun combobulate-envelope-recursive-current-choice (context)
  "Return the currently selected choice in CONTEXT."
  (unless (eq (combobulate-envelope-recursive-context-kind context) 'choice)
    (error "Interaction is not a choice"))
  (nth (combobulate-envelope-recursive-context-index context)
       (combobulate-envelope-recursive-context-choices context)))

(defun combobulate-envelope-recursive--clear-preview (context)
  "Delete CONTEXT's current refactor-owned preview overlay."
  (when-let ((overlay
              (combobulate-envelope-recursive-context-preview-overlay context)))
    (delete-overlay overlay)
    (setf (combobulate-envelope-recursive-context-preview-overlay context) nil)))

(defun combobulate-envelope-recursive--render-choice (context)
  "Render CONTEXT's current choice as a refactor-owned label."
  (combobulate-envelope-recursive--clear-preview context)
  (let* ((runtime (combobulate-envelope-recursive-context-runtime context))
         (choice (combobulate-envelope-recursive-current-choice context))
         (position (point-marker))
         (label (format "[%s]"
                        (combobulate-envelope-recursive-choice-name choice))))
    (setf (combobulate-envelope-recursive-context-preview-overlay context)
          (combobulate-envelope-recursive--reenter runtime 'render-choice
            (mark-range-label position position label
                              'combobulate-refactor-choice-face t)))
    (combobulate-envelope-recursive--record
     runtime 'render-choice
     :name (combobulate-envelope-recursive-choice-name choice)
     :index (combobulate-envelope-recursive-context-index context))))

(defun combobulate-envelope-recursive--require-context (&optional kind)
  "Return the active recursive context, optionally requiring KIND."
  (unless combobulate-envelope-recursive-context
    (user-error "No recursive Envelope interaction is active"))
  (when (and kind
             (not (eq kind
                      (combobulate-envelope-recursive-context-kind
                       combobulate-envelope-recursive-context))))
    (user-error "Expected a %S interaction, not %S"
                kind
                (combobulate-envelope-recursive-context-kind
                 combobulate-envelope-recursive-context)))
  combobulate-envelope-recursive-context)

(defun combobulate-envelope-recursive-next ()
  "Select and render the next choice in the current recursive interaction."
  (interactive)
  (let* ((context (combobulate-envelope-recursive--require-context 'choice))
         (choices (combobulate-envelope-recursive-context-choices context)))
    (setf (combobulate-envelope-recursive-context-index context)
          (mod (1+ (combobulate-envelope-recursive-context-index context))
               (length choices)))
    (combobulate-envelope-recursive--render-choice context)))

(defun combobulate-envelope-recursive-previous ()
  "Select and render the previous choice in the current recursive interaction."
  (interactive)
  (let* ((context (combobulate-envelope-recursive--require-context 'choice))
         (choices (combobulate-envelope-recursive-context-choices context)))
    (setf (combobulate-envelope-recursive-context-index context)
          (mod (1- (combobulate-envelope-recursive-context-index context))
               (length choices)))
    (combobulate-envelope-recursive--render-choice context)))

(defun combobulate-envelope-recursive-set-prompt-value (value)
  "Set the current recursive prompt to VALUE and update its live field."
  (interactive "sPrompt value: ")
  (let* ((context (combobulate-envelope-recursive--require-context 'prompt))
         (runtime (combobulate-envelope-recursive-context-runtime context))
         (marker (combobulate-envelope-recursive-context-prompt-marker context))
         (tag (combobulate-envelope-recursive-context-prompt-tag context)))
    (setf (combobulate-envelope-recursive-context-prompt-value context) value)
    ;; Synthetic nested-context ERTs may omit a concrete field.  A real prompt
    ;; context always carries both MARKER and TAG.
    (when (and marker tag)
      (combobulate-envelope-recursive--update-prompt
       runtime marker tag value))
    value))

(defun combobulate-envelope-recursive-accept ()
  "Accept the value selected by the current recursive interaction."
  (interactive)
  (let* ((context (combobulate-envelope-recursive--require-context))
         (runtime (combobulate-envelope-recursive-context-runtime context))
         (value
          (pcase (combobulate-envelope-recursive-context-kind context)
            ('choice (combobulate-envelope-recursive-current-choice context))
            ('prompt (combobulate-envelope-recursive-context-prompt-value context))
            (kind (error "Unknown recursive interaction kind %S" kind)))))
    (setf (combobulate-envelope-recursive-context-result context)
          (combobulate-envelope-recursive-outcome-create
           :status 'accept :value value))
    (funcall (combobulate-envelope-recursive-runtime-exit-function runtime))))

(defun combobulate-envelope-recursive-abort ()
  "Abort the current recursive interaction."
  (interactive)
  (let* ((context (combobulate-envelope-recursive--require-context))
         (runtime (combobulate-envelope-recursive-context-runtime context)))
    (setf (combobulate-envelope-recursive-context-result context)
          (combobulate-envelope-recursive-outcome-create :status 'abort))
    (funcall (combobulate-envelope-recursive-runtime-abort-function runtime))))

(defun combobulate-envelope-recursive--interact (runtime context)
  "Run CONTEXT through RUNTIME's recursive interaction driver."
  (let* ((combobulate-envelope-recursive-context context)
         (combobulate-envelope-recursive--depth
          (1+ combobulate-envelope-recursive--depth))
         (overriding-terminal-local-map combobulate-envelope-recursive-map))
    (setf (combobulate-envelope-recursive-context-depth context)
          combobulate-envelope-recursive--depth)
    (combobulate-envelope-recursive--record
     runtime 'enter-interaction
     :kind (combobulate-envelope-recursive-context-kind context)
     :depth combobulate-envelope-recursive--depth
     :refactor-id (combobulate-envelope-recursive-runtime-refactor-id runtime))
    (when (eq (combobulate-envelope-recursive-context-kind context) 'choice)
      (combobulate-envelope-recursive--render-choice context))
    (unwind-protect
        (condition-case nil
            (funcall (combobulate-envelope-recursive-runtime-driver runtime))
          (quit
           (unless (combobulate-envelope-recursive-context-result context)
             (setf (combobulate-envelope-recursive-context-result context)
                   (combobulate-envelope-recursive-outcome-create
                    :status 'abort)))))
      (combobulate-envelope-recursive--clear-preview context)
      (combobulate-envelope-recursive--record
       runtime 'leave-interaction
       :kind (combobulate-envelope-recursive-context-kind context)
       :depth combobulate-envelope-recursive--depth))
    (or (combobulate-envelope-recursive-context-result context)
        (error "Recursive interaction returned without an outcome"))))

(defun combobulate-envelope-recursive--accepted-value (outcome)
  "Return OUTCOME's value or signal an Envelope abort."
  (pcase (combobulate-envelope-recursive-outcome-status outcome)
    ('accept (combobulate-envelope-recursive-outcome-value outcome))
    ('abort (signal 'combobulate-envelope-recursive-abort nil))
    (status (error "Unknown recursive outcome %S" status))))

(defun combobulate-envelope-recursive--execute-plan (runtime plan)
  "Execute each precomputed step in PLAN recursively using RUNTIME."
  (seq-doseq (step (combobulate-envelope-recursive-plan-steps plan))
    (combobulate-envelope-recursive--record
     runtime 'run-step
     :step-kind (combobulate-envelope-recursive-step-kind step)
     :description (combobulate-envelope-recursive-step-description step))
    (funcall (combobulate-envelope-recursive-step-function step) runtime)))

(defun combobulate-envelope-recursive--insert-step (text source)
  "Create a step that will insert TEXT.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   'insert
   (format "insert %S" text)
   (lambda (runtime)
     (insert text)
     (combobulate-envelope-recursive--record runtime 'insert :text text))
   source))

(defun combobulate-envelope-recursive--newline-step (indent source)
  "Create a newline step, optionally resolving INDENT live.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   (if indent 'newline-and-indent 'newline)
   (if indent "newline and indent live buffer" "insert newline")
   (lambda (runtime)
     (newline)
     (when indent
       (funcall (or (combobulate-envelope-recursive-runtime-indent-function
                     runtime)
                    #'indent-according-to-mode)))
     (combobulate-envelope-recursive--record
      runtime
      (if indent 'newline-and-indent 'newline)
      :column (current-column)))
   source))

(defun combobulate-envelope-recursive--nested-plan-step (plan source)
  "Create a step that will recursively execute PLAN.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   'enter-plan
   (format "recursively execute %s"
           (combobulate-envelope-recursive-plan-name plan))
   (lambda (runtime)
     (combobulate-envelope-recursive--record
      runtime 'enter-plan :name (combobulate-envelope-recursive-plan-name plan))
     (combobulate-envelope-recursive--execute-plan runtime plan)
     (combobulate-envelope-recursive--record
      runtime 'leave-plan :name (combobulate-envelope-recursive-plan-name plan)))
   source))

(defun combobulate-envelope-recursive--save-column-step (plan source)
  "Create a step that will execute PLAN around a live column.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   'save-column
   (format "save live column around %s"
           (combobulate-envelope-recursive-plan-name plan))
   (lambda (runtime)
     (let ((column (current-column)))
       (combobulate-envelope-recursive--record
        runtime 'capture-column :column column)
       (combobulate-envelope-recursive--execute-plan runtime plan)
       (insert (make-string column ?\s))
       (combobulate-envelope-recursive--record
        runtime 'restore-column :column column)))
   source))

(defun combobulate-envelope-recursive--materialize-step
    (token tag label source)
  "Create a prompt materialization step for TOKEN, TAG and LABEL.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   'materialize-prompt
   (format "materialize disabled prompt %S" tag)
   (lambda (runtime)
     (combobulate-envelope-recursive--materialize-prompt
      runtime token tag label))
   source))

(defun combobulate-envelope-recursive--activate-step
    (token tag label source)
  "Create a prompt activation step for TOKEN, TAG and LABEL.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   'activate-prompt
   (format "activate prompt %S through recursive-edit" tag)
   (lambda (runtime)
     (let* ((marker
             (combobulate-envelope-recursive--prompt-marker runtime token))
            (initial
             (combobulate-envelope-recursive--register-value runtime tag)))
       (combobulate-envelope-recursive--toggle-prompt
        runtime marker tag 'activate-prompt)
       (let* ((context
               (combobulate-envelope-recursive-context-create
                :runtime runtime
                :kind 'prompt
                :prompt-tag tag
                :prompt-marker marker
                :prompt-value initial))
              (value
               (combobulate-envelope-recursive--accepted-value
                (combobulate-envelope-recursive--interact runtime context))))
         (combobulate-envelope-recursive--update-prompt
          runtime marker tag value)
         (setf (alist-get tag
                          (combobulate-envelope-recursive-runtime-registers
                           runtime)
                          nil nil #'equal)
               value)
         (combobulate-envelope-recursive--record
          runtime 'accept-prompt :tag tag :label label :value value))))
   source))

(defun combobulate-envelope-recursive--choice-step (choices source)
  "Create a recursive interaction step for CHOICES.
SOURCE is the instruction that produced the step."
  (combobulate-envelope-recursive--make-step
   'choice
   (format "recursively choose one of %s"
           (mapconcat #'combobulate-envelope-recursive-choice-name
                      choices ", "))
   (lambda (runtime)
     (let* ((context
             (combobulate-envelope-recursive-context-create
              :runtime runtime
              :kind 'choice
              :choices choices))
            (choice
             (combobulate-envelope-recursive--accepted-value
              (combobulate-envelope-recursive--interact runtime context))))
       (combobulate-envelope-recursive--record
        runtime 'accept-choice
        :name (combobulate-envelope-recursive-choice-name choice))
       (combobulate-envelope-recursive--execute-plan
        runtime (combobulate-envelope-recursive-choice-plan choice))))
   source))

(defun combobulate-envelope-recursive--compile-choice (source alternatives)
  "Compile ALTERNATIVES from choice instruction SOURCE."
  (unless alternatives
    (error "A choice requires at least one alternative"))
  (let ((choices))
    (dolist (alternative alternatives)
      (let ((name (plist-get alternative :name))
            (instructions (plist-get alternative :instructions)))
        (unless (and (stringp name) (listp instructions))
          (error "Invalid recursive POC choice alternative: %S" alternative))
        (push (combobulate-envelope-recursive-choice-create
               :name name
               :plan (combobulate-envelope-recursive-compile
                      instructions (format "choice:%s" name)))
              choices)))
    (combobulate-envelope-recursive--choice-step
     (nreverse choices) source)))

(defun combobulate-envelope-recursive--compile-sequence (instructions name)
  "Compile INSTRUCTIONS into a recursive plan named NAME."
  (let ((steps) (activations))
    (dolist (instruction instructions)
      (pcase instruction
        ((and (pred stringp) text)
         (setq steps
               (nconc steps
                      (list (combobulate-envelope-recursive--insert-step
                             text instruction)))))
        ('n
         (setq steps
               (nconc steps
                      (list (combobulate-envelope-recursive--newline-step
                             nil instruction)))))
        ('n>
         (setq steps
               (nconc steps
                      (list (combobulate-envelope-recursive--newline-step
                             t instruction)))))
        (`(b . ,body)
         (let ((plan (combobulate-envelope-recursive-compile body "block")))
           (setq steps
                 (nconc steps
                        (list
                         (combobulate-envelope-recursive--nested-plan-step
                          plan instruction))))))
        (`(save-column . ,body)
         (let ((plan
                (combobulate-envelope-recursive-compile body "save-column")))
           (setq steps
                 (nconc steps
                        (list
                         (combobulate-envelope-recursive--save-column-step
                          plan instruction))))))
        ((or `(p ,tag ,label) `(prompt ,tag ,label))
         (unless (stringp label)
           (error "Prompt label must be a string: %S" instruction))
         (let ((token (gensym "prompt-")))
           (setq steps
                 (nconc steps
                        (list
                         (combobulate-envelope-recursive--materialize-step
                          token tag label instruction))))
           (setq activations
                 (nconc activations
                        (list
                         (combobulate-envelope-recursive--activate-step
                          token tag label instruction))))))
        (`(choice . ,alternatives)
         (setq steps
               (nconc steps
                      (list
                       (combobulate-envelope-recursive--compile-choice
                        instruction alternatives)))))
        (_ (error "Unsupported recursive POC instruction: %S" instruction))))
    (combobulate-envelope-recursive-plan-create
     :name name
     :steps (vconcat steps activations))))

(defun combobulate-envelope-recursive-compile (instructions &optional name)
  "Precompute INSTRUCTIONS as a recursive plan named NAME."
  (unless (listp instructions)
    (error "Recursive Envelope instructions must be a list"))
  (combobulate-envelope-recursive--compile-sequence
   instructions (or name "envelope")))

(cl-defun combobulate-envelope-recursive-start
    (plan &key registers indent-function driver exit-function abort-function)
  "Create, but do not execute, a recursive runtime for PLAN.

REGISTERS supplies initial prompt values.  INDENT-FUNCTION resolves `n>' steps.
DRIVER defaults to `recursive-edit'.  EXIT-FUNCTION and ABORT-FUNCTION default
to `exit-recursive-edit' and `abort-recursive-edit'; tests can inject no-op
functions and drive the same interactive commands synchronously."
  (unless (combobulate-envelope-recursive-plan-p plan)
    (error "Not a recursive Envelope plan: %S" plan))
  (combobulate-envelope-recursive-runtime--create
   :plan plan
   :refactor-id (gensym "combobulate-envelope-recursive-")
   :registers (copy-tree registers)
   :indent-function indent-function
   :driver (or driver #'recursive-edit)
   :exit-function (or exit-function #'exit-recursive-edit)
   :abort-function (or abort-function #'abort-recursive-edit)
   :status 'ready))

(defun combobulate-envelope-recursive-run (runtime)
  "Execute RUNTIME to completion through recursive interaction levels.

Return `:done' after committing the root refactor session or `:aborted' after
an interaction abort causes the existing atomic refactor machinery to roll all
mutations back."
  (let ((id (combobulate-envelope-recursive-runtime-refactor-id runtime)))
    (setf (combobulate-envelope-recursive-runtime-status runtime) 'running)
    (condition-case nil
        (let ((result
               (combobulate-refactor (:id id)
                 (combobulate-envelope-recursive--execute-plan
                  runtime (combobulate-envelope-recursive-runtime-plan runtime))
                 (commit)
                 :done)))
          (setf (combobulate-envelope-recursive-runtime-status runtime) 'done)
          (combobulate-envelope-recursive--record
           runtime 'commit :refactor-id id)
          result)
      (combobulate-envelope-recursive-abort
       (setf (combobulate-envelope-recursive-runtime-status runtime) 'aborted)
       (combobulate-envelope-recursive--record
        runtime 'abort :refactor-id id)
       :aborted))))

(provide 'combobulate-envelope-recursive-edit)
;;; combobulate-envelope-recursive-edit.el ends here
