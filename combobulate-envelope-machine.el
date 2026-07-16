;;; combobulate-envelope-machine.el --- incremental envelope machine POC  -*- lexical-binding: t; -*-

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

;; This file is an isolated proof of concept for an incremental envelope
;; interpreter.  It is deliberately not loaded by `combobulate' and does not
;; replace any part of `combobulate-envelope.el'.
;;
;; The experiment separates four things that the production interpreter often
;; combines:
;;
;;  1. A PLAN is a precomputed sequence of inspectable steps.
;;  2. A FRAME records where execution should resume in a nested plan.
;;  3. A TASK pairs a precomputed step with the frame executing it.
;;  4. A RUNTIME owns a LIFO continuation stack and a FIFO ready queue.
;;
;; `combobulate-envelope-machine-step' drains at most one closure.  Step
;; closures therefore read columns, markers, indentation, and other mutable
;; buffer state only when that particular step becomes current.
;;
;; Supported instruction subset:
;;
;;   "text"                         insert text
;;   n                              insert a newline
;;   n>                             newline, then indent at execution time
;;   (b INSTRUCTION...)             execute a nested sequence
;;   (save-column INSTRUCTION...)   capture a live column around a sequence
;;   (p TAG LABEL)                  materialize now, activate after the block
;;   (choice ALTERNATIVE...)        suspend until one branch is selected
;;
;; A choice alternative has this POC-only shape:
;;
;;   (:name "branch-name" :instructions (INSTRUCTION...))
;;
;; Prompt materialization inserts a disabled overlay immediately.  Prompt
;; activation is compiled into the containing plan's interaction tail and
;; suspends the machine until `combobulate-envelope-machine-resume-prompt' is
;; called.  Choice branches are all compiled up front, but only the selected
;; branch is pushed onto the continuation stack.
;;
;; Deliberate omissions include production refactor transactions, speculative
;; choice previews, repeats, region registers, point proffering, field
;; transformers, branch `:missing' behavior, abort/rollback, and integration
;; with the real envelope DSL.  The point is to make scheduling and mutation
;; boundaries understandable before attempting feature parity.

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defstruct
    (combobulate-envelope-machine-step
     (:constructor combobulate-envelope-machine-step-create))
  "One inspectable, closure-backed operation in a compiled plan."
  kind
  description
  function
  source)

(cl-defstruct
    (combobulate-envelope-machine-plan
     (:constructor combobulate-envelope-machine-plan-create))
  "A precomputed sequence of envelope machine STEPS."
  name
  steps)

(cl-defstruct
    (combobulate-envelope-machine-frame
     (:constructor combobulate-envelope-machine-frame-create))
  "A continuation frame for PLAN.

INDEX identifies the next step to schedule.  LOCALS contains values, such as a
saved column, that belong to this particular invocation of the plan."
  plan
  (index 0)
  locals)

(cl-defstruct
    (combobulate-envelope-machine-task
     (:constructor combobulate-envelope-machine-task-create))
  "A STEP scheduled for execution on behalf of FRAME."
  step
  frame)

(cl-defstruct
    (combobulate-envelope-machine-choice
     (:constructor combobulate-envelope-machine-choice-create))
  "A named, precompiled choice PLAN."
  name
  plan)

(cl-defstruct
    (combobulate-envelope-machine-suspension
     (:constructor combobulate-envelope-machine-suspension-create))
  "An interaction that has temporarily suspended a runtime."
  kind
  payload)

(cl-defstruct
    (combobulate-envelope-machine-field
     (:constructor combobulate-envelope-machine-field-create))
  "A materialized prompt field owned by the POC runtime."
  tag
  label
  overlay
  state)

(cl-defstruct
    (combobulate-envelope-machine-runtime
     (:constructor combobulate-envelope-machine-runtime--create))
  "Mutable execution state for an envelope machine plan.

STACK is LIFO.  QUEUE-HEAD and QUEUE-TAIL implement a FIFO queue without
repeatedly traversing it.  The queue contains `combobulate-envelope-machine-task'
objects, not bare closures, so every operation retains its executing frame."
  stack
  queue-head
  queue-tail
  registers
  fields
  event-log
  suspension
  indent-function)

(defun combobulate-envelope-machine--record (runtime kind &rest properties)
  "Record an event of KIND with PROPERTIES on RUNTIME."
  (push (append (list :kind kind) properties)
        (combobulate-envelope-machine-runtime-event-log runtime)))

(defun combobulate-envelope-machine-events (runtime)
  "Return RUNTIME's event log in execution order."
  (reverse (combobulate-envelope-machine-runtime-event-log runtime)))

(defun combobulate-envelope-machine--enqueue (runtime task)
  "Append TASK to RUNTIME's FIFO ready queue."
  (let ((cell (list task)))
    (if (combobulate-envelope-machine-runtime-queue-head runtime)
        (setcdr (combobulate-envelope-machine-runtime-queue-tail runtime) cell)
      (setf (combobulate-envelope-machine-runtime-queue-head runtime) cell))
    (setf (combobulate-envelope-machine-runtime-queue-tail runtime) cell)))

(defun combobulate-envelope-machine--dequeue (runtime)
  "Remove and return the first ready task from RUNTIME."
  (let* ((head (combobulate-envelope-machine-runtime-queue-head runtime))
         (task (car head)))
    (when head
      (setf (combobulate-envelope-machine-runtime-queue-head runtime) (cdr head))
      (unless (combobulate-envelope-machine-runtime-queue-head runtime)
        (setf (combobulate-envelope-machine-runtime-queue-tail runtime) nil)))
    task))

(defun combobulate-envelope-machine-ready-steps (runtime)
  "Return the inspectable steps currently ready on RUNTIME."
  (mapcar (lambda (task)
            (combobulate-envelope-machine-task-step task))
          (combobulate-envelope-machine-runtime-queue-head runtime)))

(defun combobulate-envelope-machine-stack-depth (runtime)
  "Return the number of continuation frames on RUNTIME's stack."
  (length (combobulate-envelope-machine-runtime-stack runtime)))

(defun combobulate-envelope-machine-fields-for-tag (runtime tag)
  "Return all fields in RUNTIME whose tag is equal to TAG."
  (seq-filter (lambda (field)
                (equal tag (combobulate-envelope-machine-field-tag field)))
              (combobulate-envelope-machine-runtime-fields runtime)))

(defun combobulate-envelope-machine-field-text (field)
  "Return the current buffer text represented by FIELD."
  (let ((overlay (combobulate-envelope-machine-field-overlay field)))
    (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay))))

(defun combobulate-envelope-machine--make-step (kind description function source)
  "Create an inspectable step of KIND.

DESCRIPTION explains the operation, FUNCTION performs it, and SOURCE is the
instruction that produced it."
  (combobulate-envelope-machine-step-create
   :kind kind
   :description description
   :function function
   :source source))

(defun combobulate-envelope-machine--push-plan (runtime plan)
  "Push a fresh continuation frame for PLAN onto RUNTIME."
  (push (combobulate-envelope-machine-frame-create :plan plan)
        (combobulate-envelope-machine-runtime-stack runtime))
  (combobulate-envelope-machine--record
   runtime 'push-plan :name (combobulate-envelope-machine-plan-name plan)))

(defun combobulate-envelope-machine--enter-plan-step (plan source)
  "Return a step that pushes PLAN when the step produced by SOURCE runs."
  (combobulate-envelope-machine--make-step
   'enter-plan
   (format "enter %s" (combobulate-envelope-machine-plan-name plan))
   (lambda (runtime _frame)
     (combobulate-envelope-machine--push-plan runtime plan))
   source))

(defun combobulate-envelope-machine--insert-step (text source)
  "Return a step that inserts TEXT when the step produced by SOURCE runs."
  (combobulate-envelope-machine--make-step
   'insert
   (format "insert %S" text)
   (lambda (runtime _frame)
     (insert text)
     (combobulate-envelope-machine--record runtime 'insert :text text))
   source))

(defun combobulate-envelope-machine--newline-step (indent source)
  "Return a newline step produced by SOURCE.

When INDENT is non-nil, resolve indentation against the live buffer after the
newline has been inserted."
  (combobulate-envelope-machine--make-step
   (if indent 'newline-and-indent 'newline)
   (if indent "newline and indent live buffer" "insert newline")
   (lambda (runtime _frame)
     (newline)
     (when indent
       (funcall (or (combobulate-envelope-machine-runtime-indent-function runtime)
                    #'indent-according-to-mode)))
     (combobulate-envelope-machine--record
      runtime
      (if indent 'newline-and-indent 'newline)
      :column (current-column)))
   source))

(defun combobulate-envelope-machine--capture-column-step (slot source)
  "Return a step that captures the live column in frame-local SLOT."
  (combobulate-envelope-machine--make-step
   'capture-column
   (format "capture live column in %S" slot)
   (lambda (runtime frame)
     (let ((column (current-column)))
       (push (cons slot column)
             (combobulate-envelope-machine-frame-locals frame))
       (combobulate-envelope-machine--record
        runtime 'capture-column :slot slot :column column)))
   source))

(defun combobulate-envelope-machine--restore-column-step (slot source)
  "Return a step that inserts the column saved in frame-local SLOT.

This intentionally mirrors the small but important behavior of the production
`save-column' instruction: after its nested sequence ends, it inserts enough
spaces to return the surrounding template to the captured column."
  (combobulate-envelope-machine--make-step
   'restore-column
   (format "restore column saved in %S" slot)
   (lambda (runtime frame)
     (let ((column (alist-get slot
                              (combobulate-envelope-machine-frame-locals frame)
                              nil nil #'eq)))
       (unless (integerp column)
         (error "No saved column for %S" slot))
       (insert (make-string column ?\s))
       (combobulate-envelope-machine--record
        runtime 'restore-column :slot slot :column column)))
   source))

(defun combobulate-envelope-machine--register-value (runtime tag)
  "Return TAG's runtime register value or a readable default."
  (or (alist-get tag
                  (combobulate-envelope-machine-runtime-registers runtime)
                  nil nil #'equal)
      (if (symbolp tag) (symbol-name tag) (format "%s" tag))))

(defun combobulate-envelope-machine--materialize-prompt-step (tag label source)
  "Return a step that inserts a disabled field for TAG and LABEL."
  (combobulate-envelope-machine--make-step
   'materialize-prompt
   (format "materialize disabled prompt %S" tag)
   (lambda (runtime _frame)
     (let* ((start (point))
            (value (combobulate-envelope-machine--register-value runtime tag)))
       (insert value)
       ;; Adjacent template insertions must not become part of the field.  A
       ;; front-advancing start and non-advancing end keep both boundaries
       ;; outside later insertions at exactly those positions.
       (let ((overlay (make-overlay start (point) nil t nil)))
         (overlay-put overlay 'face 'shadow)
         (overlay-put overlay 'combobulate-envelope-machine-field tag)
         (push (combobulate-envelope-machine-field-create
                :tag tag
                :label label
                :overlay overlay
                :state 'disabled)
               (combobulate-envelope-machine-runtime-fields runtime)))
       (combobulate-envelope-machine--record
        runtime 'materialize-prompt :tag tag :value value)))
   source))

(defun combobulate-envelope-machine--activate-prompt-step (tag label source)
  "Return a step that activates TAG and suspends for a prompt value."
  (combobulate-envelope-machine--make-step
   'activate-prompt
   (format "activate prompt %S" tag)
   (lambda (runtime _frame)
     (let ((fields (combobulate-envelope-machine-fields-for-tag runtime tag)))
       (unless fields
         (error "Cannot activate prompt %S before materializing it" tag))
       (dolist (field fields)
         (setf (combobulate-envelope-machine-field-state field) 'active)
         (overlay-put (combobulate-envelope-machine-field-overlay field)
                      'face 'highlight))
       (setf (combobulate-envelope-machine-runtime-suspension runtime)
             (combobulate-envelope-machine-suspension-create
              :kind 'prompt
              :payload (list :tag tag :label label)))
       (combobulate-envelope-machine--record
        runtime 'activate-prompt :tag tag :label label)))
   source))

(defun combobulate-envelope-machine--choice-step (choices source)
  "Return a step that suspends the runtime with precompiled CHOICES."
  (combobulate-envelope-machine--make-step
   'choice
   (format "choose one of %s"
           (mapconcat #'combobulate-envelope-machine-choice-name choices ", "))
   (lambda (runtime _frame)
     (setf (combobulate-envelope-machine-runtime-suspension runtime)
           (combobulate-envelope-machine-suspension-create
            :kind 'choice
            :payload choices))
     (combobulate-envelope-machine--record
      runtime 'suspend-choice
      :choices (mapcar #'combobulate-envelope-machine-choice-name choices)))
   source))

(defun combobulate-envelope-machine--compile-choice (source alternatives)
  "Compile ALTERNATIVES from choice instruction SOURCE."
  (unless alternatives
    (error "A choice requires at least one alternative"))
  (let ((choices))
    (dolist (alternative alternatives)
      (let ((name (plist-get alternative :name))
            (instructions (plist-get alternative :instructions)))
        (unless (and (stringp name) (listp instructions))
          (error "Invalid POC choice alternative: %S" alternative))
        (push (combobulate-envelope-machine-choice-create
               :name name
               :plan (combobulate-envelope-machine-compile
                      instructions (format "choice:%s" name)))
              choices)))
    (combobulate-envelope-machine--choice-step (nreverse choices) source)))

(defun combobulate-envelope-machine--compile-sequence (instructions name)
  "Compile INSTRUCTIONS into a plan named NAME.

Prompt materialization remains in lexical position.  Prompt activation steps
are collected in ACTIVATIONS and appended to this plan's interaction tail.
Nested plans own their own interaction tails, which keeps branch-local prompts
inside the selected branch."
  (let ((steps) (activations))
    (dolist (instruction instructions)
      (pcase instruction
        ((and (pred stringp) text)
         (setq steps
               (nconc steps
                      (list (combobulate-envelope-machine--insert-step
                             text instruction)))))
        ('n
         (setq steps
               (nconc steps
                      (list (combobulate-envelope-machine--newline-step
                             nil instruction)))))
        ('n>
         (setq steps
               (nconc steps
                      (list (combobulate-envelope-machine--newline-step
                             t instruction)))))
        (`(b . ,body)
         (let ((plan (combobulate-envelope-machine-compile body "block")))
           (setq steps
                 (nconc steps
                        (list (combobulate-envelope-machine--enter-plan-step
                               plan instruction))))))
        (`(save-column . ,body)
         (let* ((slot (gensym "saved-column-"))
                (plan (combobulate-envelope-machine-compile body "save-column")))
           (setq steps
                 (nconc
                  steps
                  (list
                   (combobulate-envelope-machine--capture-column-step
                    slot instruction)
                   (combobulate-envelope-machine--enter-plan-step
                    plan instruction)
                   (combobulate-envelope-machine--restore-column-step
                    slot instruction))))))
        ((or `(p ,tag ,label) `(prompt ,tag ,label))
         (unless (stringp label)
           (error "Prompt label must be a string: %S" instruction))
         (setq steps
               (nconc steps
                      (list
                       (combobulate-envelope-machine--materialize-prompt-step
                        tag label instruction))))
         (setq activations
               (nconc activations
                      (list
                       (combobulate-envelope-machine--activate-prompt-step
                        tag label instruction)))))
        (`(choice . ,alternatives)
         (setq steps
               (nconc steps
                      (list
                       (combobulate-envelope-machine--compile-choice
                        instruction alternatives)))))
        (_ (error "Unsupported POC envelope instruction: %S" instruction))))
    (combobulate-envelope-machine-plan-create
     :name name
     :steps (vconcat steps activations))))

(defun combobulate-envelope-machine-compile (instructions &optional name)
  "Precompute INSTRUCTIONS as an inspectable plan named NAME."
  (unless (listp instructions)
    (error "Envelope machine instructions must be a list"))
  (combobulate-envelope-machine--compile-sequence
   instructions (or name "envelope")))

(cl-defun combobulate-envelope-machine-start
    (plan &key registers indent-function)
  "Create a runtime for compiled PLAN.

REGISTERS is an alist of initial prompt values.  INDENT-FUNCTION, when non-nil,
is called after every `n>' newline; otherwise the active major mode's
`indent-according-to-mode' is used."
  (unless (combobulate-envelope-machine-plan-p plan)
    (error "Not an envelope machine plan: %S" plan))
  (let ((runtime
         (combobulate-envelope-machine-runtime--create
          :registers (copy-tree registers)
          :indent-function indent-function)))
    (combobulate-envelope-machine--push-plan runtime plan)
    runtime))

(defun combobulate-envelope-machine--fill-ready-queue (runtime)
  "Schedule the next plan step on RUNTIME if possible.

Empty frames are popped until a frame with work is found.  Scheduling does not
execute a step closure."
  (while (and (null (combobulate-envelope-machine-runtime-queue-head runtime))
              (null (combobulate-envelope-machine-runtime-suspension runtime))
              (combobulate-envelope-machine-runtime-stack runtime))
    (let* ((frame (car (combobulate-envelope-machine-runtime-stack runtime)))
           (plan (combobulate-envelope-machine-frame-plan frame))
           (steps (combobulate-envelope-machine-plan-steps plan))
           (index (combobulate-envelope-machine-frame-index frame)))
      (if (< index (length steps))
          (progn
            (setf (combobulate-envelope-machine-frame-index frame) (1+ index))
            (combobulate-envelope-machine--enqueue
             runtime
             (combobulate-envelope-machine-task-create
              :step (aref steps index)
              :frame frame)))
        (pop (combobulate-envelope-machine-runtime-stack runtime))
        (combobulate-envelope-machine--record
         runtime 'pop-plan :name (combobulate-envelope-machine-plan-name plan))))))

(defun combobulate-envelope-machine-done-p (runtime)
  "Return non-nil when RUNTIME has no queued, stacked, or suspended work."
  (and (null (combobulate-envelope-machine-runtime-stack runtime))
       (null (combobulate-envelope-machine-runtime-queue-head runtime))
       (null (combobulate-envelope-machine-runtime-suspension runtime))))

(defun combobulate-envelope-machine-step (runtime)
  "Execute at most one ready step closure on RUNTIME.

Return `:ran' after an ordinary step, `:suspended' when interaction is required,
or `:done' once all frames and tasks are exhausted.  Popping exhausted frames
and scheduling a task are bookkeeping operations; this function still invokes
no more than one `combobulate-envelope-machine-step-function'."
  (if (combobulate-envelope-machine-runtime-suspension runtime)
      :suspended
    (combobulate-envelope-machine--fill-ready-queue runtime)
    (if-let ((task (combobulate-envelope-machine--dequeue runtime)))
        (let* ((step (combobulate-envelope-machine-task-step task))
               (frame (combobulate-envelope-machine-task-frame task)))
          (combobulate-envelope-machine--record
           runtime 'run-step
           :step-kind (combobulate-envelope-machine-step-kind step)
           :description (combobulate-envelope-machine-step-description step))
          (funcall (combobulate-envelope-machine-step-function step)
                   runtime frame)
          (if (combobulate-envelope-machine-runtime-suspension runtime)
              :suspended
            :ran))
      :done)))

(defun combobulate-envelope-machine-run (runtime)
  "Run RUNTIME until it finishes or reaches an interaction suspension."
  (let ((status :ran))
    (while (eq status :ran)
      (setq status (combobulate-envelope-machine-step runtime)))
    status))

(defun combobulate-envelope-machine-resume-choice (runtime name)
  "Resume choice-suspended RUNTIME with the precompiled branch named NAME."
  (let ((suspension (combobulate-envelope-machine-runtime-suspension runtime)))
    (unless (and suspension
                 (eq (combobulate-envelope-machine-suspension-kind suspension)
                     'choice))
      (error "Runtime is not suspended for a choice"))
    (let* ((choices (combobulate-envelope-machine-suspension-payload suspension))
           (choice (seq-find
                    (lambda (candidate)
                      (equal name
                             (combobulate-envelope-machine-choice-name candidate)))
                    choices)))
      (unless choice
        (error "Unknown choice %S; expected one of %S"
               name
               (mapcar #'combobulate-envelope-machine-choice-name choices)))
      (setf (combobulate-envelope-machine-runtime-suspension runtime) nil)
      (combobulate-envelope-machine--record runtime 'resume-choice :name name)
      (combobulate-envelope-machine--push-plan
       runtime (combobulate-envelope-machine-choice-plan choice))
      runtime)))

(defun combobulate-envelope-machine--update-prompt-fields (runtime tag value)
  "Replace TAG's materialized fields in RUNTIME with VALUE."
  ;; Work from right to left so replacing an earlier field cannot invalidate a
  ;; later field's numeric location.  Markers would also work, but explicit
  ;; ordering keeps this POC's mutation rule visible.
  (let ((fields
         (sort (copy-sequence
                (combobulate-envelope-machine-fields-for-tag runtime tag))
               (lambda (left right)
                 (> (overlay-start
                     (combobulate-envelope-machine-field-overlay left))
                    (overlay-start
                     (combobulate-envelope-machine-field-overlay right)))))))
    (dolist (field fields)
      (let* ((overlay (combobulate-envelope-machine-field-overlay field))
             (start (overlay-start overlay)))
        (save-excursion
          (goto-char start)
          (delete-region start (overlay-end overlay))
          (insert value)
          (move-overlay overlay start (point)))
        (setf (combobulate-envelope-machine-field-state field) 'complete)
        (overlay-put overlay 'face nil)))))

(defun combobulate-envelope-machine-resume-prompt (runtime value)
  "Resume prompt-suspended RUNTIME with VALUE."
  (let ((suspension (combobulate-envelope-machine-runtime-suspension runtime)))
    (unless (and suspension
                 (eq (combobulate-envelope-machine-suspension-kind suspension)
                     'prompt))
      (error "Runtime is not suspended for a prompt"))
    (let* ((payload (combobulate-envelope-machine-suspension-payload suspension))
           (tag (plist-get payload :tag)))
      (unless (stringp value)
        (error "Prompt value must be a string: %S" value))
      (combobulate-envelope-machine--update-prompt-fields runtime tag value)
      (setf (alist-get tag
                       (combobulate-envelope-machine-runtime-registers runtime)
                       nil nil #'equal)
            value)
      (setf (combobulate-envelope-machine-runtime-suspension runtime) nil)
      (combobulate-envelope-machine--record
       runtime 'resume-prompt :tag tag :value value)
      runtime)))

(provide 'combobulate-envelope-machine)
;;; combobulate-envelope-machine.el ends here
