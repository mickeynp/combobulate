;;; combobulate-envelope.el --- code templating and snippet expansion for combobulate  -*- lexical-binding: t; -*-

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

;; Code templating and snippet expansions that work similarly to
;; `tempo', `skeleton', etc.

;;; Code:

(require 'generator)
(require 'combobulate-settings)
(require 'combobulate-manipulation)

(defvar combobulate-envelope-indent-region-function)

(defvar combobulate-envelope-prompt-history nil
  "History for `combobulate-envelope-prompt'.")

(defvar combobulate-envelope-prompt-window nil)

(defvar combobulate-envelope-registers nil
  "Registers of data for Combobulate envelopes.")

(defvar combobulate-envelope--undo-on-quit nil
  "If t, then undo on keyboard quit.

Note that this is an internal variable and should not be set
manually.")

(defvar combobulate-envelope--registers nil
  "Internal store for Combobulate envelopes.")

(defvar combobulate-envelope-static nil
  "When non-nil, interactive commands are ignored.")

(defvar combobulate-envelope-refactor-id nil
  "The ID of the current refactor.")

(defvar-keymap combobulate-envelope-prompt-map
  :doc "Keymap for envelope prompts."
  :parent minibuffer-local-map)

(defun combobulate-envelope-prompt (prompt default-value &optional buffer update-fn)
  "Insert text into fields using the minibuffer with PROMPT and DEFAULT-VALUE.

BUFFER if optionally the buffer (and its associated window) to
use. If it is nil, then `current-buffer' is used."
  (let ((win (when (eq (window-buffer) (or buffer (current-buffer)))
               (selected-window))))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq combobulate-envelope-prompt-window win)
          ;; not presently used.
          ;; (add-hook 'minibuffer-exit-hook #'combobulate-envelope-prompt-exit nil t)
          (add-hook 'post-command-hook update-fn nil t))
      (read-from-minibuffer
       (format-prompt
        prompt
        (or default-value (car combobulate-envelope-prompt-history) ""))
       nil
       combobulate-envelope-prompt-map
       nil
       'combobulate-envelope-prompt-history
       (car combobulate-envelope-prompt-history)
       'inherit-input-method))))

(defun combobulate-envelope-get-register (register &optional default)
  "Retrieve the value of REGISTER from `combobulate-envelope--registers'.

If the register does not exist, return DEFAULT or nil."
  (map-elt combobulate-envelope--registers register default))

(defun combobulate-envelope-prompt-expansion (prompt)
  "Ask a `yes-or-no-p' question with PROMPT."
  (yes-or-no-p prompt))

(defun combobulate-envelope--update-prompts (buffer tag text)
  "In BUFFER, update prompt fields named TAG with TEXT."
  (with-current-buffer buffer
    (mapc (lambda (ov)
            (if (combobulate--refactor-field-has-tag-p ov tag)
                (overlay-put ov 'face 'combobulate-refactor-field-face)
              (overlay-put ov 'face 'combobulate-refactor-inactive-field-face))
            (combobulate--refactor-update-field ov tag text (symbol-name tag)))
          (combobulate--refactor-get-all-overlays))))

(defun combobulate-envelope-expand-instructions-1 (instructions &optional categories)
  "Internal function that expands INSTRUCTIONS."
  (let ((buf (current-buffer))
        (post-instructions)
        (end (point-marker))
        (start (point-marker)))
    (combobulate-refactor (:id combobulate-envelope-refactor-id)
      (dolist (sub-instruction instructions)
        (pcase sub-instruction
          ((or (and `(choice . ,rest)
                    (let name nil)
                    (let missing nil)
                    (let rest-instructions rest))
               (and `(choice* . ,rest)
                    (let name (plist-get rest :name))
                    (let missing (plist-get rest :missing))
                    (let rest-instructions (plist-get rest :rest))))
           ;; check if we're already in static display mode: if we are,
           ;; do nothing; that should stop nested choices from being
           ;; expanded.
           (unless combobulate-envelope-static
             (let ((combobulate-envelope-static t))
               (push `(choice ,(point-marker) ,name ,missing ,rest-instructions
                              ,(apply-partially #'combobulate-envelope-expand-instructions-1
                                                rest-instructions))
                     post-instructions))))
          ;; `(save-column BLOCK)'
          ;;
          ;; Records the `current-column' of `point' when it enters
          ;; BLOCK and resets `point' to that column when after exiting.
          (`(save-column . ,rest)
           (let ((col (current-column)))
             (setq post-instructions
                   (nconc post-instructions
                          (cdr (combobulate-envelope-expand-instructions-1
                                rest '(repeat prompt)))))
             (move-to-column col t)))
          ;; `(prompt TAG PROMPT [TRANSFORM-FN])' / `(p TAG PROMPT [TRANSFORM-FN])'
          ;;
          ;; Prompts the user with PROMPT and stores the returned value
          ;; against TAG.  Any fields tagged TAG (alongside the prompt
          ;; itself) are updated automatically.
          ((or (and `(prompt ,tag ,prompt) (let transformer-fn nil))
               (and `(p ,tag ,prompt) (let transformer-fn nil))
               (and `(prompt ,tag ,prompt ,transformer-fn))
               (and `(p ,tag ,prompt ,transformer-fn)))
           (when (and transformer-fn (not (functionp transformer-fn)))
             (error "Prompt has invalid transformer function `%s'" transformer-fn))
           (let ((prompt-point (point)))
             (mark-field prompt-point tag (combobulate-envelope-get-register tag) transformer-fn)
             (push (cons 'prompt
                         (lambda () (save-excursion
                                 (goto-char prompt-point)
                                 (unless combobulate-envelope-static
                                   (let ((new-text (or (combobulate-envelope-get-register tag)
                                                       (combobulate-envelope-prompt
                                                        prompt tag nil
                                                        (lambda ()
                                                          (combobulate-envelope--update-prompts
                                                           buf tag (minibuffer-contents)))))))
                                     (push (cons tag new-text) combobulate-envelope--registers)
                                     (combobulate-envelope--update-prompts buf tag new-text))))))
                   post-instructions)))
          ;; `(field TAG)' or `(f TAG)'
          ;;
          ;; If there is a matching prompt TAG, update its text to that value.
          ((or (and `(field ,tag) (let transformer-fn nil))
               (and `(f ,tag) (let transformer-fn nil))
               (and `(field ,tag ,transformer-fn))
               (and `(f ,tag ,transformer-fn)))
           (mark-field (point) tag (combobulate-envelope-get-register tag) transformer-fn))
          ;; `@'
          ;;
          ;; Push a `point-marker' that will serve as a possible
          ;; placement point for point after expansion.
          ('@ (push `(point ,(point-marker)) post-instructions))
          ;; `n' or `n>'
          ;;
          ;; Calls `newline' or `newline-and-indent'
          ('n (newline))
          ('n> (newline) (indent-according-to-mode))
          ;; `>'
          ;;
          ;; Call `indent-according-to-mode'
          ('> (indent-according-to-mode))
          ('< (let ((col (- (current-column) 4)))
                (newline)
                (move-to-column col t)))
          ;; `(r> REGISTER [DEFAULT])' and `(r REGISTER [DEFAULT])'; or `r>' and `r'
          ;;
          ;; Inserts the register REGISTER (retreived from
          ;; `combobulate-envelope--registers') or, if there is no
          ;; register specified, default to the REGISTER `region' (or
          ;; `region-indented' if
          ;; `combobulate-envelope-indent-region-function' is nil) which
          ;; holds that captured region (if any).
          ;;
          ;; Forms ending with `>' are indented as per the major mode's
          ;; indentation engine. Forms without `>' are not indented at all.
          ((or
            ;; surely there's a better way than this?
            (and 'r>
                 (let register nil)
                 (let default "")
                 (let indent t))
            (and 'r
                 (let register nil)
                 (let default "")
                 (let indent nil))
            (and `(r> ,register)
                 (let indent t)
                 (let default ""))
            (and `(r ,register)
                 (let indent nil)
                 (let default ""))
            (and `(r> ,register ,default)
                 (let indent t))
            (and `(r ,register ,default)
                 (let indent nil)))
           (setq default (combobulate-envelope-get-register
                          (or register
                              (if (and (null combobulate-envelope-indent-region-function) indent)
                                  'region-indented
                                'region))
                          default))
           (cond
            ((and combobulate-envelope-indent-region-function indent)
             (funcall combobulate-envelope-indent-region-function
                      (point) (progn (insert default) (point))))
            ;; if `combobulate-envelope-indent-region-function' is nil
            ;; then we default to a simplistic indentation style that
            ;; works well with the likes of Python where crass,
            ;; region-based indentation will never work.
            ((and (not combobulate-envelope-indent-region-function) indent)
             (forward-line 0)
             (insert (combobulate-indent-string
                      default
                      :first-line-operation 'absolute
                      :first-line-amount (current-indentation)
                      :rest-lines-operation 'relative)))
            (t (insert default))))
          ;; "string"
          ;;
          ;; Strings are inserted at point.
          ((and (pred stringp) s)
           (insert s))
          ;; `(repeat BLOCK)' or `(repeat-1 BLOCK)'
          ;;
          ;; Repeats BLOCK an unlimited number of times or at most once.
          ((or (and `(repeat . ,repeat-instructions) (let max-repeat most-positive-fixnum))
               (and `(repeat-1 . ,repeat-instructions) (let max-repeat 1)))
           (condition-case nil
               ;; we start with `repeat-answer' set to `t' by default
               ;; because we want to expand `repeat-instructions'
               ;; *first* and *then*  prompt the user if they want
               ;; to keep the now-displayed instruction.
               (let ((repeat-answer t))
                 (when combobulate-envelope-static
                   (setq max-repeat 1))
                 (while (and repeat-answer (> max-repeat 0))
                   (combobulate-refactor ()
                     ;; call with `combobulate-envelope-static' set to
                     ;; `t'. When an envelope is static, no
                     ;; interactive functions are called (prompts and
                     ;; such).
                     ;;
                     ;; That way we can safely insert the template
                     ;; knowing that it won't block for user input.
                     (seq-let [[inst-start &rest inst-end] &rest _]
                         (let ((combobulate-envelope-static t))
                           (combobulate-envelope-expand-instructions-1 repeat-instructions))
                       ;; mark the range as highlighted, so it's
                       ;; easier to see its extent; and as deleted,
                       ;; so that -- due to how we're using
                       ;; `combobulate-refactor' -- we can delete
                       ;; the expansion immediately after the
                       ;; prompt.

                       ;; BUG: if
                       ;; `combobulate-envelope-expand-instructions-1'
                       ;; ends up calling `save-column' as its last form
                       ;; before exiting, then the call to set the column
                       ;; will corrupt the `inst-end' value resulting in
                       ;; text being left behind.
                       (mark-range-deleted inst-start inst-end)
                       (mark-range-highlighted inst-start inst-end)
                       ;; note that regardless of whether we accept
                       ;; or decline the expansion, we `commit'
                       ;; (i.e., delete!) the expansion we created
                       ;; above. The reason this is done is so that
                       ;; we ditch the static ersatz template and
                       ;; instead re-run it, and this time
                       ;; interactively so prompts and the like are
                       ;; invoked.
                       (if (setq repeat-answer
                                 (or combobulate-envelope-static
                                     (combobulate-envelope-prompt-expansion "Apply this expansion? ")))
                           (progn (commit)
                                  (let ((sub-inst (combobulate-envelope-expand-instructions-1
                                                   repeat-instructions)))
                                    (setq post-instructions (append post-instructions (cdr sub-inst))))
                                  (cl-decf max-repeat)
                                  (commit))
                         (commit))))))
             ;; capture `C-g' (`keyboard-quit') so that a user can
             ;; enter an expansion and back out one step.
             ;;
             ;; The actual cleanup is done when
             ;; `combobulate-refactor' captures the uncaught error
             ;; and undoes everything.
             (quit (combobulate-message "Keyboard quit. Undoing expansion."))))))
      (prog1 (let ((post-instructions
                    (combobulate-envelope-expand-post-run-instructions
                     post-instructions
                     ;; categoires to expand right now. Note that we intentionally
                     ;; exclude `point' as they should only be run once everything
                     ;; else is finalised: they are literally the only thing to run
                     ;; after everything else is done.
                     (or categories '(prompt repeat choice)))))
               (cons (cons start (car post-instructions)) (cdr post-instructions)))
        (rollback)))))

(defun combobulate-envelope-render-preview (index current-node proxy-nodes refactor-id)
  (combobulate-refactor (:id refactor-id)
    (let ((marker (point-marker))
          (combobulate-envelope-static t)
          (combobulate-envelope--undo-on-quit nil))
      (dolist (node proxy-nodes)
        (let ((expand-envelope) (is-current-node))
          (pcase-let ((`(,missing . ,rest-envelope) (combobulate-proxy-node-extra node)))
            (combobulate-move-to-node node)
            (cond ((equal node current-node)
                   (setq expand-envelope rest-envelope
                         is-current-node t))
                  (t (setq expand-envelope missing)))
            (seq-let [[start &rest end] &rest pt]
                (combobulate-refactor (:id combobulate-envelope-refactor-id)
                  (prog1
                      (combobulate-envelope-expand-instructions-1 expand-envelope)
                    (rollback)))
              (mark-range-deleted start end)
              (when is-current-node
                (mark-range-highlighted start end)))))))))

(cl-defun combobulate-envelope-expand-post-run-instructions (collected-instructions categories)
  "Execute COLLECTED-INSTRUCTIONS if they are one of CATEGORIES.

CATEGORIES is a list of instructions to expand now. Valid choices are:
`prompt', `choice', `repeat' and `point'. All other categories
are ignored.

Every instruction in COLLECTED-INSTRUCTIONS is of the form

   (TYPE . REST)

Where TYPE is one of the CATEGORIES and REST could be anything,
depending on TYPE. The instructions are grouped by TYPE and
executed in the order presented in CATEGORIES.

Where MARK-FIELD is the inherited `combobulate-refactor'
functions that were set globally when the envelope expansion
procedure first began. "
  ;; loop over COLLECTION-INSTRUCTIONS and remove any that aren't in CATEGORIES.
  (let ((selected-point) (grouped-instructions (seq-group-by #'car collected-instructions))
        (remaining-instructions)
        (start (point))
        (end (point-marker))
        (result))
    (setq remaining-instructions (seq-remove (lambda (x) (member (car x) categories)) collected-instructions))
    (combobulate-refactor (:id combobulate-envelope-refactor-id)
      (dolist (category categories)
        (pcase (assoc category grouped-instructions)
          (`(prompt . ,prompts)
           (save-excursion (mapc #'funcall (mapcar #'cdr prompts))))
          (`(choice . ,choices)
           (let ((node) (nodes))
             (pcase-dolist (`(choice ,pt ,name ,missing ,rest-envelope ,text) choices)
               (push (make-combobulate-proxy-node
                      :start pt
                      :end pt
                      :text text
                      :named t
                      :type "Choice"
                      :pp (format "Choice: %s" name)
                      :extra (cons missing rest-envelope))
                     nodes)
               (rollback))
             (when-let (selected-node
                        (combobulate-proffer-choices
                         nodes
                         #'combobulate-envelope-render-preview
                         ;; ordinarily, we'd want to filter out nodes
                         ;; that have identical node ranges. However,
                         ;; with choices, we may well have multiple
                         ;; choices in a row, each occupying the exact
                         ;; same range, but nevertheless expanding to
                         ;; vastly different things.
                         :unique-only nil
                         :reset-point-on-abort nil
                         :reset-point-on-accept nil
                         :accept-action 'commit))
               (dolist (node nodes)
                 (let ((expand-envelope) (is-selected-node))
                   (pcase-let ((`(,missing . ,rest-envelope) (combobulate-proxy-node-extra node)))
                     (combobulate-move-to-node node)
                     (cond ((equal node selected-node)
                            (setq expand-envelope rest-envelope
                                  is-selected-node t))
                           (t (setq expand-envelope missing)))
                     (combobulate-envelope-expand-instructions-1 expand-envelope)))))))
          (`(point . ,points)
           (let ((nodes (mapcar (lambda (pt-instruction)
                                  (combobulate-make-proxy-point-node (cadr pt-instruction)))
                                points)))
             (mapc #'mark-node-cursor nodes)
             (setq selected-point
                   (combobulate-node-start
                    (combobulate-proffer-choices
                     nodes
                     (lambda (index current-node proxy-nodes refactor-id)
                       (combobulate-refactor (:id refactor-id)
                         (combobulate-move-to-node current-node)))
                     :first-choice combobulate-envelope-static
                     :reset-point-on-abort t
                     :reset-point-on-accept nil)))
             (push (cons 'selected-point selected-point) remaining-instructions)
             (rollback)))))
      (rollback))
    (cons end remaining-instructions)))

(defun combobulate-envelope-expand-instructions (instructions &optional registers)
  "Expand an envelope of INSTRUCTIONS at point.

Combobulate envelopes work in much the same way as Tempo or
Skeletons but with a handful of differences:

1. Prompts are executed in the minibuffer, much like the
aforementioned tools, but they also update interactively as you
type.

2. Prompts (and their fields) are placed inline and are only
expanded at the end. This may occasionally cause issues if you
have a complex template. The solution is to recognize that code
templating is no substitute for a real keyboard macro or elisp.

3. Regions are now stored in a register, and thus `r', `r>' and
so on are in effect just using those.

4. The indentation algorithm now \"understands\" block-based,
whitespace-sensitive languages like Python better.

5. Remembering a previous line's indentation is very difficult
with other templating tools. Combobulate simplifies this with the
`save-column' form. When Combobulate enters a `save-column' form
it saves the column offset (but not point!) and restores the
column on exit. That makes it possible to have nested sequences
of code and be assured that the column is reset correctly when
you exit the block.

6. You can now explicitly place point with `@'. Multiple
instances of `@' are remembered and presented to you at the end
of the expansion so that you can choose which one to place your
point at.

7. Repetition (also a feature in Skeleton, but not Tempo) is also
possible with `repeat' and `repeat-1'.

If there is an active REGION, then everything between `point' and
`mark' is extracted and deleted and made available to
INSTRUCTIONS through the register system. It is accessed
implicitly by calling `r' or `r>' without a form; or explicitly
with `(r REGISTER [DEFAULT])' where REGISTER is `region'.

The following instructions are supported by Combobulate's envelope
expansion:

 `>'

   Call `indent-according-to-mode' at point.

 `(r REGISTER [DEFAULT])'
 `(r> REGISTER [DEFAULT])'
 `r'
 `r>'

   Insert REGISTER at point. Where REGISTER defaults to
   `region' (when `combobulate-envelope-indent-region-function'
   is non-nil) or `region-indented' when it is nil.

   Both names hold the thus the
   marked region (if there is one) when the envelope is
   activated. The value is stored in
   `combobulate-envelope-registers'.

   The DEFAULT is a fallback value in case the REGISTER does not
   exist.

   Instructions ending with `>' also indent the inserted text
   according to the major mode's indentation preferences. It uses
   `indent-region'.

   However, if `combobulate-envelope-indent-region-function' is
   nil (as it is in the likes of `python-mode') then a
   specialized indentation mechanism is used that checks *point's
   current column* and reindents the register's text, preserving
   the relative indentation between the lines in the region.

 `(prompt TAG PROMPT [TRANSFORMER-FN])'
 `(p TAG PROMPT [TRANSFORMER-FN])'

   Place a prompt named TAG at point and interactively query the
   user with PROMPT. Any input is optionally run through
   TRANSFORMER-FN which takes one function argument: the input
   string.

   Prompts are commonly matched with fields, which replicate the
   input of a prompt.

   Prompt values are stored in
   `combobulate-envelope-registers'. If there is already an entry
   for TAG in that variable, then no interactive prompt is
   presented, and the stored value is used instead.

 `(field TAG [TRANSFORMER-FN])'
 `(f TAG [TRANSFORMER-FN])'

   Like a prompt, but only inserts the text belonging to its
   prompt named TAG.

 `@'

   Insert a marker at point. After expansion, your point is
   placed at this location. If there is more than one, then you
   are asked to pick the one to jump to.

 `n'
 `n>'

   Call `newline' or `newline-and-indent'. Indentation is
   done according to your major mode.

 `(save-column BLOCK)'

   Saves point's *column* -- but not point itself! -- when
   entering BLOCK. Use this to remember the relative offset from
   the beginning of the line.

   Pay attention when you place `n>' and `n' at the end of (or
   immediately outside) a `save-column' block.

 `(repeat BLOCK)'
 `(repeat-1 BLOCK)'

   Temporarily inserts BLOCK and then asks if you want to keep
   it.  If you answer yes, the BLOCK is kept; if you answer no,
   it is removed and Combobulate exits the repeat instruction
   form and carries on.

   The instruction `repeat-1' is identical to `repeat' except it
   only allows at most 1 entry.

 \"STRING\"

   Insert STRING at point."
  (when (and (use-region-p) (> (point) (mark))) (exchange-point-and-mark))
  (let ((result) (start (point))
        (end (point-marker))
        (change-group (prepare-change-group))
        (should-unwind nil)
        (is-success nil)
        (undo-outer-limit nil)
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum)
        (combobulate-envelope--registers (append registers combobulate-envelope-registers))
        (combobulate-envelope-refactor-id (combobulate-refactor-setup "foo"))
        (selected-point (point)))
    (activate-change-group change-group)
    (when (use-region-p)
      (let ((col (current-indentation))
            (text (delete-and-extract-region (point) (mark))))
        (push (cons 'region-indented (combobulate-indent-string-first-line text col))
              combobulate-envelope--registers)
        (push (cons 'region text) combobulate-envelope--registers))
      ;; deactivate the mark as the region would otherwise interfere
      ;; with the expansion.
      (setq mark-active nil))
    (combobulate-refactor (:id combobulate-envelope-refactor-id)
      (unwind-protect
          (progn
            ;; HACK: passing `mark-field' (from `combobulate-refactor')
            ;; around like this is a messy work-around because of how
            ;; `cl-flet' scoping works.
            (setq result (cdr (combobulate-envelope-expand-instructions-1 instructions)))
            (set-marker end (point))
            ;; Some instructions are meant to be run at the very end, after
            ;; all recursive expansions have taken place. Proffered point
            ;; locations are one such example.
            (let ((post-run-instructions (cdr (combobulate-envelope-expand-post-run-instructions result '(point)))))
              (pcase-dolist (`('selected-point . ,pt) post-run-instructions)
                (setq selected-point pt)))
            (commit)
            (setq is-success t))
        (rollback)
        (unless is-success
          ;; We cannot cancel a change group inside a
          ;; `combobulate-refactor' (which itself uses change groups) as
          ;; that... that seems to break Emacs. So instead we use a flag
          ;; to indicate we unwound due to an error.
          (setq should-unwind t))))
    ;; Only when both `combobulate-envelope--undo-on-quit' and
    ;; `should-unwind' is set do we cancel. The
    ;; `combobulate-envelope--undo-on-quit' exist purely to prevent
    ;; cancellations during proffer choices where the envelope is
    ;; previewed.
    (if (and should-unwind combobulate-envelope--undo-on-quit)
        (cancel-change-group change-group)
      (accept-change-group change-group))
    (when combobulate-envelope-indent-region-function
      (apply combobulate-envelope-indent-region-function
             (combobulate-extend-region-to-whole-lines start end)))
    (cons (cons start end) selected-point)))

(provide 'combobulate-envelope)
;;; combobulate-envelope.el ends here
