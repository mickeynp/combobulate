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
;; `tempo' and `skeleton', albeit with far more features. See
;; `combobulate-envelope-expand-instructions' for how to write
;; envelope templates.

;;; Code:

(require 'generator)
(require 'combobulate-settings)
(require 'combobulate-manipulation)
(require 'eieio)

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
  "When zero, interactive envelope commands are ignored.")

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
          (combobulate--refactor-get-overlays))))

(cl-defstruct (combobulate-envelope-context
               (:constructor combobulate-envelope-context-create)
               (:copier nil))
  "The context of a combobulate envelope during its expansion."
  start end user-actions)

(defun combobulate-envelope-expand-instructions-1 (instructions)
  "Internal function that expands INSTRUCTIONS."
  (let ((buf (current-buffer))
        (user-actions)
        (end)
        (start (point-marker)))
    (ignore end)
    (cl-flet ((expand-block (rest-instructions categories)
                ;; Expand a block of instructions.
                (let ((ctx (combobulate-envelope-expand-instructions-1 rest-instructions))
                      (expanded-ctx))
                  (setq expanded-ctx
                        (combobulate-envelope-expand-post-run-instructions
                         ctx
                         ;; categories to expand right now. Note that we
                         ;; intentionally exclude `point' as the default
                         ;; as they should only be run once everything
                         ;; else is finalised: they are literally the only
                         ;; thing to run after everything else is done.
                         categories))
                  (setq user-actions
                        (nconc user-actions
                               (combobulate-envelope-context-user-actions
                                expanded-ctx)))
                  (setq end (combobulate-envelope-context-end expanded-ctx)))))
      (combobulate-refactor (:id combobulate-envelope-refactor-id)
        (dolist (sub-instruction instructions)
          (pcase sub-instruction
            ;; `(b instructions)'
            ;; `(b* instructions)'
            ;; Execute INSTRUCTIONS in a block, and interactively ask
            ;; the user to complete `repeat' and `choice' instructions.
            ;;
            ;; The special block `b*' will also execute `point' instructions.
            ((or (and `(b . ,rest) (let categories '(repeat choice prompt)))
                 (and `(b* ,categories . ,rest)))
             (expand-block rest categories))
            ;; `(choice instructions)'
            ;; `(choice* :name NAME :missing MISSING :rest INSTRUCTIONS)'
            ;;
            ;; Presents a prompt to the user to choose between any of
            ;; the choices in the same block. `choice' is the simplest,
            ;; and `choice*' is more complex.
            ;;
            ;; `choice*' allows you to specify a name for the choice; it
            ;; is shown in the prompt. The `:missing' keyword argument
            ;; is a string that is shown if the user does not pick that
            ;; choice. `:rest' is the instructions to execute if the
            ;; user picks that choice.
            ((or (and `(choice . ,rest)
                      (let name nil)
                      (let missing nil)
                      (let rest-instructions rest))
                 (and `(choice* . ,rest)
                      (let name (plist-get rest :name))
                      (let missing (plist-get rest :missing))
                      (let rest-instructions (plist-get rest :rest))))
             (push `(choice ,(point-marker) ,name ,missing ,rest-instructions
                            ,(apply-partially #'combobulate-envelope-expand-instructions-1
                                              rest-instructions))
                   user-actions))
            ;; `(save-column BLOCK)'
            ;;
            ;; Records the `current-column' of `point' when it enters
            ;; BLOCK and resets `point' to that column when after exiting.
            (`(save-column . ,rest)
             (let ((col (current-column)))
               (expand-block rest nil)
               ;; (delete-horizontal-space)
               (insert (make-string col ? ))))
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
             (let ((prompt-point (point-marker)))
               (push (cons 'prompt
                           (lambda () (save-excursion
                                        (goto-char prompt-point)
                                        (mark-field prompt-point tag (combobulate-envelope-get-register tag) transformer-fn)
                                        (unless combobulate-envelope-static
                                          (let ((new-text (or (combobulate-envelope-get-register tag)
                                                              (combobulate-envelope-prompt
                                                               prompt tag nil
                                                               (lambda ()
                                                                 (combobulate-envelope--update-prompts
                                                                  buf tag (minibuffer-contents)))))))
                                            (push (cons tag new-text) combobulate-envelope--registers)
                                            (combobulate-envelope--update-prompts buf tag new-text))))))
                     user-actions)))
            ;; `(field TAG)' or `(f TAG)'
            ;;
            ;; If there is a matching prompt TAG, update its text to that value.
            ((or (and `(field ,tag) (let transformer-fn nil))
                 (and `(f ,tag) (let transformer-fn nil))
                 (and `(field ,tag ,transformer-fn))
                 (and `(f ,tag ,transformer-fn)))
             (mark-field (point-marker) tag (combobulate-envelope-get-register tag) transformer-fn))
            ;; `@>'
            ;;
            ;; Push a `point-marker' that will moves with insertions
            ;; made at the marker.
            ('@> (push `(point ,(let ((m (point-marker)))
                                  (set-marker-insertion-type m t)
                                  m))
                       user-actions))
            ;; `@'
            ;;
            ;; Push a `point-marker' that will serve as a possible
            ;; placement point for point after expansion.
            ('@ (push `(point ,(point-marker))
                      user-actions))
            ;; `@@'
            ;;
            ;; Push a `point' that will serve as a possible
            ;; placement point for point after expansion.
            ('@@ (push `(point ,(point)) user-actions))
            ;; `n' or `n>'
            ;;
            ;; Calls `newline', or `newline' then `indent-according-to-mode'.
            ;;
            ;; Never `newline-and-indent' because it strips horizontal
            ;; space, which is unhelpful.
            ('n (newline))
            ('n> (newline) (indent-according-to-mode))
            ;; `>'
            ;;
            ;; Call `indent-according-to-mode'
            ('> (indent-according-to-mode))
            ;; `<'
            ;;
            ;; For whitespace-sensitive languages, this is a way to move
            ;; back one level of indentation.
            ('< (let ((col (or (and combobulate-envelope-deindent-function
                                    (funcall combobulate-envelope-deindent-function))
                               0)))
                  (delete-horizontal-space)
                  (insert (make-string col ? ))))
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
               (let ((offset (current-indentation)))
                 (delete-horizontal-space)
                 (setf start (point))
                 ;; clear whitespace from the start of the line
                 (let ((before-pt (point)))
                   (insert (combobulate-indent-string
                            default
                            :first-line-operation 'absolute
                            :first-line-amount offset
                            :rest-lines-operation 'relative))
                   (save-excursion
                     (goto-char before-pt)
                     (back-to-indentation)
                     (push `(point ,(point-marker)) user-actions)))))
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
                                    (let ((sub-inst (combobulate-envelope-expand-instructions-1 repeat-instructions)))
                                      (setq user-actions (append user-actions (cdr sub-inst))))
                                    (cl-decf max-repeat)
                                    (commit))
                           (commit))))))
               ;; capture `C-g' (`keyboard-quit') so that a user can
               ;; enter an expansion and back out one step.
               ;;
               ;; The actual cleanup is done when
               ;; `combobulate-refactor' captures the uncaught error
               ;; and undoes everything.
               (quit (combobulate-message "Keyboard quit. Undoing expansion."))))
            (_ (error "Unknown sub-instruction: %S" sub-instruction)))))
      (combobulate-envelope-context-create
       :start start
       :end end
       :user-actions user-actions))))

(defun combobulate-envelope-render-choice-preview (action)
  "Render a preview of the envelope at INDEX.

Unlike most proffer preview functions, this one assumes that
`accept-action' passed to `combobulate-proffer-choices' is
`commit' and not its usual value of `rollback'."
  (with-slots (index current-node proxy-nodes refactor-id) action
    (combobulate-refactor (:id refactor-id)
      (let ((pt)
            (combobulate-envelope-static t)
            (combobulate-envelope--undo-on-quit nil))
        (dolist (node proxy-nodes)
          (let ((expand-envelope) (is-current-node))
            (pcase-let ((`(,missing .  ,rest-envelope) (combobulate-proxy-node-extra node)))
              (combobulate-move-to-node node)
              (cond ((equal node current-node)
                     (setq expand-envelope rest-envelope
                           is-current-node t
                           pt (point)))
                    (t (setq expand-envelope missing)))
              (pcase-let (((cl-struct combobulate-envelope-context
                                      (start start)
                                      (end end)
                                      (user-actions user-actions))
                           (combobulate-refactor (:id refactor-id)
                             (combobulate-envelope-expand-instructions-1
                              ;; Normally we'd just use `(b ...)' but we
                              ;; want the points calculated also, if
                              ;; there are any in the envelope, so we
                              ;; can pull out the `selected-point' and
                              ;; use it to set `pt' later.
                              `((b* (repeat choice prompt point) ,@expand-envelope))))))
                (mark-range-deleted start end)
                (when is-current-node
                  (pcase user-actions
                    (`((selected-point . ,selected-pt))
                     (setq pt selected-pt)))
                  (mark-range-highlighted start end))))))
        (when pt (goto-char pt))))))

(cl-defun combobulate-envelope-expand-post-run-instructions (ctx categories)
  "Expand the user actions in CTX according to CATEGORIES.

CATEGORIES is a list of instructions to expand now.

Valid choices are: `prompt', `choice', `repeat' and `point'. All
other categories are ignored.

Every instruction in CTX's `:user-actions' must be of the form

   (TYPE . REST)

Where TYPE is one of the CATEGORIES and REST could be anything,
depending on TYPE.

This function will expand the user actions in the order
they are given in CATEGORIES."
  (pcase-let (((cl-struct combobulate-envelope-context
                          (user-actions user-actions))
               ctx))
    ;; We need to group the instructions by category so that we can
    ;; action each category as one cohesive whole. `seq-group-by'
    ;; preserves the relative order in the user actions, which
    ;; is also important.
    (let ((selected-point) (grouped-instructions (seq-group-by #'car user-actions))
          (remaining-user-actions)
          (end (point-marker)))
      ;; The set of categories we're asked to process is possibly a
      ;; subset of the user actions we've been given. All
      ;; instructions that we have not been told to process are passed
      ;; through unchanged.
      (setq remaining-user-actions (seq-remove (lambda (x) (member (car x) categories)) user-actions))
      ;; Re-use the global refactor ID here so we manipulate the same
      ;; refactoring instance as the progenitor instance the envelope
      ;; code was first activated with.
      (combobulate-refactor (:id combobulate-envelope-refactor-id)
        (dolist (category categories)
          (pcase (assoc category grouped-instructions)
            (`(prompt . ,prompts)
             (save-excursion (mapc #'funcall (mapcar #'cdr prompts))))
            (`(choice . ,choices)
             (let ((nodes))
               (pcase-dolist (`(choice ,pt ,name ,missing ,rest-envelope ,text) choices)
                 (push (combobulate-proxy-node-create
                        :start pt
                        :end pt
                        :text text
                        :named t
                        :type "Choice"
                        :pp (if name (format "Choice: %s" name) "Choice")
                        :extra (cons missing rest-envelope))
                       nodes))
               (when-let (selected-node
                          (combobulate-proffer-choices
                           nodes
                           #'combobulate-envelope-render-choice-preview
                           ;; ordinarily, we'd want to filter out nodes
                           ;; that have identical node ranges. However,
                           ;; with choices, we may well have multiple
                           ;; choices in a row, each occupying the exact
                           ;; same range, but nevertheless expanding to
                           ;; vastly different things.
                           :unique-only nil
                           ;; pass whatever the value of
                           ;; `combobulate-envelope-static' is to the
                           ;; proffer function. If it's non-nil, then
                           ;; the caller of this function does not
                           ;; intend for the user to make a choice;
                           ;; instead, the first is picked
                           ;; automatically. The automatic choice is
                           ;; made because we want to expand some
                           ;; instructions (like prompt and choice)
                           ;; without actually triggering a user
                           ;; interaction
                           :first-choice combobulate-envelope-static
                           :signal-on-abort t
                           :quiet t
                           :reset-point-on-abort nil
                           :reset-point-on-accept nil
                           ;; `combobulate-envelope-render-choice-preview'
                           ;; inserts text for potentially many nodes,
                           ;; which would be preserved if the normal
                           ;; accept action -- rollback -- were used
                           ;; instead.
                           :accept-action 'commit))
                 ;; If one of the proffered choices was selected, then
                 ;; we need to:
                 ;;
                 ;; 1. Move to the node
                 ;;
                 ;; 2. Expand the envelope found in either `:missing'
                 ;;    or `:rest'. The `:missing' envelope is expanded
                 ;;    if the node is not the selected node, and the
                 ;;    `:rest' envelope is expanded if the node is the
                 ;;    selected node.
                 ;;
                 ;; 3. The outcome of recursively expanding the
                 ;;    envelope will yield user actions that
                 ;;    require further processing. However, these
                 ;;    user actions may include categories the
                 ;;    `b' block cannot process itself. Namely, that
                 ;;    is almost always just `point' nodes. We'll need
                 ;;    to walk each user action in turn and put
                 ;;    them back into the grouped instructions alist
                 ;;    so they can be processed in turn.
                 (dolist (node nodes)
                   (pcase-let ((`(,missing . ,rest-envelope) (combobulate-proxy-node-extra node)))
                     (combobulate-move-to-node node)
                     (pcase-let (((cl-struct combobulate-envelope-context
                                             (user-actions user-actions)
                                             (end ctx-end))
                                  (combobulate-envelope-expand-instructions-1
                                   ;; If the node is selected we use
                                   ;; the rest-envelope; for
                                   ;; everything else, the missing
                                   ;; envelope.
                                   ;;
                                   ;; Regardless of the envelope, we
                                   ;; ensure it's wrapped in an
                                   ;; implicit `b' block.
                                   `((b ,@(if (equal node selected-node) rest-envelope missing))))))
                       (pcase-dolist (`(,block-category . ,user-action) user-actions)
                         ;; If we're dealing with any sort of block
                         ;; instruction that is part of the categories
                         ;; we are dealing with, put them back into
                         ;; the grouped instructions alist so they can
                         ;; be processed in turn.
                         (if (member block-category categories)
                             (setf (alist-get block-category grouped-instructions)
                                   (cons (cons block-category user-action)
                                         (alist-get block-category grouped-instructions)))
                           (push (cons block-category user-action) remaining-user-actions)))
                       (setq end (max end ctx-end))))))))
            (`(selected-point . ,pts)
             ;; it's possible there's more than one selected-point, I
             ;; suppose? It should not happen, though.
             (dolist (pt pts)
               (goto-char (cdr pt))))
            (`(point . ,points)
             (let ((nodes (mapcar (lambda (pt-instruction)
                                    (combobulate-proxy-node-make-point-node (cadr pt-instruction)))
                                  points)))
               ;; Ensure every single point node has a cursor visible
               ;; so the user can see the available cursor choices.
               (mapc #'mark-node-cursor nodes)
               (save-excursion
                 (if-let (selected-node (combobulate-proffer-choices
                                         nodes
                                         (lambda-slots (current-node refactor-id)
                                           (combobulate-refactor (:id refactor-id)
                                             (combobulate-move-to-node current-node)))
                                         ;; as above, if we're in static mode, we do not
                                         ;; prompt the user to pick a cursor
                                         :first-choice combobulate-envelope-static
                                         :signal-on-abort t
                                         :quiet t
                                         :reset-point-on-abort t
                                         :reset-point-on-accept nil))
                     (setq selected-point (combobulate-node-start selected-node))
                   (setq selected-point nil)))
               ;; `selected-point' is a special post-run block
               ;; instruction that we only ever action once we've
               ;; exited the entire envelope instruction loop. It is
               ;; the final action carried out at the very end.
               (push (cons 'selected-point selected-point) remaining-user-actions)))))
        (if combobulate-envelope-static
            (rollback)
          (commit)))
      (combobulate-envelope-context-create
       :user-actions remaining-user-actions
       :start nil
       :end end))))

(defun combobulate-envelope-expand-instructions (instructions &optional registers)
  "Expand an envelope of INSTRUCTIONS at point.

Combobulate envelopes work in much the same way as Tempo or
Skeletons, but Combobulate's envelopes are more powerful.

Here are some of the differences:

1. Prompts are executed in the minibuffer, much like the
   aforementioned tools, but they also update interactively as
   you type. You can have transformers that alter some or all of
   the prompts and fields you use.

2. Regions are now stored in a register, and thus `r', `r>' and
   so on are in effect just using those.

3. The indentation algorithm now \"understands\" block-based,
whitespace-sensitive languages like Python better.

   This follows on from point #2: special care is made in
   whitespace-sensitive languages like Python. Combobulate will
   attempt to ensure the indentation is correct for the block you
   wish to insert the code into. Combobulate can only do this if
   the underlying Python mode's indentation engine is capable of
   determining the correct indentation for a given line. If it is
   not, then Combobulate will not be able to determine the
   correct indentation either.

4. Remembering a previous line's indentation is very difficult
   with other templating tools. Combobulate simplifies this with
   the `save-column' form. When Combobulate enters a
   `save-column' form it saves the column offset (but not point!)
   and restores the column on exit. That makes it possible to
   have nested sequences of code and be assured that the column
   is reset correctly when you exit the block. This is only of
   importance in whitespace-sensitive languages where Combobulate
   cannot safely indent the whole region.

5. You can now explicitly place point with `@'. Multiple
   instances of `@' are remembered and presented to you at the
   end of the expansion so that you can choose which one to place
   your point at.

7. Repetition (also a feature in Skeleton, but not Tempo) is also
   possible with `repeat' and `repeat-1'. Note that indentation
   in whitespace-sensitive languages can be difficult to control
   with these forms. Keep them simple if you can.

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

   Both register names hold the marked region (which is likely
   the triggering node, if the user did not mark a region
   themselves) when the envelope is activated. The value is
   stored in `combobulate-envelope-registers'.

   The DEFAULT is a fallback value in case the REGISTER does not
   exist.

   Instructions ending with `>' also indent the inserted text
   according to the major mode's indentation preferences. It uses
   `indent-region' for languages that are not
   whitespace-sensitive.

   However, if `combobulate-envelope-indent-region-function' is
   nil (as it is in the likes of `python-mode') then a
   specialized indentation system is used instead. The relative
   indentation at the point of envelope invocation is preserved
   and used to indent the inserted register according to its new
   column offset when it is inserted and indented by `r>'.

 `(b BLOCK)'

   Execute the BLOCK of instructions and, when exiting, action
   all the user actions that require user input:

     - `repeat' instructions are executed first;
     - Then, `choice' prompts are executed;
     - Then, `prompt's are executed.

   All envelopes are wrapped an implicit `b' block. You really
   only need this construct if you're doing something very
   specific, such as multiple distinct choice groupings.

 `(choice BLOCK)'
 `(choice* :name NAME :missing MISSING-BLOCK :rest BLOCK)'

   Collect all choice instructions in the current `b' block and
   present them to the user to choose one. The BLOCK is a list of
   instructions that are executed when the user picks that
   choice.

   The `choice*' form is a variant that allows you to specify a
   name for the choice, a block to execute if the choice is
   *not* picked, and a block to execute if the choice is picked.

   Warning: If you are using multiple `choice*' instructions with
   `:missing' properties set in a row, you may run into expansion
   problems.

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
   Insert a point marker at point. Point markers move with the
   text being inserted, which is probably what you want. After
   expansion, your point is placed at this location. If there is
   more than one, then you are asked to pick the one to jump to.

 `@@'

   Track the absolute position of point. This is useful if you
   want to place point at a specific location after expansion,
   and because `@' does not put your point where you want it to.

 `n'
 `n>'

   Call `newline' or `newline' followed by
   `indent-according-to-mode'.

   Indentation is done according to your major mode. Pay
   attention if you use an envelope in a whitespace-sensitive
   language like Python or YAML: you may need to use
   `save-column' to remember the indentation of the previous
   line, or `<' to remove one level of semantic indentation.

 `<'

   Remove one level of indentation from the current line. This
   operation only works in select, whitespace-sensitive modes
   like Python or YAML.

 `(save-column BLOCK)'

   Saves point's *column* -- but not point itself! -- when
   entering BLOCK. Use this to remember indentation offset from
   the beginning of the line.

   This is mostly of use in whitespace-sensitive languages like
   Python.

   You are strongly encouraged to place a singular `n' at the end
   of BLOCK: this will ensure your point is placed on a new line
   with the correct indentation.

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
  (let ((start (point))
        (end (point-marker))
        (change-group (prepare-change-group))
        (state 'start)
        (combobulate-envelope--registers (append registers combobulate-envelope-registers))
        (combobulate-envelope-refactor-id (combobulate-refactor-setup))
        (selected-point (point)))
    (activate-change-group change-group)
    (if (use-region-p)
        (progn
          (indent-region (point) (mark) nil)
          (let ((col (current-indentation))
                (text (substring-no-properties (delete-and-extract-region (point) (mark)))))
            (push (cons 'region-indented (combobulate-indent-string-first-line text col))
                  combobulate-envelope--registers)
            (push (cons 'region text) combobulate-envelope--registers)
            ;; deactivate the mark as the region would otherwise interfere
            ;; with the expansion.
            (setq mark-active nil))))
    (cl-assert (eq state 'start))
    (combobulate-refactor (:id combobulate-envelope-refactor-id)
      (condition-case nil
          (progn
            (let ((ctx (combobulate-envelope-expand-instructions-1
                        ;; Build the base template for the envelope we are to
                        ;; expand. The base template is just `b*', which is a
                        ;; "super-block" that expands all user actions such as
                        ;; choice, repeat, prompt and -- for `b*' specifically --
                        ;; also `point'.
                        `((b* (repeat choice prompt point selected-point) ,@instructions)))))
              ;; The `point' category is special in that it is
              ;; executed only at the `b*' superblock stage. If there
              ;; is more than one `point', the user is asked to
              ;; choose: that choice is then put back into the context
              ;; as `selected-point'. This code will then move point
              ;; to that location.
              ;;
              ;; It's a little bit hacky, but we assume that as we
              ;; come out of this expansion of post-run instructions,
              ;; that where ever point is, is what we want to end the
              ;; envelope at.
              (combobulate-envelope-expand-post-run-instructions
               ctx
               '(selected-point))
              (setq selected-point (point))
              ;; Track the contextual end of the envelope.
              (set-marker end (combobulate-envelope-context-end ctx)))
            (commit)
            (setq state 'success))
        (quit (rollback) (setq state 'error))))
    ;; Amalgamate all the changes into one single change. If a user
    ;; accepts an envelope, but changes their mind, they won't have to
    ;; undo multiple times to return to the state they were in before
    ;; the envelope was expanded.
    (undo-amalgamate-change-group change-group)
    ;; Only when both `combobulate-envelope--undo-on-quit' and `state'
    ;; is `error' is set do we cancel the change group. The
    ;; `combobulate-envelope--undo-on-quit' variable is there to
    ;; prevent cancellations during proffer choices where the envelope
    ;; is previewed.
    (if (and (eq state 'error) combobulate-envelope--undo-on-quit)
        (cancel-change-group change-group)
      (accept-change-group change-group))
    ;; Throw a courtesy region indent call if we support such a
    ;; thing. (We do not in the likes of Python, where indenting a
    ;; region is dangerous.
    (when combobulate-envelope-indent-region-function
      (apply combobulate-envelope-indent-region-function
             (combobulate-extend-region-to-whole-lines start end)))
    (cons (cons start end) selected-point)))

(defun combobulate-envelop-region (template)
  "Insert Combobulate TEMPLATE around the active region."
  (interactive)
  (when (and (use-region-p) (> (point) (mark))) (exchange-point-and-mark))
  (skip-chars-forward combobulate-skip-prefix-regexp)
  (exchange-point-and-mark)
  (skip-chars-backward combobulate-skip-prefix-regexp)
  (combobulate-envelope-expand-instructions template))

(defun combobulate-get-envelope-by-name (name)
  "Find an envelope with `:name' equal to NAME."
  (seq-find (lambda (envelope) (equal (plist-get envelope :name) name))
            (append combobulate-manipulation-envelopes
                    (combobulate-get-envelopes-by-major-mode))))

(defun combobulate-get-envelopes-by-major-mode ()
  (mapcan
   (lambda (parser) (alist-get (combobulate-parser-language parser)
                               combobulate-manipulation-envelopes-custom))
   (combobulate-parser-list)))

(defun combobulate-get-envelope-function-by-name (name)
  "Find an envelope with `:name' equal to NAME."
  (when-let (env (combobulate-get-envelope-by-name name))
    (symbol-function (plist-get env :template-symbol))))

(defun combobulate-apply-envelope (envelope &optional node region)
  "Envelop NODE near point or active region with ENVELOPE.

If REGION is non-nil, envelop the region instead of NODE."
  (map-let (:nodes :mark-node :description :template :point-placement :name :procedures) envelope
    (unless (and name)
      (error "Envelope `%s' is not valid." envelope))
    (if region
        (combobulate-envelop-region template)
      (with-navigation-nodes (:nodes nodes :procedures procedures)
        (if (setq node (or node (combobulate--get-nearest-navigable-node)))
            (save-excursion
              ;; If we are asked to mark the node, we do. If not, we still go to
              ;; the beginning
              (if mark-node
                  (combobulate--mark-node node t)
                (cond
                 ;; nothing to do if point is `stay'.
                 ((member point-placement '(start end))
                  (combobulate--goto-node node (eq point-placement 'end)))))
              ;; triggering an envelope will invalidate `node'.
              (setq node (combobulate-proxy-node-make-from-nodes node))
              (prog1 (combobulate-envelope-expand-instructions template)
                (combobulate-message "Enveloping" node "in" description)))
          (error "Cannot apply envelope `%s'. Point must be in one of \
these nodes: `%s'." name nodes))))))

(defun combobulate-envelope-get-shorthand-procedure (shorthand)
  "Get the procedure given a SHORTHAND.

Raise an error if the SHORTHAND is not valid."
  (let ((procedure (alist-get shorthand combobulate-envelope-procedure-shorthand-alist)))
    (unless procedure
      (error "Shorthand `%s' is not valid." shorthand))
    procedure))

(defun combobulate-envelope-get-applicable-nodes (envelope &optional _force)
  "Given an ENVELOPE, return a list of valid nodes to apply it to.

The NODES are the nodes that the envelope can be applied to. The
NODES must be a list of strings or procedures. If a string, it is
the name of a node type. If a procedure, then it follows the
procedural rules laid out in `combobulate-procedure-apply'."
  (map-let (:nodes :shorthand) envelope
    ;; only nodes or shorthand can be used in the same envelope, never both.
    (if (and nodes shorthand)
        (error "Envelope `%s' has both `:nodes' and `:shorthand' defined. \
Only one can be used." envelope))
    (let* ((string-elements (seq-filter #'stringp nodes))
           (procedure-elements
            (append (seq-filter #'listp nodes)
                    (and shorthand (combobulate-envelope-get-shorthand-procedure shorthand)))))
      (mapcar #'combobulate-procedure-result-action-node
              (append
               (and procedure-elements (combobulate-procedure-start (point) procedure-elements t))
               ;; As we have no selectors nor a parent discriminator, we just use the action node.
               (combobulate-procedure-start (point) `((:activation-nodes ((:nodes ,string-elements)))) t))))))

(defun combobulate-execute-envelope (envelope-name &optional node force)
  "Executes any envelope with a `:name' equal to ENVELOPE-NAME.

See `combobulate-apply-envelope' for more information."
  (let ((envelope (combobulate-get-envelope-by-name envelope-name))
        ;; Default to true as we'll only turn this off briefly when we
        ;; expand an envelope during a proffer preview.
        (combobulate-envelope--undo-on-quit t)
        (chosen-node) (accepted nil))
    (unless envelope
      (error "There is no such envelope registered with the name `%s'"
             envelope-name))
    (if (region-active-p)
        (combobulate-apply-envelope envelope nil t)
      (if node
          (when-let (target-pt (cdr (combobulate-apply-envelope envelope node)))
            (goto-char target-pt))
        (let ((combobulate-envelope-static t)
              (envelope-nodes
               (if (or (plist-member envelope :nodes) (plist-member envelope :shorthand))
                   (combobulate-envelope-get-applicable-nodes envelope force)
                 ;; if we don't have any assigned envelope nodes,
                 ;; create a proxy node at point; that node (and
                 ;; thus `point') will instead be where the
                 ;; envelope is inserted.
                 (list (combobulate-proxy-node-make-point-node)))))
          (progn
            (setq chosen-node
                  (combobulate-proffer-choices
                   (seq-sort
                    (lambda (a b)
                      ;; "Smart" sorting that orders by largest node first but
                      ;; *only* when the distance from `point' to the start of `a'
                      ;; is 0 (i.e., the node starts at point.)
                      ;;
                      ;; For all other instances, we measure distance from point.
                      (if (= (- (combobulate-node-start a) (point)) 0)
                          (combobulate-node-larger-than-node-p a b)
                        (> (- (combobulate-node-start a) (point))
                           (- (combobulate-node-start b) (point)))))
                    (reverse envelope-nodes))
                   (lambda-slots (current-node refactor-id)
                     (combobulate-refactor (:id refactor-id)
                       (let ((ov (mark-node-highlighted current-node))
                             (combobulate-envelope--undo-on-quit nil))
                         (seq-let [[start &rest end] &rest pt]
                             (combobulate-apply-envelope envelope current-node)
                           (goto-char pt)
                           ;; lil' hack. The extent of the node is not
                           ;; the same as the envelope we just
                           ;; applied.
                           (move-overlay ov start end)))))
                   :reset-point-on-abort t
                   :reset-point-on-accept nil))
            (setq accepted t)))
        ;; here we simply repeat what ever the selected choice was, as
        ;; an explicit node skips the proffering process entirely.
        (when (and chosen-node accepted)
          (combobulate-execute-envelope envelope-name chosen-node))))))

(provide 'combobulate-envelope)
;;; combobulate-envelope.el ends here
