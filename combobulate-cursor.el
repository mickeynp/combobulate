;;; combobulate-cursor.el --- third-party integrations for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.1
;; Homepage: https://www.github.com/mickeynp/combobulate
;; Keywords: convenience, tools, languages

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

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(defvar mc--default-cmds-to-run-once)


(declare-function mc/remove-fake-cursors "multiple-cursors-core.el")
(declare-function mc/maybe-multiple-cursors-mode "multiple-cursors-core.el")
(declare-function mc/create-fake-cursor-at-point "multiple-cursors-core.el")
(declare-function combobulate-tally-nodes "combobulate-manipulation.el")
(declare-function combobulate-procedure-collect-activation-nodes "combobulate-navigation")

(defvar multiple-cursors-mode)
(when (fboundp 'multiple-cursors-mode)
  (require 'multiple-cursors))

;; Generic wrappers for multiple cursors.
;;
;; TODO: Add support for other types of cursor editing (like iedit)

(defun combobulate--mc-assert-is-supported ()
  (unless (fboundp 'multiple-cursors-mode)
    (error "Multiple cursors is not installed or activated.")))

(defun combobulate--mc-active ()
  "Return non-nil if multiple cursors mode is active."
  (and (fboundp 'multiple-cursors-mode)
       multiple-cursors-mode))

(defun combobulate--mc-clear-cursors ()
  "Clear multiple cursors."
  (mc/remove-fake-cursors))

(defun combobulate--mc-enable ()
  "Enable multiple cursors."
  ;; abysmal MC hack to prevent MC from triggering on the damned
  ;; command that started the whole thing.
  (dolist (ignored-command '(combobulate-edit-sequence-dwim
                             combobulate-edit-node-type-dwim
                             combobulate-edit-node-siblings-dwim
                             combobulate-edit-node-by-text-dwim
                             combobulate-query-builder-edit-nodes
                             combobulate-edit-query))
    (add-to-list 'mc--default-cmds-to-run-once ignored-command))
  (mc/maybe-multiple-cursors-mode))

(defun combobulate--mc-place-cursor ()
  "Place a cursor at the current node."
  (mc/create-fake-cursor-at-point))

(defun combobulate--mc-place-nodes (placement-nodes &optional default-action)
  "Edit PLACEMENT-NODES according to each node's desired placement action.

Must be a list of cons cells where the car is the placement
action and the cdr is the node.

The car should be one of the following symbols:

`before', which puts the point at the node start; `after', which puts the
point at the node end; and `mark', which marks the node.

If DEFAULT-ACTION is non-nil, it is used for labelled nodes that do not have
match a placement action."
  (combobulate--mc-assert-is-supported)
  (if (combobulate--mc-active)
      ;; bail out if mc's running as we don't want to run mc
      ;; inside mc, which will happen if you trigger a command
      ;; from execute-extended-command for... some... reason..
      nil
    (let ((counter 0) (node-point) (do-mark) (matched)
          (default-action (or default-action 'before)))
      (cl-flet ((apply-action (action node)
                  (pcase action
                    ((or 'before '@before) (setq node-point (combobulate-node-start node)))
                    ((or 'after '@after) (setq node-point (combobulate-node-end node)))
                    ((or 'mark '@mark) (setq node-point (combobulate-node-start node)
                                             do-mark t))
                    (_ nil))))
        (combobulate--mc-clear-cursors)
        (pcase-dolist (`(,action . ,node) (reverse placement-nodes))
          (setq do-mark nil)
          (unless (apply-action action node)
            ;; fall back to `default-action' if we get nil back from
            ;; `apply-action'.
            (apply-action default-action node))
          (push (cons action node) matched)
          (if (= counter (- (length placement-nodes) 1))
              (progn (goto-char node-point)
                     (when do-mark (set-mark (combobulate-node-end node))))
            (goto-char node-point)
            (when do-mark (set-mark (combobulate-node-end node)))
            (combobulate--mc-place-cursor))
          (cl-incf counter))
        (combobulate--mc-enable)
        matched))))

(defconst combobulate-cursor-substitute-default "\\0"
  "Substitute that represents the text for each individual node.

This is only used when Combobulate's editing mode is `combobulate'.")

(defconst combobulate-cursor-substitute-match (rx (group "\\" (group (1+ (any "0-9")))))
  "Regexp that matches the backslash and number in a regexp group.

Altering this is possible, but not recommended as other parts of the
code base make assumptions based on this.")

(defun combobulate-cursor--update-function (buf tag)
  "Return a function that updates the overlays in BUF with TAG.

This function is intended to be used in `post-command-hook' to
update the overlays in BUF with TAG. It is returned as a closure
that captures the minibuffer text at the time of its creation.

Overlays must be valid `combobulate-refactor' field overlays."
  (lambda ()
    (let ((idx 0)
          ;; nab the minibuffer text here (before we change the
          ;; current buffer) as we return a closure that runs in
          ;; `post-command-hook' inside the minibuffer prompt
          ;; machinery, and the minibuffer contents function will,
          ;; despite its name, happily return the contents of
          ;; *whatever* buffer is the current buffer, whether it is
          ;; actually a minibuffer or not.
          (minibuffer-text (minibuffer-contents)))
      (with-current-buffer buf
        (let ((ovs (combobulate--refactor-get-overlays)))
          (mapc (lambda (ov)
                  (when (overlay-get ov 'combobulate-refactor-field-enabled)
                    ;; Essential. There's a million things in here
                    ;; that throw errors left and right that'll
                    ;; trigger `post-command-hook' to eject us from
                    ;; it, preventing us from updating again.
                    (ignore-errors
                      (combobulate--refactor-update-field
                       ov tag
                       (let* ((to (query-replace-compile-replacement minibuffer-text t))
                              ;; `replace-eval-replacement' and
                              ;; `query-replace-compile-replacement'
                              ;; rewrites `\,(...)'  elisp forms and
                              ;; detects regexp backslash groups such
                              ;; as `\N' and rewrites them as
                              ;; `(match-string N)' ---
                              ;; great. However, we cannot use this
                              ;; feature as it assumes match data is
                              ;; set (as it ordinarily would be in a
                              ;; normal search&replace loop) and the
                              ;; coterie of functions that depend on
                              ;; it.
                              ;;
                              ;; As we're effectively hacking up the
                              ;; replace machinery to work with
                              ;; Combobulate's refactoring system, we
                              ;; either need to rewrite uses of
                              ;; `match-string' to a custom function
                              ;; of our own choosing; ensure match
                              ;; data is set so it matches the field
                              ;; overlays in the buffer, which is
                              ;; complex; or just `cl-letf' it here
                              ;; temporarily.
                              (replacement-text
                               (cl-letf  (((symbol-function 'match-string)
                                           (lambda (n &optional string)
                                             ;; Subtract 1 because 0
                                             ;; refers to the whole
                                             ;; match
                                             (overlay-get
                                              (nth (1- n) ovs)
                                              'combobulate-refactor-field-original-text))))
                                 (or (replace-eval-replacement
                                      (cond
                                       ((null to) "")
                                       ((stringp to)
		                        (list 'concat to))
		                       (t (cdr to)))
                                      idx)
                                     ""))))
                         (replace-regexp-in-string
                          combobulate-cursor-substitute-match
                          (lambda (sub)
                            ;; Avoid using any sort of matching
                            ;; functions here that touches match
                            ;; data. It does not work well due to
                            ;; `replace-regexp-in-string' doing its own
                            ;; thing with the data.
                            ;;
                            ;; Thankfully,
                            ;; `combobulate-cursor-substitute-match'
                            ;; is set to match backslash groups and
                            ;; they are syntactically simple: the
                            ;; first character is a backslash and the
                            ;; rest is a number. We can just extract
                            ;; the number and use it to index into the
                            ;; overlays.
                            (if-let (matched-ov
                                     (let ((num (string-to-number (substring sub 1))))
                                       ;; The number 0 is special. In
                                       ;; an ordinary search&replace
                                       ;; loop it refers to the whole
                                       ;; match. Here we repurpose it
                                       ;; to mean the original text of
                                       ;; the CURRENT field.  Also,
                                       ;; subtract 1 to account for
                                       ;; the 0-based whole match
                                       ;; index.
                                       (if (= num 0) (nth idx ovs) (nth (1- num) ovs))))
                                (overlay-get matched-ov 'combobulate-refactor-field-original-text)
                              ;; No match, return the default text in the minibuffer.
                              minibuffer-text))
                          replacement-text))
                       ;; required, as we need to ensure `default-text' is
                       ;; set to a default value that won't cause issues
                       ;; down the road if the eval replacement call does
                       ;; not yield anything.
                       "")))
                  (cl-incf idx))
                ovs))))))

(defvar combobulate-cursor--field-tag 'combobulate-cursor-field
  "Tag used to identify Combobulate cursor fields.")

(defvar combobulate-cursor--refactor-id nil
  "The ID of the current refactoring operation in progress.")

(defvar combobulate-cursor--active-buffer nil
  "The buffer that is currently being refactored.")

(defun combobulate-cursor-goto-field (direction)
  "Move to the next or previous field in DIRECTION."
  (with-current-buffer combobulate-cursor--active-buffer
    (let* ((ovs (combobulate--refactor-get-overlays combobulate-cursor--refactor-id))
           (wnd (get-buffer-window combobulate-cursor--active-buffer))
           (window-point (window-point wnd)))
      (if-let (ov (if (eq direction 'next)
                      (seq-find (lambda (ov) (> (overlay-start ov) window-point)) ovs)
                    (seq-find (lambda (ov) (< (overlay-start ov) window-point)) (reverse ovs))))
          (set-window-point wnd (overlay-start ov))
        (user-error "No more fields in this direction.")))))

(defun combobulate-cursor-next-field ()
  "Move to the next field in the cursor editing prompt."
  (interactive)
  (combobulate-cursor-goto-field 'next))

(defun combobulate-cursor-prev-field ()
  "Move to the previous field in the cursor editing prompt."
  (interactive)
  (combobulate-cursor-goto-field 'prev))

(defun combobulate-cursor-toggle-field (&optional pt)
  "Enable or disable the current field in the cursor editing prompt."
  (interactive "d")
  (with-current-buffer combobulate-cursor--active-buffer
    (combobulate-refactor (:id combobulate-cursor--refactor-id)
      (toggle-field (or pt (window-point (get-buffer-window combobulate-cursor--active-buffer)))
                    combobulate-cursor--field-tag))))

(defun combobulate-cursor-invert-fields ()
  "Invert the enabled/disabled state of all fields in the cursor editing prompt."
  (interactive)
  (with-current-buffer combobulate-cursor--active-buffer
    (combobulate-refactor (:id combobulate-cursor--refactor-id)
      (let ((ovs (combobulate--refactor-get-overlays combobulate-cursor--refactor-id)))
        (dolist (ov ovs)
          (combobulate-cursor-toggle-field (overlay-start ov)))))))

(defun combobulate-cursor-help ()
  (interactive)
  (with-electric-help
   (lambda ()
     (insert (substitute-command-keys "Combobulate Cursor Editing Help:

The default prompt value maps to the original text of each node,
represented by `\\0'.

You can use the value from a particular field by using the backslash
followed by the field number. For example, `\\1' refers to the first
field, `\\2' refers to the second field, and so on.

Like Emacs's normal replace commands, you can use the `\\#' shorthand to
insert a number equal to the index of the field, starting from the count
of 0.

You can also evaluate Emacs lisp forms in the context of each field with
the `\,(...)` syntax. For example, `\\,(upcase \\0)' will convert each
field to uppercase.

Cursor Key Bindings:

\\`C-n' - Move to the next field.
\\`C-p' - Move to the previous field.
\\`C-v' - Toggle the current field on or off.
\\`C-i' - Invert the enabled/disabled state of all fields.

General Key Bindings:

\\`C-h' - Show this help message.
\\`RET' - Accept the current prompt.
\\`C-g' - Abort the cursor editing.")))))

(defvar-keymap combobulate-cursor-key-map
  :parent combobulate-envelope-prompt-map
  :doc "Keymap for Combobulate's cursor editing prompt."
  "C-n" #'combobulate-cursor-next-field
  "C-p" #'combobulate-cursor-prev-field
  "C-v" #'combobulate-cursor-toggle-field
  "C-i" #'combobulate-cursor-invert-fields
  "C-h" #'combobulate-cursor-help)



(defun combobulate-cursor-edit-nodes (nodes &optional action ctx-node)
  "Edit NODES with multiple cursors placed at ACTION.

NODES is a list of nodes to edit.

ACTION is the action to take. Depending on the value of
`combobulate-cursor-tool', this can be either `combobulate' or
`multiple-cursors'. If it is `combobulate', the nodes are edited with
Combobulate's refactoring system. If the value is `multiple-cursors',
the nodes are edited with multiple cursors.

For `multiple-cursors', ACTION must be `before', `after', or `mark'. For
`combobulate', the value is disregarded.

CTX-NODE is the node that was used to generate NODES, such as a
parent node. It is only used for messaging."
  (cond
   ((null nodes) (error "There are no editable nodes."))
   ((eq combobulate-cursor-tool 'combobulate)
    (let ((proxy-nodes (combobulate-proxy-node-make-from-nodes nodes))

          (combobulate-cursor--refactor-id (combobulate-refactor-setup))
          (combobulate-cursor--active-buffer (current-buffer)))
      (combobulate-refactor (:id combobulate-cursor--refactor-id)
        ;; Ditch the proxy nodes' text in the buffer and commit the
        ;; deletions immediately. The fields we insert will be the new
        ;; values.
        (mapc #'mark-node-deleted proxy-nodes)
        (commit)
        ;; Create new fields with the new values in the same places
        ;; as the old ones.
        (mapc (lambda (node)
                (mark-field (combobulate-node-start node)
                            combobulate-cursor--field-tag
                            (combobulate-node-text node)))
              proxy-nodes)
        (save-match-data
          (combobulate-envelope-prompt
           (substitute-command-keys "Editing fields. Use \\`C-h' for help.")
           nil
           combobulate-cursor--active-buffer
           (combobulate-cursor--update-function
            combobulate-cursor--active-buffer
            combobulate-cursor--field-tag)
           combobulate-cursor-substitute-default
           combobulate-cursor-key-map))
        (commit))))
   ((eq combobulate-cursor-tool 'multiple-cursors)
    (combobulate--mc-place-nodes (mapcar (lambda (node) (cons action node)) nodes))
    (combobulate-message (concat "Editing " (combobulate-tally-nodes nodes t)
                                 (and ctx-node (format " in `%s'"
                                                       (propertize
                                                        (combobulate-pretty-print-node ctx-node)
                                                        'face 'combobulate-active-indicator-face))))))
   (t (error "Unknown cursor tool: %s" combobulate-cursor-tool))))

(provide 'combobulate-cursor)
;;; combobulate-cursor.el ends here
