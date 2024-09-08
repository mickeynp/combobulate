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

(when (fboundp 'iedit-mode)
  (require 'iedit))

;; Generic wrappers for multiple cursors. Add support for other types
;; of multi-cursor editing (like iedit)

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
                  ;; Essential. There's a million things in here that
                  ;; throw errors left and right that'll trigger
                  ;; `post-command-hook' to eject us from it,
                  ;; preventing us from updating again.
                  (ignore-errors
                    (combobulate--refactor-update-field
                     ov tag
                     (let* ((to (query-replace-compile-replacement minibuffer-text t))
                            ;; `replace-eval-replacement' and
                            ;; `query-replace-compile-replacement'
                            ;; rewrites `\,(...)'  elisp forms and
                            ;; detects regexp backslash groups such as
                            ;; `\N' and rewrites them as
                            ;; `(match-string N)' --- great. However,
                            ;; we cannot use this feature as it
                            ;; assumes match data is set (as it
                            ;; ordinarily would be in a normal
                            ;; search&replace loop) and the coterie of
                            ;; functions that depend on it.
                            ;;
                            ;; As we're effectively hacking up the
                            ;; replace machinery to work with
                            ;; Combobulate's refactoring system, we
                            ;; either need to rewrite uses of
                            ;; `match-string' to a custom function of
                            ;; our own choosing; ensure match data is
                            ;; set so it matches the field overlays in
                            ;; the buffer, which is complex; or just
                            ;; `cl-letf' it here temporarily.
                            (replacement-text
                             (cl-letf  (((symbol-function 'match-string)
                                         (lambda (n &optional string)
                                           (overlay-get (nth n ovs) 'combobulate-refactor-field-original-text))))
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
                          ;; `combobulate-cursor-substitute-match' is
                          ;; set to match backslash groups and they
                          ;; are syntactically simple: the first
                          ;; character is a backslash and the rest is
                          ;; a number. We can just extract the number
                          ;; and use it to index into the overlays.
                          (if-let (matched-ov (let ((num (string-to-number (substring sub 1))))
                                                ;; The number 0 is
                                                ;; special. In an
                                                ;; ordinary
                                                ;; search&replace loop
                                                ;; it refers to the
                                                ;; whole match. Here
                                                ;; we repurpose it to
                                                ;; mean the original
                                                ;; text of the CURRENT
                                                ;; field.
                                                (if (= num 0) (nth idx ovs) (nth num ovs))))
                              (overlay-get matched-ov 'combobulate-refactor-field-original-text)
                            ;; No match, return the default text in the minibuffer.
                            minibuffer-text))
                        replacement-text))
                     ;; required, as we need to ensure `default-text' is
                     ;; set to a default value that won't cause issues
                     ;; down the road if the eval replacement call does
                     ;; not yield anything.
                     ""))
                  (cl-incf idx))
                ovs))))))

(defun combobulate-cursor-edit-nodes (nodes &optional action ctx-node)
  "Edit NODES with multiple cursors placed at ACTION.

Where ACTION is `before', `after', or `mark'.

CTX-NODE is the node that was used to generate NODES, such as a
parent node. It is only used for messaging."
  (let ((prompt (concat "Editing " (combobulate-tally-nodes nodes t)
                        (and ctx-node (format " in `%s'"
                                              (propertize
                                               (combobulate-pretty-print-node ctx-node)
                                               'face 'combobulate-active-indicator-face))))))
    (cond
     ((null nodes) (error "There are no editable nodes."))
     (t (let ((proxy-nodes (combobulate-proxy-node-make-from-nodes nodes))
              (tag 'edit))
          (combobulate-refactor ()
            (mapc #'mark-node-deleted proxy-nodes)
            (commit)
            (mapc (lambda (node)
                    (mark-field (combobulate-node-start node) tag (combobulate-node-text node)))
                  proxy-nodes)
            (let ((buf (current-buffer)))
              (save-match-data
                (combobulate-envelope-prompt
                 prompt
                 nil
                 buf
                 (combobulate-cursor--update-function buf tag)
                 combobulate-cursor-substitute-default)))
            (commit))))
     (nil (combobulate--mc-place-nodes (mapcar (lambda (node) (cons action node)) nodes))
          (combobulate-message prompt)))))

(provide 'combobulate-cursor)
;;; combobulate-cursor.el ends here
