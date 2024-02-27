;;; combobulate-contrib.el --- third-party integrations for combobulate  -*- lexical-binding: t; -*-

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
(defvar mc--default-cmds-to-run-once)
(declare-function avy-process "avy.el")
(declare-function mc/remove-fake-cursors "multiple-cursors-core.el")
(declare-function mc/maybe-multiple-cursors-mode "multiple-cursors-core.el")
(declare-function mc/create-fake-cursor-at-point "multiple-cursors-core.el")
(declare-function combobulate-tally-nodes "combobulate-manipulation.el")
(declare-function combobulate-procedure-collect-activation-nodes "combobulate-navigation")

(defun combobulate-avy-jump ()
  "Use avy to jump to a navigable node"
  (interactive)
  ;; Is this right?
  (avy-process (mapcar (lambda (node) (cons (cons (combobulate-node-start node)
                                             (combobulate-node-end node))
                                       (selected-window)))
                       (combobulate--query-tree (combobulate--make-navigation-query)
                                                #'combobulate-node-visible-window-p))))

(defun combobulate-avy-jump-defun ()
  "Use avy to jump to a defun"
  (interactive)
  (with-navigation-nodes (:procedures combobulate-navigation-defun-procedures)
    (combobulate-avy-jump)))

(defvar multiple-cursors-mode)
(when (fboundp 'multiple-cursors-mode)
  (require 'multiple-cursors))

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
  (dolist (ignored-command '(combobulate-edit-cluster-dwim
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

(defun combobulate--mc-edit-nodes (nodes &optional action ctx-node)
  "Edit NODES with multiple cursors placed at ACTION.

Where ACTION is `before', `after', or `mark'.

CTX-NODE is the node that was used to generate NODES, such as a
parent node. It is only used for messaging."
  (cond
   ((null nodes) (error "There are no editable nodes."))
   (t (combobulate--mc-place-nodes (mapcar (lambda (node) (cons action node)) nodes))
      (combobulate-message
       (concat "Editing " (combobulate-tally-nodes nodes t)
               (and ctx-node (format " in `%s'"
                                     (propertize
                                      (combobulate-pretty-print-node ctx-node)
                                      'face 'combobulate-active-indicator-face))))))))

(provide 'combobulate-contrib)
;;; combobulate-contrib.el ends here
