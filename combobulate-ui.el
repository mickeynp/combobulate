;;; combobulate-ui.el --- user interface for combobulate  -*- lexical-binding: t; -*-

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


(require 'map)
(require 'transient)

(require 'combobulate-settings)

(transient-define-prefix combobulate-highlight ()
  "Highlight nodes in the buffer"
  ["Highlight"
   ("h" "Highlight DWIM" combobulate-highlight-dwim-at-point)
   ("q" "Highlight query" combobulate-highlight-query)
   ("c" "Clear all highlights" combobulate-highlight-clear)])

(transient-define-prefix combobulate-avy ()
  "Call avy on a collection of nodes around point"
  ["Avy"
   ("a" "Jump to a nearby node" combobulate-avy-jump)
   ("d" "Jump to a nearby defun" combobulate-avy-jump-defun)])

(transient-define-prefix combobulate-xref ()
  "Find and navigate query matches with Xref"
  ["Xref"
   ("b" "Xref in buffer" combobulate-xref-find-query-buffer-references)])

(transient-define-prefix combobulate-query ()
  "Querying, filtering and searching nodes"
  ["Query Builder"
   ("q" "Open query builder" combobulate-query-builder)
   ("p" "Build match query for node at point" combobulate-query-builder-match-node-at-point)
   ("r" "Build query from root node to point" combobulate-query-builder-root-to-point)])

(transient-define-prefix combobulate-edit ()
  "Edit nodes with multiple cursors..."
  ["Edit with multiple cursors"
   ["DWIM at Point"
    ("c" "Cluster" combobulate-edit-cluster-dwim)
    ("t" "Node Type" combobulate-edit-node-type-dwim)
    ("x" "Node Text" combobulate-edit-node-by-text-dwim)
    ("s" "Sibling" combobulate-edit-node-siblings-dwim)]
   ["Query"
    ("q" "By Query" combobulate-edit-query)]])

(transient-define-prefix combobulate-envelop ()
  "Envelop node on or around point with a template"
  ["Envelopes"
   :setup-children
   (lambda (_)
     ;; This is... not pretty.
     (transient-parse-suffixes
      transient--prefix
      `[,@(mapcar (lambda (envelope)
                    (map-let (:key :description :function) envelope
                      `(,key ,description ,function)))
                  combobulate-manipulation-envelopes)]))])


(transient-define-prefix combobulate ()
  "Structured Editing and Navigation with Combobulate"
  ["Navigation"
   ["Linear / Explicit"
    ("M-a" "Logical prev" combobulate-navigate-logical-previous :transient t)
    ("M-e" "Logical next" combobulate-navigate-logical-next :transient t)
    ("C-M-f" "Forward sexp" forward-sexp :transient t)
    ("C-M-b" "Backward sexp" backward-sexp :transient t)
    ("a" "Avy …" combobulate-avy)]
   ["Hierarchical"
    ("C-M-u" "Up into list" combobulate-navigate-up :transient t)
    ("C-M-d" "Down into list" combobulate-navigate-down :transient t)
    ("C-M-p" "Backward sibling" combobulate-navigate-previous :transient t)
    ("C-M-n" "Forward sibling" combobulate-navigate-next :transient t)]
   [:description (lambda () (concat
                        (propertize "Defun " 'face 'transient-heading)
                        (format "(to: %s)"
                                (propertize
                                 (symbol-name combobulate-beginning-of-defun-behavior)
                                 'face
                                 'font-lock-doc-face))))
                 ("C-M-a" "Beginning of defun" combobulate-navigate-beginning-of-defun :transient t)
                 ("C-M-e" "End of defun" combobulate-navigate-end-of-defun :transient t)]
   ]
  ["Querying"
   [("B" "Query Builder …" combobulate-query)
    ("x" "Xref …" combobulate-xref)
    ("h" "Highlight …" combobulate-highlight)]
   [("M-n" "Next query ring entry" combobulate-query-ring-next-query :transient t)
    ("M-p" "Previous query ring entry" combobulate-query-ring-previous-query :transient t)]]
  ["Editing and Marking"
   ["Marking"
    ("C-M-h" "Mark defun" combobulate-mark-defun :transient t)
    ("M-h" "Expand region" combobulate-mark-node-dwim :transient t)]
   ["Editing"
    ("M-k" "Kill node DWIM" combobulate-kill-node-dwim :transient t)
    ("t" "Edit …" combobulate-edit)
    ("c" "Clone node DWIM" combobulate-clone-node-dwim)
    ("C-M-t" "Transpose sexp" combobulate-transpose-sexps)
    ("e" "Envelop …" combobulate-envelop)
    ("M-P" "Drag node up" combobulate-drag-up :transient t)
    ("M-N" "Drag node down" combobulate-drag-down :transient t)]
   ["Splicing"
    ("M-<up>" "Elevate before and out" combobulate-splice-up :transient t)
    ("M-<down>" "Elevate after and out" combobulate-splice-down :transient t)
    ("M-<left>" "Elevate self and out" combobulate-splice-self :transient t)
    ("M-<right>" "Elevate all and out" combobulate-splice-parent)]])

(provide 'combobulate-ui)
;;; combobulate-ui.el ends here
