;;; combobulate-display.el --- display hierarchies of structured code  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>

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

;;; Tree drawing code

(defvar combobulate-display-glyphs '((vbar . "│")
                                     (prev-next-sibling . "├")
                                     (hbar . "─")
                                     (next-sibling . "┌")
                                     (prev-sibling . "└")
                                     (spacer . " "))
  "Glyphs to use for the tree display")

(defun combobulate--determine-tree-node-context (node)
  (list (cons :parents (if (combobulate--nav-get-parent node) 1 0))
        (cons :prev-sibling (not (null (combobulate-tree-get-siblings-of-node combobulate-display-tree node 'backward))))
        (cons :next-sibling (not (null (combobulate-tree-get-siblings-of-node combobulate-display-tree node 'forward))))))

(defun combobulate--determine-parent-tree-node-context (node)
  (list
   (cons :prev-sibling (not (null (combobulate-tree-get-siblings-of-node combobulate-display-tree node 'backward))))
   (cons :next-sibling (not (null (combobulate-tree-get-siblings-of-node combobulate-display-tree node 'forward))))))

(defun combobulate--display-tree-node (ctx)
  (let-alist combobulate-display-glyphs
    (or (pcase ctx
          ;; interstitial nodes
          (`((:prev-sibling . t)
             (:next-sibling . t))
           .vbar)
          (`((:prev-sibling . nil)
             (:next-sibling . t))
           ;; straight bar
           .vbar)
          (`((:prev-sibling . t)
             (:next-sibling . nil))
           .spacer)
          ;; leaf nodes
          ;; ----------
          ;; maybe parents, but no siblings
          (`((:parents . ,parents)
             (:prev-sibling . nil)
             (:next-sibling . nil))
           (if (> parents 0) (concat .prev-sibling .hbar) (concat .spacer .hbar)))
          ;; maybe parents, but one next sibling
          (`((:parents . ,parents)
             (:prev-sibling . nil)
             (:next-sibling . t))
           (if (> parents 0) (concat .prev-next-sibling .hbar) (concat .next-sibling .hbar)))
          ;; maybe parents, but one previous sibling
          (`((:parents . ,_)
             (:prev-sibling . t)
             (:next-sibling . nil))
           (concat .prev-sibling .hbar))
          ;; maybe parents, both siblings
          (`((:parents . ,_)
             (:prev-sibling . t)
             (:next-sibling . t))
           (concat .prev-next-sibling .hbar)))
        ;; fallback
        .spacer)))

(defun combobulate--draw-node (node &optional highlighted)
  "Draws a navigation node NODE, and optionally HIGHLIGHTED, with tree guides"
  (let ((disp) (guides)
        (parents (seq-filter #'combobulate-navigable-node-p (combobulate-get-parents node)))
        (ctx (combobulate--determine-tree-node-context node)))
    (dotimes (offset (length parents))
      (push (concat (combobulate--display-tree-node
                     (combobulate--determine-parent-tree-node-context
                      (nth offset parents)))
                    "  ")
            guides))
    (push (propertize
           (format "%s%s %s"
                   (apply 'concat guides)
                   (combobulate--display-tree-node ctx)
                   (propertize (combobulate-pretty-print-node node) 'face
                               (if highlighted 'combobulate-highlighted-node-face nil))))
          disp)))

;;; Sparse tree code -- used for performance reasons instead of
;;; `combobulate--nav-xxxx'.
(defvar combobulate-display-tree nil
  "Internal storage for the sparse display tree")

(defun combobulate-tree-get-child-nodes (tree)
  "Return children of TREE"
  (mapcar 'car (cdadr tree)))

(defun combobulate-tree-make ()
  "Build a sparse tree around the parent of the navigable node at point"
  (treesit-induce-sparse-tree
   (thread-first (combobulate-node-at-point combobulate-navigation-default-nodes)
                 (combobulate-node-parent))
   #'combobulate-navigable-node-p))

(defun combobulate-tree-get-siblings-of-node (tree node direction)
  "Given a TREE find all children that are parents of NODE in DIRECTION."
  (seq-filter (lambda (tree-node)
                (and (combobulate-node-parent node)
                     (cond ((eq direction 'forward)
                            (combobulate--node-after-node-p tree-node node))
                           ((eq direction 'backward)
                            (combobulate--node-before-node-p tree-node node))
                           (t (error "Unknown direction `%s'" direction)))))
              (combobulate-tree-get-child-nodes tree)))

(defun combobulate-draw-node-locus (node)
  "Creates an abbreviated navigation tree around NODE"
  ;; HACK: This approach is obviously hacky and flawed as we simply
  ;; try to generate a facsimile of a breadth-first tree search with a
  ;; depth stop of "1" for all nodes but the parent nodes (for which
  ;; we get "2" so you can better see where you're going)
  ;;
  ;; Note that this is mostly for performance reasons above all else.
  (seq-filter #'combobulate-node-p
              `(,@(reverse (seq-take (combobulate--nav-get-parents node) 2))
                ,(car (combobulate-tree-get-siblings-of-node combobulate-display-tree node 'backward))
                ,node
                ,(combobulate--nav-get-child node)
                ,(car (combobulate-tree-get-siblings-of-node combobulate-display-tree node 'forward)))))

(defun combobulate-draw-node-tree (node)
  "Renders a navigation tree in node-list mode around NODE"
  (let ((node-list)
        (combobulate-display-tree (combobulate-tree-make)))
    (dolist (nav-element (combobulate-draw-node-locus node))
      (push (combobulate--draw-node nav-element (combobulate-node-eq node nav-element)) node-list))
    (mapconcat #'car (reverse node-list) "\n")))


(provide 'combobulate-display)
;;; combobulate-display.el ends here
