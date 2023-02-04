;;; combobulate-display.el --- display hierarchies of structured code  -*- lexical-binding: t; -*-

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
(require 'subr-x)

(defvar combobulate--display-tree nil
  "Internal storage for the sparse display tree")


;;; Tree drawing code

(defvar combobulate-display-glyphs '((vbar . "│")
                                     (prev-next-sibling . "├")
                                     (hbar . "─")
                                     (next-sibling . "┌")
                                     (prev-sibling . "└")
                                     (spacer . " "))
  "Glyphs to use for the tree display")



(defun combobulate--display-tree-node (ctx)
  (let-alist combobulate-display-glyphs
    (or (pcase ctx
          ;; interstitial nodes
          (`((:prev-sibling . t)
             (:next-sibling . t))
           .vbar)
          (`((:prev-sibling . nil)
             (:next-sibling . t))
           .spacer)
          (`((:prev-sibling . t)
             (:next-sibling . nil))
           .spacer)
          (`((:prev-sibling . nil)
             (:next-sibling . nil))
           .spacer)
          ;; leaf nodes
          ;; ----------
          ;; maybe parents, but no siblings
          (`((:parents . ,parents)
             (:prev-sibling . nil)
             (:next-sibling . nil))
           (if (> parents 0)
               (concat .prev-sibling .hbar)
             (concat .spacer .hbar)))
          ;; maybe parents, but one next sibling
          (`((:parents . ,parents)
             (:prev-sibling . nil)
             (:next-sibling . t))
           (if (> parents 0)
               (concat .prev-next-sibling .hbar)
             (concat .next-sibling .hbar)))
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


(defun combobulate-tree-parents (loc)
  (let ((parents))
    (while (setq loc (treepy-up loc))
      ;; parent of root node is root node.. which is a bit odd.
      (when (and (treepy-up loc) (car-safe (treepy-node (treepy-up loc))))
        (when-let (node (car (treepy-node loc)))
          (and ;; (treepy-up loc) (treesit-node-p node)
           (push loc parents)))))
    (reverse parents)))

(defun combobulate--determine-tree-node-context (tree)
  (cons (cons :parents (length (combobulate-tree-parents tree)))
        (combobulate--determine-parent-tree-node-context tree)))

(defun combobulate--determine-parent-tree-node-context (tree)
  (list
   ;; bit of a hacky work-around as `treepy-lefts' and `treepy-rights'
   ;; seem to think the children are the siblings and not the other
   ;; way around?!
   (cons :prev-sibling (not (null (thread-first tree
                                                (treepy-up)
                                                (treepy-lefts)))))
   (cons :next-sibling (not (null (thread-first tree
                                                (treepy-up)
                                                (treepy-rights)))))))

(defun combobulate--draw-node (loc &optional highlighted)
  "Draws a navigation node NODE, and optionally HIGHLIGHTED, with tree guides"
  (let ((guides)
        (parents (combobulate-tree-parents loc))
        (ctx (combobulate--determine-tree-node-context loc)))
    (dotimes (offset (length parents))
      (push (concat (combobulate--display-tree-node
                     (combobulate--determine-parent-tree-node-context
                      (nth offset parents)))
                    (alist-get 'spacer combobulate-display-glyphs))
            guides))
    (let ((node-text (combobulate-pretty-print-node (treepy-node loc))))
      (string-join
       (list
        (propertize (apply 'concat guides) 'face 'combobulate-tree-branch-face)
        (propertize (combobulate--display-tree-node ctx) 'face 'combobulate-tree-branch-face)
        (propertize "" 'face 'combobulate-tree-branch-face)
        (propertize (string-trim node-text) 'face
                    (cond
                     ((and (not (treepy-branch-p loc))
                           (combobulate-node-in-region-p (treepy-node loc)))
                      'highlight)
                     (highlighted 'combobulate-tree-highlighted-node-face)
                     (t 'combobulate-tree-normal-node-face))
                    ))
       ""))))

;;; Sparse tree code -- used for performance reasons instead of
;;; `combobulate-nav-xxxx'.

(defun combobulate-display-make-tree (&optional start-node depth)
  "Build a sparse tree around the parent of the navigable node at point"
  (when-let ((node (or start-node (thread-first (combobulate--get-nearest-navigable-node)
                                                (combobulate-node-parent)
                                                (combobulate-node-parent)
                                                (combobulate-node-parent)))))
    (let ((tree (combobulate-induce-sparse-tree
                 node #'combobulate-navigable-node-p nil (or depth 3))))
      ;; the tree may start with a `nil' node; if so, ditch it, as it
      ;; messes with the tree hierarchy.
      (treepy-list-zip (if (car tree) tree (cadr tree))))))

(defun combobulate-draw-node-locus (node)
  "Creates an abbreviated navigation tree around NODE"
  ;; HACK: This approach is obviously hacky and flawed as we simply
  ;; try to generate a facsimile of a breadth-first tree search with a
  ;; depth stop of "1" for all nodes but the parent nodes (for which
  ;; we get "2" so you can better see where you're going)
  ;;
  ;; Note that this is mostly for performance reasons above all else.
  (let ((prev-siblings (last (combobulate-tree-get-siblings-of-node combobulate--display-tree node 'backward)))
        (next-siblings (car (combobulate-tree-get-siblings-of-node combobulate--display-tree node 'forward)))
        (parents (reverse (seq-take (combobulate-nav-get-parents node) 2)))
        (children (combobulate-nav-get-child node)))
    (seq-filter #'combobulate-node-p
                (combobulate-node-unique
                 (flatten-tree `(,@parents
                                 ,prev-siblings
                                 ,node
                                 ,children
                                 ,next-siblings))))))

(defun combobulate-draw-node-tree (node)
  "Renders a navigation tree in node-list mode around NODE"
  (with-navigation-nodes (:nodes (seq-difference
                                  combobulate-navigation-default-nodes
                                  combobulate-display-ignored-node-types))
    (when (member (combobulate-node-type node) combobulate-display-ignored-node-types)
      (setq node (combobulate--get-nearest-navigable-node)))
    (when node
      (save-excursion
        (combobulate-draw-tree-1 (combobulate-display-create-locus node) node)))))


(defun combobulate-display-create-locus (start-node)
  (let* ((parent (combobulate-nav-get-parent start-node))
         (grand-parent (combobulate-nav-get-parent parent))
         (ztree (treepy-list-zip (list (or grand-parent parent)))))
    (if grand-parent
        (treepy-append-child
         ztree
         (cons (cons parent
                     (mapcar (lambda (n)
                               (cons n (mapcar #'list
                                               (seq-filter #'combobulate-navigable-node-p
                                                           (combobulate-node-children n)))))
                             (seq-uniq
                              (seq-filter #'combobulate-navigable-node-p
                                          (or (combobulate-get-immediate-siblings-of-node start-node)
                                              (list
                                               (combobulate-node-prev-sibling start-node)
                                               start-node
                                               (combobulate-node-next-sibling start-node)))))))
               nil))
      (treepy-append-child
       ztree
       (cons
        (cons nil
              (mapcar (lambda (n)
                        (cons n (mapcar #'list
                                        (seq-filter #'combobulate-navigable-node-p
                                                    (combobulate-node-children n)))))
                      (seq-uniq
                       (seq-filter #'combobulate-navigable-node-p
                                   (combobulate-get-immediate-siblings-of-node start-node)))))
        nil)))))

(defun combobulate-draw-tree-1 (tree &optional highlighted-node)
  (let ((drawing))
    (while (and (setq tree (treepy-next tree))
                (not (treepy-end-p tree)))
      (when (not (treepy-branch-p tree))
        (push (combobulate--draw-node
               tree (and highlighted-node
                         (or (equal (treepy-node tree)
                                    highlighted-node))))
              drawing)))
    (string-join (reverse drawing) "\n")))

(defun combobulate-draw-tree (start-node target-buf &optional depth)
  (let* ((tree (combobulate-display-make-tree start-node depth)))
    (with-current-buffer (get-buffer-create target-buf)
      (erase-buffer)
      (combobulate-draw-tree-1 tree))))

(provide 'combobulate-display)
;;; combobulate-display.el ends here
