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

(defvar combobulate-display-glyphs '((vbar . "│")
                                     (prev-next-sibling . "├")
                                     (hbar . "─")
                                     (next-sibling . "┌")
                                     (prev-sibling . "└")
                                     (spacer . " "))
  "Glyphs to use for the tree display")

(defun combobulate--determine-tree-node-context (node)
  (list (cons :parents (if (combobulate--nav-get-parent node) 1 0))
        (cons :prev-sibling (if (combobulate--nav-get-prev-sibling node) t nil))
        (cons :next-sibling (if (combobulate--nav-get-next-sibling node) t nil))))

(defun combobulate--determine-parent-tree-node-context (node)
  (list
   (cons :prev-sibling (if (combobulate--nav-get-prev-sibling node) t nil))
   (cons :next-sibling (if (combobulate--nav-get-next-sibling node) t nil))))

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

(defun combobulate--debug-nav-setup ()
  (with-current-buffer (get-buffer-create "*nav*")
    (delete-region (point-min) (point-max))))

(defun combobulate--debug-nav-tree-collector (disp)
  (with-current-buffer (get-buffer-create "*nav*")
    (insert (car disp) "\n")))

(defun combobulate--nav-draw-node (node &optional highlighted)
  "Draws a navigation node NODE, and optionally HIGHLIGHTED, with tree guides"
  (let ((disp) (guides)
        (parents (combobulate--nav-get-parents node))
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

(defun combobulate--generate-nav-orientation (node)
  "Creates an abbreviated navigation tree around NODE"
  ;; HACK: This approach is obviously hacky and flawed as we simply
  ;; try to generate a facsimile of a breadth-first tree search with a
  ;; depth stop of "1" for all nodes but the parent nodes (for which
  ;; we get "2" so you can better see where you're going)
  (seq-filter #'tsc-node-p `(,@(reverse (seq-take (combobulate--nav-get-parents node) 2))
                             ,(combobulate--nav-get-prev-sibling node)
                             ,node
                             ,(combobulate--nav-get-child node)
                             ,(combobulate--nav-get-next-sibling node))))

(defun combobulate-render-nav-orientation (node)
  "Renders a navigation tree in orientation mode around NODE"
  (let ((orientation))
    (dolist (nav-element (combobulate--generate-nav-orientation node))
      (push (combobulate--nav-draw-node nav-element (tsc-node-eq node nav-element)) orientation))
    (mapconcat #'car (reverse orientation) "\n")))

(defun combobulate--display-nav-tree (start-node collect-fn)
  "Create a navigation tree starting from START-NODE passing each match to COLLECT-FN

The tree structure follows all the children"
  (when (and start-node (combobulate-navigable-node-p start-node))
    (funcall collect-fn (combobulate--nav-draw-node start-node)))
  (tsc-mapc-children
   (lambda (node)
     (combobulate--display-nav-tree node collect-fn))
   start-node))


(provide 'combobulate-display)
;;; combobulate-display.el ends here
