;;; combobulate-ztree.el --- zipper tree structure   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

;; Author: Daniel Barreto <daniel.barreto.n@gmail.com>
;; Crimped by: Mickey Petersen <mickey@masteringemacs.org>
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

;; NOTE: I pinched this excellent implementation of zipper tree data
;; structure (`treepy.el', https://github.com/volrath/treepy.el) from
;; Daniel Barreto and renamed and tweaked certain things so that it
;; serves Combobulate's needs better.
;;
;;; Original Commentary:
;;
;; Generic tools for recursive and iterative tree traversal based on
;; clojure.walk and clojure.zip respectively.  Depends on `map', a map
;; manipulation library built in Emacs 25.1.

;;; Code:

(require 'map)
(require 'cl-lib)

;;; Walk (recursive tree traversal)

(defun combobulate-ztree-walk (inner outer form)
  "Using INNER and OUTER, traverse FORM, an arbitrary data structure.
INNER and OUTER are functions.  Apply INNER to each element of
FORM, building up a data structure of the same type, then apply
OUTER to the result.  Recognize cons, lists, alists, vectors and
hash tables."
  (cond
   ((and (listp form) (cdr form) (atom (cdr form))) (funcall outer (cons (funcall inner (car form))
                                                                         (funcall inner (cdr form)))))
   ((listp form) (funcall outer (mapcar inner form)))
   ((vectorp form) (funcall outer (apply #'vector (mapcar inner form))))
   ((hash-table-p form) (funcall outer (map-apply (lambda (k v) (funcall inner (cons k v))) form)))
   (t (funcall outer form))))

(defun combobulate-ztree-postwalk (f form)
  "Perform a depth-first, post-order traversal of F applied to FORM.
Call F on each sub-form, use F's return value in place of the
original.  Recognize cons, lists, alists, vectors and
hash tables."
  (combobulate-ztree-walk (apply-partially #'combobulate-ztree-postwalk f) f form))

(defun combobulate-ztree-prewalk (f form)
  "Perform a depth-first, pre-order traversal of F applied to FORM.
Like `combobulate-ztree-postwalk'."
  (combobulate-ztree-walk (apply-partially #'combobulate-ztree-prewalk f) #'identity (funcall f form)))

(defun combobulate-ztree-postwalk-demo (form)
  "Demonstrate the behavior of `combobulate-ztree-postwalk' for FORM.
Return a list of each form as it is walked."
  (let ((walk nil))
    (combobulate-ztree-postwalk (lambda (x) (push x walk) x)
                     form)
    (reverse walk)))

(defun combobulate-ztree-prewalk-demo (form)
  "Demonstrate the behavior of `combobulate-ztree-prewalk' for FORM.
Return a list of each form as it is walked."
  (let ((walk nil))
    (combobulate-ztree-prewalk (lambda (x) (push x walk) x)
                    form)
    (reverse walk)))

(defun combobulate-ztree-postwalk-replace (smap form &optional testfn)
  "Use SMAP to transform FORM by doing replacing operations.
Recursively replace in FORM keys in SMAP with their values.
Does replacement at the leaves of the tree first."
  ;; Also see comment in `map-contains-key's definition.
  (declare (advertised-calling-convention (smap key) "0.1.3"))
  (combobulate-ztree-postwalk (lambda (x)
                     (if (with-suppressed-warnings ((callargs map-contains-key))
                           (map-contains-key smap x testfn))
                         (map-elt smap x)
                       x))
                   form))

(defun combobulate-ztree-prewalk-replace (smap form &optional testfn)
  "Use SMAP to transform FORM by doing replacing operations.
Recursively replace in FORM keys in SMAP with their values.
Does replacement at the root of the tree first."
  ;; Also see comment in `map-contains-key's definition.
  (declare (advertised-calling-convention (smap key) "0.1.3"))
  (combobulate-ztree-prewalk (lambda (x)
                    (if (with-suppressed-warnings ((callargs map-contains-key))
                          (map-contains-key smap x testfn))
                        (map-elt smap x)
                      x))
                  form))


;;; Zipper (iterative tree traversal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combobulate-ztree--context (loc &optional key)
  "Return context for this LOC.
If KEY is given, only return this key's value in context."
  (let ((context (cdr (car loc))))
    (if (and context key)
        (map-elt context key)
      context)))

(defun combobulate-ztree--context-assoc-1 (context k v)
  "Assoc in CONTEXT a key K with a value V."
  (if (map-contains-key context k)
      (mapcar (lambda (entry)
                (if (equal (car entry) k)
                    (cons k v)
                  entry))
              context)
    (cons (cons k v) context)))

(defun combobulate-ztree--context-assoc (context &rest kvs)
  "Immutable map association in CONTEXT using KVS."
  (seq-reduce (lambda (context kv)
                (seq-let [k v] kv
                  (combobulate-ztree--context-assoc-1 context k v)))
              (seq-partition kvs 2) context))

(defun combobulate-ztree--meta (loc &optional key)
  "Return meta information for this LOC.
If KEY is given, only return this key's value in meta
information."
  (let ((meta (cdr loc)))
    (if key
        (map-elt meta key)
      meta)))

(defun combobulate-ztree--with-meta (obj meta)
  "Bind OBJ with some META information."
  (cons obj meta))

(defun combobulate-ztree--join-children (left-children right-children)
  "Return a joining of LEFT-CHILDREN and RIGHT-CHILDREN.
Reverses LEFT-CHILDREN so that they are correctly ordered as in
the tree."
  (append (reverse left-children) right-children))

(defmacro combobulate-ztree--with-loc (loc vars &rest body)
  "Create a lexical context using LOC VARS.
Execute BODY in this context."
  (declare (indent defun))
  (let ((lex-ctx (mapcar (lambda (v)
                           (cl-case v
                             (node    `(node (combobulate-ztree-node ,loc)))
                             (context `(context (combobulate-ztree--context ,loc)))
                             (t       `(,v (combobulate-ztree--context ,loc (quote ,(intern (concat ":" (symbol-name v)))))))))
                         vars)))
    `(let* (,@lex-ctx) ,@body)))

;;;; Construction

(defun combobulate-ztree-zipper (branchp children make-node root)
  "Create a new zipper structure.

BRANCHP is a function that, given a node, returns t if it can
have children, even if it currently doesn't.

CHILDREN is a function that, given a branch node, returns a seq
of its children.

MAKE-NODE is a function that, given an existing node and a seq of
children, returns a new branch node with the supplied children.

ROOT is the root node."
  (combobulate-ztree--with-meta
   (cons root nil)
   `((:branchp . ,branchp) (:children . ,children) (:make-node . ,make-node))))

(defun combobulate-ztree-list-zip (root)
  "Return a zipper for nested lists, given a ROOT list."
  (let ((make-node (lambda (_ children) children)))
    (combobulate-ztree-zipper #'listp #'identity make-node root)))

(defun combobulate-ztree-vector-zip (root)
  "Return a zipper for nested vectors, given a ROOT vector."
  (let ((make-node (lambda (_ children) (apply #'vector children)))
        (children (lambda (cs) (seq-into cs 'list))))
    (combobulate-ztree-zipper #'vectorp children make-node root)))

;;;; Context

(defun combobulate-ztree-node (loc)
  "Return the node at LOC."
  (caar loc))

(defun combobulate-ztree-branch-p (loc)
  "Return t if the node at LOC is a branch."
  (funcall (combobulate-ztree--meta loc ':branchp) (combobulate-ztree-node loc)))

(defun combobulate-ztree-leaf-p (loc)
  "Return t if the node at LOC is a leaf."
  (not (combobulate-ztree-branch-p loc)))

(defun combobulate-ztree-children (loc)
  "Return a children list of the node at LOC, which must be a branch."
  (if (combobulate-ztree-branch-p loc)
      (funcall (combobulate-ztree--meta loc ':children) (combobulate-ztree-node loc))
    (error "Called children on a leaf node")))

(defun combobulate-ztree-make-node (loc node children)
  "Return a new branch node.
Given an existing LOC, NODE and new CHILDREN, creates a new LOC
with them.  The LOC is only used to supply the constructor."
  (funcall (combobulate-ztree--meta loc ':make-node) node children))

(defun combobulate-ztree-path (loc)
  "Return a list of nodes leading to the given LOC."
  (reverse (combobulate-ztree--context loc ':pnodes)))

(defun combobulate-ztree-lefts (loc)
  "Return a list of the left siblings of this LOC."
  (reverse (combobulate-ztree--context loc ':l)))

(defun combobulate-ztree-rights (loc)
  "Return a list of the right siblings of this LOC."
  (combobulate-ztree--context loc ':r))

;;;; Navigation

(defun combobulate-ztree-down (loc)
  "Return the loc of the leftmost child of the node at this LOC.
nil if no children."
  (when (combobulate-ztree-branch-p loc)
    (let ((children (combobulate-ztree-children loc)))
      (combobulate-ztree--with-loc loc (node context pnodes)
        (seq-let [c &rest cs] children
          (when children
            (combobulate-ztree--with-meta
             `(,c . ((:l . ,nil)
                     (:pnodes . ,(if context (cons node pnodes) (list node)))
                     (:ppath . ,context)
                     (:r . ,cs)))
             (combobulate-ztree--meta loc))))))))

(defun combobulate-ztree-up (loc)
  "Return the loc of the parent of the node at this LOC.
nil if at the top."
  (combobulate-ztree--with-loc loc (node pnodes ppath changed? l r)
    (when pnodes
      (let ((pnode (car pnodes)))
        (combobulate-ztree--with-meta
         (if changed?
             (cons (combobulate-ztree-make-node loc pnode (combobulate-ztree--join-children l (cons node r)))
                   (and ppath (combobulate-ztree--context-assoc ppath ':changed? t)))
           (cons pnode ppath))
         (combobulate-ztree--meta loc))))))

(defun combobulate-ztree-root (loc)
  "Zip from LOC all the way up and return the root node.
Reflect any alterations to the tree."
  (if (equal :end (combobulate-ztree--context loc))
      (combobulate-ztree-node loc)
    (let ((p loc))
      (while (setq p (combobulate-ztree-up p))
        (setq loc p))
      (combobulate-ztree-node loc))))

(defun combobulate-ztree-right (loc)
  "Return the loc of the right sibling of the node at this LOC.
nil if there's no more right sibilings."
  (combobulate-ztree--with-loc loc (node context l r)
    (let ((r (if (listp r)
                 r
               ;; If `r' is not a list (or nil), then we're dealing with a non
               ;; nil cdr ending list.
               (cons r nil))))
      (seq-let [cr &rest rnext] r
        (when (and context r)
          (combobulate-ztree--with-meta
           (cons cr
                 (combobulate-ztree--context-assoc context
                                        ':l (cons node l)
                                        ':r rnext))
           (combobulate-ztree--meta loc)))))))


(defun combobulate-ztree-rightmost (loc)
  "Return the loc of the rightmost sibling of the node at this LOC.
If LOC is already the rightmost sibling, return self."
  (combobulate-ztree--with-loc loc (node context l r)
    (if (and context r)
        (combobulate-ztree--with-meta
         (cons (car (last r))
               (combobulate-ztree--context-assoc context
                                      ':l (combobulate-ztree--join-children l (cons node (butlast r)))
                                      ':r nil))
         (combobulate-ztree--meta loc))
      loc)))

(defun combobulate-ztree-left (loc)
  "Return the loc of the left sibling of the node at this LOC.
nil if no more left sibilings."
  (combobulate-ztree--with-loc loc (node context l r)
    (when (and context l)
      (seq-let [cl &rest lnext] l
        (combobulate-ztree--with-meta
         (cons cl
               (combobulate-ztree--context-assoc context
                                      ':l lnext
                                      ':r (cons node r)))
         (combobulate-ztree--meta loc))))))

(defun combobulate-ztree-leftmost (loc)
  "Return the loc of the leftmost sibling of the node at this LOC.
If LOC is already the leftmost sibling, return self."
  (combobulate-ztree--with-loc loc (node context l r)
    (if (and context l)
        (combobulate-ztree--with-meta
         (cons (car (last l))
               (combobulate-ztree--context-assoc context
                                      ':l []
                                      ':r (combobulate-ztree--join-children (butlast l) (cons node r))))
         (combobulate-ztree--meta loc))
      loc)))

(defun combobulate-ztree-leftmost-descendant (loc)
  "Return the leftmost descendant of the given LOC.
\(ie, down repeatedly)."
  (while (combobulate-ztree-branch-p loc)
    (setq loc (combobulate-ztree-down loc)))
  loc)

;;;; Modification

(defun combobulate-ztree-insert-left (loc item)
  "Insert as the left sibling of this LOC'S node the ITEM.
Return same loc with sibilings updated."
  (combobulate-ztree--with-loc loc (node context l)
    (if (not context)
        (error "Insert at top")
      (combobulate-ztree--with-meta
       (cons node
             (combobulate-ztree--context-assoc context
                                    ':l (cons item l)
                                    ':changed? t))
       (combobulate-ztree--meta loc)))))

(defun combobulate-ztree-insert-right (loc item)
  "Insert as the right sibling of this LOC's node the ITEM.
Return same loc with sibilings updated."
  (combobulate-ztree--with-loc loc (node context r)
    (if (not context)
        (error "Insert at top")
      (combobulate-ztree--with-meta
       (cons node
             (combobulate-ztree--context-assoc context
                                    ':r (cons item r)
                                    ':changed? t))
       (combobulate-ztree--meta loc)))))

(defun combobulate-ztree-replace (loc node)
  "Replace the node in this LOC with the given NODE, without moving."
  (let ((context (combobulate-ztree--context loc)))
    (combobulate-ztree--with-meta
     (cons node
           (combobulate-ztree--context-assoc context
                                  ':changed? t))
     (combobulate-ztree--meta loc))))

(defun combobulate-ztree-edit (loc f &rest args)
  "Replace the node at this LOC with the value of (F node ARGS)."
  (combobulate-ztree-replace loc (apply f (combobulate-ztree-node loc) args)))

(defun combobulate-ztree-insert-child (loc item)
  "Insert as the leftmost child of this LOC's node the ITEM.
Return same loc with children updated."
  (combobulate-ztree-replace loc (combobulate-ztree-make-node loc (combobulate-ztree-node loc) (cons item (combobulate-ztree-children loc)))))

(defun combobulate-ztree-append-child (loc item)
  "Insert as the rightmost child of this LOC'S node the ITEM.
Return same loc with children updated."
  (combobulate-ztree-replace loc (combobulate-ztree-make-node loc (combobulate-ztree-node loc) (append (combobulate-ztree-children loc) `(,item)))))  ;; TODO: check performance

(defun combobulate-ztree-remove (loc)
  "Remove the node at LOC.
Return the loc that would have preceded it in a depth-first
walk."
  (combobulate-ztree--with-loc loc (context pnodes ppath l r)
    (if (not context)
        (error "Remove at top")
      (if (> (length l) 0)
          (let ((nloc (combobulate-ztree--with-meta (cons (car l)
                                               (combobulate-ztree--context-assoc context
                                                                      ':l (cdr l)
                                                                      ':changed? t))
                                         (combobulate-ztree--meta loc)))
                (child nil))
            (while (setq child (and (combobulate-ztree-branch-p nloc) (combobulate-ztree-children nloc)))
              (setq nloc (combobulate-ztree-rightmost child)))
            nloc)
        (combobulate-ztree--with-meta
         (cons (combobulate-ztree-make-node loc (car pnodes) r)
               (and ppath (combobulate-ztree--context-assoc context ':changed? t)))
         (combobulate-ztree--meta loc))))))

;;;; Enumeration

(defun combobulate-ztree--preorder-next (loc)
  "Move to the next LOC in the hierarchy, depth-first in preorder.
When reaching the end, returns a distinguished loc detectable via
`combobulate-ztree-end-p'.  If already at the end, stays there."
  (if (equal :end (combobulate-ztree--context loc))
      loc
    (let ((cloc loc))
      (or
       (and (combobulate-ztree-branch-p cloc) (combobulate-ztree-down cloc))
       (combobulate-ztree-right cloc)
       (let ((p cloc)
             (pr nil))
         (while (and (combobulate-ztree-up p) (not (setq pr (combobulate-ztree-right (combobulate-ztree-up p)))))
           (setq p (combobulate-ztree-up p)))
         (or pr (cons (cons (combobulate-ztree-node p) :end) nil)))))))

(defun combobulate-ztree--postorder-next (loc)
  "Move to the next LOC in the hierarchy, depth-first in postorder.
When reaching the end, returns a distinguished loc detectable via
`combobulate-ztree-end-p'.  If already at the end, stays there."
  (if (equal :end (combobulate-ztree--context loc))
      loc
    (if (null (combobulate-ztree-up loc))
        (cons (cons (combobulate-ztree-node loc) :end) nil)
      (or (let ((rloc (combobulate-ztree-right loc)))
            (and rloc (combobulate-ztree-leftmost-descendant rloc)))
          (combobulate-ztree-up loc)))))

(defun combobulate-ztree-next (loc &optional order)
  "Move to the next LOC in the hierarchy, depth-first.
Use ORDER if given.  Possible values for ORDER are `:preorder' and
`:postorder', defaults to the former."
  (cl-case (or order ':preorder)
    (:preorder (combobulate-ztree--preorder-next loc))
    (:postorder (combobulate-ztree--postorder-next loc))
    (t (error "Unrecognized order"))))

(defun combobulate-ztree--preorder-prev (loc)
  "Move to the previous LOC in the hierarchy, depth-first preorder.
If already at the root, returns nil."
  (let ((lloc (combobulate-ztree-left loc))
        (child nil))
    (if lloc
        (progn
          (while (setq child (and (combobulate-ztree-branch-p lloc) (combobulate-ztree-children lloc)))
            (setq lloc (combobulate-ztree-rightmost child)))
          lloc)
      (combobulate-ztree-up loc))))

(defun combobulate-ztree--postorder-prev (loc)
  "Move to the previous LOC in the hierarchy, depth-first postorder.
If already at the root, returns nil."
  (if (combobulate-ztree-branch-p loc)
      (combobulate-ztree-rightmost (combobulate-ztree-down loc))
    (progn
      (while (not (combobulate-ztree-left loc))
        (setq loc (combobulate-ztree-up loc)))
      (combobulate-ztree-left loc))))

(defun combobulate-ztree-prev (loc &optional order)
  "Move to the previous LOC in the hierarchy, depth-first.
Use ORDER if given.  Possible values for ORDER are `:preorder' and `:postorder',
defaults to the former."
  (cl-case (or order ':preorder)
    (:preorder (combobulate-ztree--preorder-prev loc))
    (:postorder (combobulate-ztree--postorder-prev loc))
    (t (error "Unrecognized order"))))

(defun combobulate-ztree-end-p (loc)
  "Return t if LOC represents the end of a depth-first walk."
  (equal :end (combobulate-ztree--context loc)))

(provide 'combobulate-ztree)
;;; combobulate-ztree.el ends here
