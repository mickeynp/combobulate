;;; combobulate-interface.el --- interface for treesitter  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'cl-lib))
(declare-function combobulate-filter-nodes "combobulate-navigation")
(declare-function combobulate-all-nodes-at-point "combobulate-navigation")
(declare-function combobulate-pretty-print-node "combobulate-navigation")
(declare-function combobulate-node-at-point "combobulate-navigation")
(declare-function combobulate--goto-node "combobulate-navigation")



(defsubst combobulate-node-p (node)
  (treesit-node-p node))

(defsubst combobulate-buffer-root-node (&optional language)
  (treesit-buffer-root-node language))

(defsubst combobulate-node-on (beg end &optional parser-or-lang named)
  (treesit-node-on beg end parser-or-lang named))

(defsubst combobulate-node-at (pos &optional parser-or-lang named)
  (treesit-node-at pos parser-or-lang named))

(defsubst combobulate-induce-sparse-tree (root predicate &optional process-fn limit)
  (treesit-induce-sparse-tree root predicate process-fn limit))

(defsubst combobulate-parser-list (&optional buffer)
  (treesit-parser-list buffer))

(defsubst combobulate-parser-language (parser)
  (treesit-parser-language parser))

(defsubst combobulate-parser-create (language &optional buffer no-reuse)
  (treesit-parser-create language buffer no-reuse))

(defsubst combobulate-parser-delete (language)
  (treesit-parser-delete language))

(defsubst combobulate-parser-node (node)
  (treesit-node-parser node))

(defun combobulate-primary-language ()
  (combobulate-parser-language (or (car (combobulate-parser-list))
                                   (error "No parsers available"))))

(defsubst combobulate-query-validate (language query)
  (treesit-query-validate language query))

(defsubst combobulate-query-capture (node query &optional beg end node-only)
  (treesit-query-capture node query beg end node-only))

(defsubst combobulate-node-named-p (node)
  (if (combobulate-node-p node)
      (treesit-node-check node 'named)
    (combobulate-proxy-node-named node)))

(defsubst combobulate-node-anonymous-p (node)
  (not (combobulate-node-named-p node)))

(defsubst combobulate-node-start (node)
  (if (combobulate-node-p node)
      (treesit-node-start node)
    (combobulate-proxy-node-start node)))

(defsubst combobulate-node-end (node)
  (if (combobulate-node-p node)
      (treesit-node-end node)
    (combobulate-proxy-node-end node)))

(defsubst combobulate-node-field-name (node)
  (if (combobulate-node-p node)
      (treesit-node-field-name node)
    (combobulate-proxy-node-field node)))

(defsubst combobulate-node-range (node)
  (cons (combobulate-node-start node) (combobulate-node-end node)))

(defsubst combobulate-node-text (node &optional with-properties)
  (if (combobulate-node-p node)
      (or (treesit-node-text node (not with-properties)) "")
    (or (and node (combobulate-proxy-node-text node)) "")))

(defsubst combobulate-node-next-sibling (node &optional anonymous)
  (treesit-node-next-sibling node (not anonymous)))

(defsubst combobulate-node-prev-sibling (node &optional anonymous)
  (treesit-node-prev-sibling node (not anonymous)))

(defsubst combobulate-node-child (node n &optional anonymous)
  (treesit-node-child node n (not anonymous)))

(defsubst combobulate-node-children (node &optional anonymous)
  (treesit-node-children node (not anonymous)))

(defsubst combobulate-node-parent (node)
  (treesit-node-parent node))

(defsubst combobulate-parent-while (node pred)
  (treesit-parent-while node pred))

(defsubst combobulate-parent-until (node pred)
  (treesit-parent-until node pred))

(defsubst combobulate-node-child-by-field (node field)
  (treesit-node-child-by-field-name node field))

(defsubst combobulate-node-eq (node1 node2)
  (if (and (combobulate-node-p node1) (combobulate-node-p node2))
      (treesit-node-eq node1 node2)
    (eq node1 node2)))

(defsubst combobulate-root-node ()
  (treesit-buffer-root-node))

(defsubst combobulate-node-descendant-for-range (node beg end &optional all)
  (treesit-node-descendant-for-range node beg end (not all)))

(defsubst combobulate-node-type (node)
  (and node
       (if (combobulate-node-p node)
           (treesit-node-type node)
         (combobulate-proxy-node-type node))))

(defsubst combobulate-filter-child (node pred &optional anonymous)
  (treesit-filter-child node pred (not anonymous)))

(cl-defstruct (combobulate-proxy-node
               (:constructor combobulate-proxy-node-create)
               (:copier nil))
  "Proxy object for a some properties of a real tree-sitter node.

Only some fields are kept: relationships to other nodes are not
kept."
  start end text type named field node pp extra)


(defun combobulate-proxy-node-make-from-nodes (nodes)
  "Factory that creates a facsimile proxy node of NODES."
  (let ((proxies
         (mapcar
          (pcase-lambda ((or (and (pred consp) `(,mark . ,node)) node))
            (let ((tgt-node (if (combobulate-node-p node)
                                (combobulate-proxy-node-create
                                 :start (set-marker (make-marker) (treesit-node-start node))
                                 :end (set-marker (make-marker) (treesit-node-end node))
                                 :text (treesit-node-text node t)
                                 :type (treesit-node-type node)
                                 :named (treesit-node-check node 'named)
                                 :field (treesit-node-field-name node)
                                 :node node
                                 :pp (combobulate-pretty-print-node node)
                                 :extra nil)
                              node)))
              (if mark (cons mark tgt-node) tgt-node)))
          (ensure-list nodes))))
    (if (consp nodes)
        proxies
      (car-safe proxies))))

(defun combobulate-proxy-node-make-from-range (beg end)
  "Factory that creates a facsimile proxy node of the region BEG END."
  (combobulate-proxy-node-create
   :start (set-marker (make-marker) beg)
   :end (set-marker (make-marker) end)
   :text (buffer-substring-no-properties beg end)
   :type "region"
   :named t
   :field nil
   :node nil
   :pp "Region"
   :extra nil))

(defun combobulate-proxy-node-make-point-node (&optional pt)
  "Create a proxy node at `point'."
  (combobulate-proxy-node-create
   :start (or pt (point))
   :end (or pt (point))
   :text ""
   :type "point"
   :named t
   :node nil
   :field nil
   :pp "Point"))

(defun combobulate-proxy-node-to-real-node (proxy-node)
  "Attempt to find the real tree-sitter node PROXY-NODE points to."
  (when proxy-node
    ;; if we are holding on to a valid treesit node then just pass
    ;; that on provided it's still useful.
    (cond
     ;; todo: disabled until it is possible to detect if a node belongs to a deleted parser
     ;; (or (treesit-node-check (combobulate-proxy-node-node proxy-node) 'outdated)
     ;;     (treesit-node-check (combobulate-proxy-node-node proxy-node) 'missing))
     ;; check if proxy-node is even a proxy node
     ((not (combobulate-proxy-node-p proxy-node)) proxy-node)
     (t (save-excursion
          (combobulate--goto-node proxy-node)
          (car-safe (seq-filter (lambda (pt-node)
                                  (and
                                   (equal (cons (marker-position (car (combobulate-node-range proxy-node)))
                                                (marker-position (cdr (combobulate-node-range proxy-node))))
                                          (combobulate-node-range pt-node))
                                   (equal (combobulate-node-type pt-node)
                                          (combobulate-node-type proxy-node))
                                   (or (equal (combobulate-node-field-name pt-node)
                                              (combobulate-node-field-name proxy-node))
                                       t)))
                                (combobulate-filter-nodes
                                 (combobulate-all-nodes-at-point)
                                 :keep-types (list (combobulate-node-type proxy-node))))))))))


(defmacro combobulate-atomic-change-group (&rest body)
  "Re-entrant change group, like `atomic-change-group'.
This means that if BODY exits abnormally,
all of its changes to the current buffer are undone.
This works regardless of whether undo is enabled in the buffer.

This mechanism is transparent to ordinary use of undo;
if undo is enabled in the buffer and BODY succeeds, the
user can undo the change normally."
  (declare (indent 0) (debug t))
  (let ((handle (gensym "--change-group-handle--"))
	(success (gensym "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
	   ;; Don't truncate any undo data in the middle of this.
	   (undo-outer-limit nil)
	   (undo-limit most-positive-fixnum)
	   (undo-strong-limit most-positive-fixnum)
	   (,success nil))
       (unwind-protect
	   (progn
	     ;; This is inside the unwind-protect because
	     ;; it enables undo if that was disabled; we need
	     ;; to make sure that it gets disabled again.
	     (activate-change-group ,handle)
	     (prog1 ,(macroexp-progn body)
	       (setq ,success t)))
	 ;; Either of these functions will disable undo
	 ;; if it was disabled before.
	 (if ,success
	     (accept-change-group ,handle)
	   (cancel-change-group ,handle))))))

(cl-defstruct (combobulate-proffer-action
               (:constructor combobulate-proffer-action-create)
               (:copier nil))
  index
  display-indicator
  current-node
  proxy-nodes
  refactor-id
  prompt-description
  extra-map)

(defmacro lambda-slots (slots &rest body)
  "Construct a macro that expands to a lambda with the given SLOTS and BODY.

The lambda takes a single argument, ACTION, which is an EIEIO object
or `cl-defstruct'.

The requested SLOTS are bound to the action object using `with-slots'."
  (declare (indent defun)
           (debug (&define [&or symbolp (symbolp &optional sexp &rest sexp)]
                           def-body)))
  `(lambda (action)
     (with-slots ,slots action
       ,@body)))


(provide 'combobulate-interface)
;;; combobulate-interface.el ends here
