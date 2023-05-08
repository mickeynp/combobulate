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

(defsubst combobulate-query-capture (node query &optional beg end node-only)
  (treesit-query-capture node query beg end node-only))

(defsubst combobulate-node-named-p (node)
  (if (combobulate-node-p node)
      (treesit-node-check node 'named)
    (combobulate-proxy-node-named node)))

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

(defsubst combobulate-node-child (node n &optional anonymous)
  (treesit-node-child node n (not anonymous)))

(defsubst combobulate-node-children (node &optional anonymous)
  (treesit-node-children node (not anonymous)))

(defsubst combobulate-node-parent (node)
  (treesit-node-parent node))

(defsubst combobulate-node-next-sibling (node &optional anonymous)
  (treesit-node-next-sibling node (not anonymous)))

(defsubst combobulate-node-prev-sibling (node &optional anonymous)
  (treesit-node-prev-sibling node (not anonymous)))

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

(cl-defstruct combobulate-proxy-node
  "Proxy object for a some properties of a real tree-sitter node.

Only some fields are kept: relationships to other nodes are not
kept."
  start end text type named field node pp)


(defun combobulate-make-proxy (nodes)
  "Factory that creates a facsimile proxy node of NODES."
  (let ((proxies (mapcar
                  (lambda (node)
                    (if (combobulate-node-p node)
                        (make-combobulate-proxy-node
                         :start (set-marker (make-marker) (treesit-node-start node))
                         :end (set-marker (make-marker) (treesit-node-end node))
                         :text (treesit-node-text node)
                         :type (treesit-node-type node)
                         :named (treesit-node-check node 'named)
                         :field (treesit-node-field-name node)
                         :node node
                         :pp (combobulate-pretty-print-node node))
                      node))
                  (if (consp nodes) nodes (list nodes)))))
    (if (consp nodes)
        proxies
      (car-safe proxies))))

(defun combobulate-proxy-to-tree-node (proxy-node)
  "Attempt to find the real tree-sitter node PROXY-NODE points to."
  (when proxy-node
    ;; if we are holding on to a valid treesit node then just pass
    ;; that on provided it's still useful.
    (if t
        ;; todo: disabled until it is possible to detect if a node belongs to a deleted parser
        ;; (or (treesit-node-check (combobulate-proxy-node-node proxy-node) 'outdated)
        ;;     (treesit-node-check (combobulate-proxy-node-node proxy-node) 'missing))
        (save-excursion
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
                                 :keep-types (list (combobulate-node-type proxy-node))))))
      (combobulate-proxy-node-node proxy-node))))


(provide 'combobulate-interface)
;;; combobulate-interface.el ends here
