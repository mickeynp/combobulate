;;; combobulate-procedure.el --- declaratively search for nodes using procedures  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mickey Petersen

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

(require 'combobulate-navigation)
(eval-when-compile (require 'cl-lib))

(defun combobulate-procedure-apply-has-parent (has-parent-rules action-node)
  "Determine if ACTION-NODE has an immediate parent matching HAS-PARENT-RULES."
  (let ((parent-node (combobulate-node-parent action-node)))
    (when (member (combobulate-node-type parent-node)
                  (combobulate-procedure-expand-rules has-parent-rules))
      parent-node)))

(defun combobulate-procedure-apply-has-ancestor (has-ancestor-rules action-node)
  "Determine if ACTION-NODE has an ancestor matching HAS-ANCESTOR-RULES."
  (let ((parents (combobulate-get-parents action-node)))
    (seq-find (lambda (parent)
                (when (member (combobulate-node-type parent)
                              (combobulate-procedure-expand-rules has-ancestor-rules))
                  parent))
              parents)))

(cl-defstruct combobulate-procedure-result
  "The result of a procedure."
  activation-node
  action-node
  parent-node
  selected-nodes
  matched-nodes
  (matched-activation nil)
  (matched-selection nil))

(defun combobulate-procedure-apply-activation-nodes (activation-nodes action-node)
  "Apply ACTIVATION-NODES to NODE."
  (let ((result (make-combobulate-procedure-result
                 :activation-node nil
                 :action-node action-node
                 :parent-node nil
                 :matched-activation nil
                 :selected-nodes nil
                 :matched-selection nil)))
    (catch 'done
      (dolist (activation-node activation-nodes)
        (map-let (:nodes nodes :position position :has-parent has-parent :has-ancestor has-ancestor)
            activation-node
          ;; Expand the rules in `:nodes' and determine if any of them match
          (let ((action-node-type (combobulate-node-type action-node))
                (expanded-nodes (combobulate-procedure-expand-rules nodes))
                (parent-node))
            (when (and
                   ;; action-node-type must be one of the expanded rules
                   (member action-node-type expanded-nodes)
                   ;; Position should be at the start of the node;
                   ;; inside the node, but not at the end nor the
                   ;; beginning; or either.
                   (cond
                    ((or (eq position 'any) (null position))
                     (or (combobulate-point-at-beginning-of-node-p action-node)
                         (combobulate-point-in-node-range-p action-node)))
                    ((eq position 'in)
                     (and (combobulate-point-in-node-range-p action-node)
                          ;; `combobulate-point-in-node-range-p' is non-nil if
                          ;; we're at the start of the node, but that is
                          ;; not allowed here.
                          (not (combobulate-point-at-beginning-of-node-p action-node))))
                    ((eq position 'at)
                     (combobulate-point-at-beginning-of-node-p action-node))
                    (t (error "Unknown `:position' specifier `%s'" pos)))
                   ;; If `:has-parent' or `:has-ancestor' is
                   ;; specified, then check the parents of the
                   ;; action node.
                   (or (and has-parent
                            (setq parent-node (combobulate-procedure-apply-has-parent
                                               has-parent action-node)))
                       (and has-ancestor
                            (setq parent-node (combobulate-procedure-apply-has-ancestor
                                               has-ancestor action-node)))
                       ;; If neither `:has-parent' nor `:has-ancestor' is
                       ;; specified, then default to `t'.
                       (not (or has-parent has-ancestor))))
              (setf (combobulate-procedure-result-activation-node result) activation-node
                    (combobulate-procedure-result-parent-node result) parent-node
                    (combobulate-procedure-result-matched-activation result) t)
              (throw 'done result))))))))

(defun combobulate-procedure-collect-activation-nodes (procedures)
  "Given a list of PROCEDURES, return a list of expanded activation nodes."
  (let ((expanded-nodes))
    (dolist (procedure procedures)
      (map-let (:activation-nodes activation-nodes)
          procedure
        (dolist (activation-node activation-nodes)
          (map-let (:nodes nodes :position position :has-parent has-parent :has-ancestor has-ancestor)
              activation-node
            (setq expanded-nodes (nconc expanded-nodes (combobulate-procedure-expand-rules nodes)))))))
    (seq-uniq expanded-nodes)))

(defun combobulate-procedure-filter-query-results (query-results)
  "Filters QUERY-RESULTS based on their tagged name.

QUERY-RESULTS is a list of cons cells where the car is the tagged
name and the cdr is the node. The tagged name should be
`@match' (or `match'), indicating the node must be kept; or
`@discard' (or `discard'), to indicate the node must be removed."
  (let ((matches))
    (pcase-dolist ((and `(,tagged-name . ,node) result) query-results)
      (cond
       ((or (equal tagged-name 'discard) (equal tagged-name '@discard)) nil)
       (t;; (or (equal tagged-name 'keep) (equal tagged-name '@keep)
        ;;     (equal tagged-name 'match) (equal tagged-name '@match))
        (push result matches))
       (t (error "Unknown tagged name `%s'" tagged-name))))
    (reverse matches)))

(defun combobulate-procedure-start (pt-or-node &optional procedures)
  "Attempt to find a procedure at PT-OR-NODE."
  (catch 'done
    (dolist (node (append
                   ;; start by getting all the nodes *at* point:
                   ;; meaning, nodes beginning at point. They should
                   ;; be our first port of call in terms of matching,
                   ;; as they are the most specific.
                   (save-excursion
                     (goto-char (if (numberp pt-or-node)
                                    pt-or-node
                                  (combobulate-node-start pt-or-node)))
                     (combobulate-all-nodes-at-point))
                   ;; then, get all the nodes point is in.
                   (combobulate-get-parents (combobulate-node-at-point))))
      (when-let (procedure-result
                 (combobulate-procedure-try
                  ;; if we haven't been given any procedures, use the default
                  ;; procedures.
                  (or procedures
                      combobulate-default-procedures
                      (error "No procedures given and no default procedures found"))
                  node))
        (throw 'done procedure-result)))))

(defun combobulate-procedure-try (procedures node)
  "Try to apply PROCEDURES, in order, to NODE.

Return the first matching procedure, or nil if none match."
  (catch 'done
    (let ((matching-procedure))
      (dolist (procedure procedures)
        (setq matching-procedure (combobulate-procedure-apply procedure node))
        (when matching-procedure
          (throw 'done matching-procedure))))))

(defun combobulate-procedure-validate (procedure)
  "Validate PROCEDURE."
  ;; assert that there are no keys beyond these three
  (when-let (unknown-keys (seq-difference (map-keys procedure)
                                          '(:activation-nodes :selector :filter)))
    (error "Unknown key in procedure `%s'. Only `:activation-nodes',
`:selector' or `:filter' are valid" unknown-keys))
  (map-let (:activation-nodes :selector :filter)
      procedure
    (unless (listp activation-nodes)
      (error "Expected `:activation-nodes' to be a list, but got `%s'" :activation-nodes))
    (dolist (activation-node activation-nodes)
      (when (and (plist-get activation-node :has-parent)
                 (plist-get activation-node :has-ancestor))
        (error "Expected `:activation-node' to have either `:has-parent' or `:has-ancestor', but got `%s'" activation-node)))
    (when selector
      (map-let (:match-children :match-siblings :match-query)
          selector
        (let ((matcher))
          (setq matcher (or (plist-get selector :match-children)
                            (plist-get selector :match-siblings)
                            (plist-get selector :match-query)))
          (cond
           ((or match-children match-siblings)
            (unless (or (listp matcher) (equal matcher 't))
              (error "Expected `:selector' to have a list of matchers, but got `%s'" matcher)))
           (match-query
            ;; match-query should be a list (:query QUERY :discard-rules RULES :engine ENGINE)
            (unless (and (listp matcher)
                         (let ((query (plist-get matcher :query))
                               (discard-rules (plist-get matcher :discard-rules)))
                           (and (listp query))))
              (error "Query matchers must have `:query' (and optionally `:discard-rules') but got `%s'" matcher))
            ;; cquery and query should match using `@match' and
            ;; `@discard' and not other markers.
            (when match-query
              (when-let
                  (wrong-mark
                   (seq-find
                    (lambda (query-atom)
                      (and
                       ;; only check symbols!
                       (symbolp query-atom)
                       (let ((name (symbol-name query-atom)))
                         ;; ensure the name, if it starts with
                         ;; `@', is either `@match' or
                         ;; `@discard'.
                         (and (string-prefix-p "@" name)
                              (not (member name '("@match" "@discard")))))))
                    (flatten-tree matcher)))
                (error "`:match-query' should use `@match' and `@discard' to tag nodes, but got `%s'" wrong-mark))))
           (t
            (error "Expected `:selector' to have either `:match-children' or `:match-query', but got `%s'" selector))))))))

(cl-defun combobulate-procedure--mark-nodes (nodes &key match-types discard-types
                                                   (default-mark '@match)
                                                   (keep-anonymous nil)
                                                   (overwrite nil))
  "Mark NODES with MATCH-TYPES and DISCARD-TYPES.

MATCH-TYPES and DISCARD-TYPES are lists of node types to match or
discard, respectively.

If a node type is not in either list, it is marked with
DEFAULT-MARK.

If the NODES are already marked, and OVERWRITE is non-nil, then
overwrite the existing marks."
  (let ((marked-nodes))
    (pcase-dolist ((or `(,existing-mark . ,node) node) nodes)
      (let ((type (combobulate-node-type node)))
        (push
         (cons (if (and existing-mark (not overwrite))
                   existing-mark
                 (cond
                  ((or (member type match-types)
                       (and (combobulate-node-anonymous-p node)
                            keep-anonymous))
                   '@match)
                  ((member type discard-types) '@discard)
                  (existing-mark existing-mark)
                  (t default-mark)))
               node)
         marked-nodes)))
    (reverse marked-nodes)))

(defun combobulate-procedure--filter-marked-nodes (nodes &optional keep-match keep-discard keep-mark)
  "Filter NODES based on their marks.

MATCH-TYPES and DISCARD-TYPES are lists of node types to match or
discard, respectively.

If keep-mark is non-nil, keep the mark in the result."
  (let ((result))
    (pcase-dolist (`(,mark . ,node) nodes)
      (when (or (and (eq mark '@match) keep-match)
                (and (eq mark '@discard) keep-discard))
        (push (if keep-mark (cons mark node) node) result)))
    (nreverse result)))

(defun combobulate-procedure--filter-nodes-by-query (selector node)
  "Filter NODES by the query defined by SELECTOR against NODE."
  (map-let (:engine :query :discard-rules)
      selector
    (let* ((expanded-discard-rules
            (combobulate-procedure-expand-rules discard-rules))
           (nodes (if (eq engine 'combobulate)
                      (combobulate-query-search node query t nil nil)
                    (treesit-query-capture node query))))
      (combobulate-procedure--mark-nodes
       nodes
       :discard-types expanded-discard-rules
       :overwrite t
       :keep-anonymous t))))

(defun combobulate-procedure--filter-nodes-by-relationship (selector node fn)
  "Filter NODES by the relationship defined by FN against NODE.

FN should be a function that takes a node and whether to include
anonymous nodes and return the nodes that match the relationship."
  (map-let (:match-rules :discard-rules :default-mark :anonymous)
      ;; we can have either a plist or `t', indicating that we want
      ;; all matches
      (if (eq selector 't) (list :match-rules 't) selector)
    (let* ((match-rules (combobulate-procedure-expand-rules match-rules))
           (discard-rules
            (combobulate-procedure-expand-rules discard-rules))
           (default-mark
            (or default-mark
                (and match-rules discard-rules
                     (error "Cannot have both `:match-rules' and `:discard-rules'
without explicitly setting `:default-mark'"))
                (if match-rules '@discard '@match)))
           (anonymous anonymous)
           (nodes (funcall fn node anonymous)))
      (combobulate-procedure--mark-nodes
       nodes
       :match-types match-rules
       :discard-types discard-rules
       :keep-anonymous anonymous
       :default-mark default-mark))))

(defun combobulate-procedure-apply (procedure node)
  "Apply PROCEDURE to NODE.

PROCEDURE is a form matching the following pattern:

   (:activation-nodes (ACTIVATION-NODE-RULES ...))
    :selector SELECTOR-RULES)

Where ACTIVATION-NODE-RULES is a list of activation nodes, each
of which is a form matching the following pattern:

   (:nodes RULES
    [:position POSITION-RULE]
    [:has-parent HAS-PARENT-RULE]
    [:has-ancestor HAS-ANCESTOR-RULE])

Where RULES is one or more rules outlined in
`combobulate-procedure-expand-rules', and POSITION-RULE is one
of:

  `any', meaning point is anywhere in the node, and is the default;
  `in', that it is *not* at the beginning;
  and `at', that it must be at the exact beginning of the node.

HAS-PARENT-RULE and HAS-ANCESTOR-RULE are optional, though at
most one can be used in an activation node rule. They each take a
list of RULES.

HAS-PARENT-RULE checks if the immediate parent of the action node
matches the rules, while HAS-ANCESTOR-RULE checks if any ancestor
of the action node matches the rules.

SELECTOR-RULES is a form matching the following pattern:

   (:choose <CHOICE>
    <MATCHER PROPERTY>)

Where <MATCHER PROPERTY> is one of:

    :match-query <QUERY-MATCHER>
    :match-children <NODE-MATCHER>
    :match-siblings <NODE-MATCHER>

And CHOICE is either `node' or `parent' (the default), indicating whether the
selector should operate on the action node or its matching
parent.

NODE-MATCHER must be either `t', indicating all node types (but
not anonymous nodes); or a form of the following pattern:

   (:match-rules <RULES|t>
    :discard-rules <RULES|t>
    [:anonymous <nil|t>]
    [:default-mark <@match|@discard>])

Only one of `:match-rules' or `:discard-rules' can be used in
NODE-MATCHER. `:anonymous' is a boolean and defaults to nil, and
`:default-mark' is a symbol and defaults to
`@match'. `:default-mark' is the tie-breaker when a node is not
matched by `:match-rules' or `:discard-rules'.

QUERY-MATCHER must be of the form:

   (:query <QUERY
    [:engine <combobulate|treesitter>]
    [:discard-rules <RULES>])

Note that QUERY can be either Combobulate's internal query
language to non-recursively match against NODE, or a regular
tree-sitter query that recursively matches against NODE and any
children of NODE. The `:engine' flag determines which, and it
defaults to `combobulate'. `:discard-rules' is a list of rules
(or `t' indicating match everything, but not anonymous nodes)."
  (combobulate-procedure-validate procedure)
  (map-let (:activation-nodes :selector :filter)
      procedure
    (when-let (procedure-result (combobulate-procedure-apply-activation-nodes
                                 activation-nodes node))
      (cl-assert (and (combobulate-procedure-result-p procedure-result)
                      (combobulate-procedure-result-matched-activation procedure-result))
                 nil "Expected `combobulate-procedure-apply-activation-nodes' to return a
 procedure result with a matched activation node")
      (if selector
          (map-let (:choose :match-query :match-children :match-siblings) selector
            ;; use `:choose' to determine whether to operate on the action node or its parent
            (when-let ((chosen-node
                        (cond
                         ((equal choose 'node)
                          (combobulate-procedure-result-action-node procedure-result))
                         ((or (equal choose 'parent) (null choose))
                          (combobulate-procedure-result-parent-node procedure-result))
                         (t (error "Unknown `:choose' specifier `%s'" choose)))))
              (setf
               ;; store the result of the selector filter in the procedure result
               (combobulate-procedure-result-selected-nodes procedure-result)
               (combobulate-procedure--mark-nodes
               (ensure-list
                (cond
                 (match-query
                  (combobulate-procedure--filter-nodes-by-query match-query chosen-node))
                 ((or match-siblings match-children)
                  (combobulate-procedure--filter-nodes-by-relationship
                   (or match-children match-siblings)
                   chosen-node
                   (if match-siblings
                       #'combobulate-linear-siblings
                     #'combobulate-node-children)))
                 (t (error "Invalid selector: %s" selector))))
                :discard-types (combobulate-procedure-expand-rules combobulate-procedure-discard-rules)
                :overwrite t
                :keep-anonymous t)
               ;; acknowledge that the selector filter was used
               (combobulate-procedure-result-matched-selection procedure-result) t)))
        ;; if there is no selector, then the action node is the selected node
        (setf (combobulate-procedure-result-matched-selection procedure-result) 'n/a))
      ;; do a final bit of clean-up: get all the nodes marked `@match'
      ;; and also put them in the `matched-nodes' slot and then remove
      ;; the mark.
      (setf (combobulate-procedure-result-matched-nodes procedure-result)
            (combobulate-procedure--filter-marked-nodes
             (combobulate-procedure-result-selected-nodes procedure-result) t nil))
      procedure-result)))

(defun combobulate-production-rules-exists-p (node-type)
  "Return t if NODE-TYPE exists in `combobulate-navigation-rules'."
  (assoc-string node-type (combobulate-production-rules-get-rules)))

(defun combobulate-production-rules-get (node-type &optional fields)
  "Get production rules for NODE-TYPE and possibly FIELDS."
  (setq fields (or fields '(:all t)))
  (let* ((rules (cadr (assoc-string node-type (combobulate-production-rules-get-rules))))
         (all-keys (map-keys rules)))
    (if rules
        (apply
         #'append
         (mapcar (lambda (prop)
                   (if (plist-member rules prop)
                       (plist-get rules prop)
                     (error "Rule `%s' has no field named `%s'. Known fields: `%s'"
                            node-type prop all-keys)))
                 (cond
                  ((null fields) '(:*unnamed*))
                  ((plist-get fields :all) all-keys)
                  (t fields))))
      (error "Cannot find any rules named `%s'" node-type))))

(defun combobulate-production-rules-get-inverted (rule-name &optional language)
  "Find the inverted production rule named RULE-NAME."
  (cadr (assoc-string rule-name (combobulate-production-rules--get combobulate-rules-inverse-alist language))))


(defun combobulate-production-rules--get (variable &optional language)
  "Get the production rules from VARIABLE."
  (cadr (assoc (or language (combobulate-primary-language)) variable)))

(defun combobulate-production-rules-get-types (&optional language)
  "Return a list of all node types in LANGUAGE."
  (combobulate-production-rules--get combobulate-rules-types-alist language))

(defun combobulate-production-rules-get-rules (&optional language)
  "Return a list of the production rules in LANGUAGE."
  (combobulate-production-rules--get combobulate-rules-alist language))

(defun combobulate-production-rules-get-inverse-rules (&optional language)
  "Return a list of the inverted production rules in LANGUAGE."
  (combobulate-production-rules--get combobulate-rules-inverse-alist language))

(defun combobulate-procedure-expand-rules-1 (rules)
  "Internal function that expands RULES into a set of node types."
  (let ((collected-node-types))
    (dolist (element (ensure-list rules))
      (pcase element
        ;; A production rule with optional fields which defaults to
        ;; nil, because `production-rules-get' interprets that to mean
        ;; all fields.
        ((or `(rule ,rule-name . ,fields)
             (and `(rule ,rule-name) (let fields nil)))
         (setq collected-node-types
               (append collected-node-types
                       (apply #'combobulate-production-rules-get rule-name fields))))
        ;; An inverted production rule that uses the inverse
        ;; production rule table instead.
        (`(irule ,inverted-rule-name)
         (setq collected-node-types
               (append collected-node-types
                       (combobulate-production-rules-get-inverted inverted-rule-name))))
        ;; String nodes are pushed directly onto the list.
        ((and (pred stringp) node-type)
         (push node-type collected-node-types))
        ;; Lists of strings are treated as a list of node types and
        ;; merged with the already-collected node types.
        ((and (pred list-of-strings-p) node-types)
         (setq collected-node-types (append collected-node-types node-types)))
        ;; The value `t' is a special case that, along with `(all)',
        ;; matches all node types
        ((or '(all) 't '(t))
         (setq collected-node-types (append collected-node-types (combobulate-production-rules-get-types))))
        ;; Exclusions are expanded recursively and the result is
        ;; subtracted from the already-collected node types.
        (`(exclude ,inclusions . ,exclusions)
         (setq collected-node-types
               (append collected-node-types
                       (seq-difference (combobulate-procedure-expand-rules inclusions)
                                       (combobulate-procedure-expand-rules exclusions)))))
        (_ (error "Invalid node qualifier: %s" element))))
    (seq-uniq collected-node-types)))

(defun combobulate-procedure-expand-rules (rules)
  "Expand RULES into a set of node types.

RULES is a list of strings or forms matching the following:

A string must be a valid node type for that language.

The value `t' is a special case that matches all node types.

A rule form is a list of the form (rule RULE-NAME [FIELD ...])
with FIELD being a list of fields to draw the node types from,
and RULE-NAME a valid production rule (node type).  If all fields
are desired, the form (rule RULE-NAME) matches all fields.

An inverted rule (that uses the inverted production rules table)
is a list of the form (irule RULE-NAME).

An exclusion is a list of the form (exclude RULES EXCLUSIONS),
where all RULES are matched, and then all EXCLUSIONS are removed
from that set.

The special form (all) matches all node types, including
supertypes and internal types. It is equivalent to manually
typing out all the rules in the production rules table for a
language.

By default, all RULES treated are unioned together, and
duplicates are removed.

If `combobulate-procedure-exclude-rules' is set, it is used to
exclude node types from all expansion results."
  ;; try to ensure that RULES is a list and that if we're given a form
  ;; where the car is a symbol, we wrap it in a list.
  (setq rules (ensure-list rules))
  (when (symbolp (car rules))
    (setq rules (list rules)))
  (combobulate-procedure-expand-rules-1 rules))


(defun combobulate-procedure-test-rules (rules node)
  "Expand RULES into a list of node types and test NODE against them.")

(provide 'combobulate-procedure)
;;; combobulate-procedure.el ends here
