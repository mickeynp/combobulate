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

;; This module provides a way to declaratively search for nodes using
;; procedures. A procedure is a list of rules that are applied to a
;; node to determine if it matches. The rules are defined using a
;; simple DSL.

;;; See `combobulate-procedure-apply' for an explanation of how the
;;; DSL works.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'map)
(require 'combobulate-rules)
(require 'combobulate-navigation)

(defun combobulate-procedure-apply-has-parent (has-parent-rules action-node)
  "Determine if ACTION-NODE has an immediate parent matching HAS-PARENT-RULES."
  (let ((parent-node (combobulate-node-parent action-node)))
    (when (member (combobulate-node-type parent-node)
                  (combobulate-procedure-expand-rules has-parent-rules))
      parent-node)))

(defun combobulate-procedure-apply-has-field (has-fields action-node)
  "Determine if ACTION-NODE belongs to a field in HAS-FIELDS."
  (let ((field-name (combobulate-node-field-name action-node)))
    (member field-name (ensure-list has-fields))))

(defun combobulate-procedure-apply-has-ancestor (has-ancestor-rules action-node)
  "Determine if ACTION-NODE has an ancestor matching HAS-ANCESTOR-RULES."
  (let ((parents (combobulate-get-parents action-node)))
    (seq-find (lambda (parent)
                (when (member (combobulate-node-type parent)
                              (combobulate-procedure-expand-rules has-ancestor-rules))
                  parent))
              parents)))

(cl-defstruct (combobulate-procedure-result
               (:constructor combobulate-procedure-result-create)
               (:copier nil))
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
  (let ((result (combobulate-procedure-result-create
                 :activation-node nil
                 :action-node action-node
                 :parent-node nil
                 :matched-activation nil
                 :selected-nodes nil
                 :matched-selection nil)))
    (catch 'done
      (dolist (activation-node activation-nodes)
        (map-let (:nodes :position :has-parent :has-ancestor :has-fields)
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
                    (t (error "Unknown `:position' specifier `%s'" position)))
                   (or (and has-fields (combobulate-procedure-apply-has-field has-fields action-node))
                       (not has-fields))
                   ;; If `:has-parent' or `:has-ancestor' is
                   ;; specified, then check the parents of the
                   ;; action node.
                   (or (and has-parent
                            (setq parent-node (combobulate-procedure-apply-has-parent
                                               has-parent action-node)))
                       (and has-ancestor
                            (setq parent-node (combobulate-procedure-apply-has-ancestor
                                               has-ancestor action-node)))
                       ;; If none of the :has-* conditions are specified,
                       ;; then we can just continue.
                       (not (or has-parent has-ancestor))))
              (setf (combobulate-procedure-result-activation-node result) activation-node
                    (combobulate-procedure-result-parent-node result) parent-node
                    (combobulate-procedure-result-matched-activation result) t)
              (throw 'done result))))))))

(defun combobulate-procedure-collect-activation-nodes (procedures)
  "Given a list of PROCEDURES, return a list of expanded activation nodes."
  (let ((expanded-nodes))
    (dolist (procedure procedures)
      (map-let (:activation-nodes) procedure
        (dolist (activation-node activation-nodes)
          (map-let (:nodes)
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
    (pcase-dolist ((and `(,tagged-name . ,_) result) query-results)
      (cond
       ((or (equal tagged-name 'discard) (equal tagged-name '@discard)) nil)
       (t;; (or (equal tagged-name 'keep) (equal tagged-name '@keep)
        ;;     (equal tagged-name 'match) (equal tagged-name '@match))
        (push result matches))))
    (reverse matches)))

(defun combobulate-procedure-start-matches (pt-or-node &optional procedures)
  "Return nodes that match a procedure at PT-OR-NODE.

If PROCEDURES is not given, use `combobulate-default-procedures'
instead.

Unlike `combobulate-procedure-start', this function returns the
the struct entry `matched-nodes' of the matching procedure, if it
exists, and nil if not."
  (when-let (procedure (car-safe (combobulate-procedure-start pt-or-node procedures)))
    (cl-assert (combobulate-procedure-result-p procedure) t
               "Expected a `combobulate-procedure-result' object, but got `%s'"
               procedure)
    (combobulate-procedure-result-matched-nodes procedure)))

(defun combobulate-procedure-start (pt-or-node &optional procedures exhaustive)
  "Attempt to find a procedure at PT-OR-NODE.

If PROCEDURES is not given, use `combobulate-default-procedures'.
If EXHAUSTIVE is non-nil, then collect all possible procedures
that may apply."
  (let ((matches)
        (nodes
         (seq-uniq
          (append
           ;; start by getting all the nodes *at* point:
           ;; meaning, nodes beginning at point. They should
           ;; be our first port of call in terms of matching,
           ;; as they are the most specific.
           (save-excursion
             (goto-char (if (numberp pt-or-node)
                            pt-or-node
                          (combobulate-node-start pt-or-node)))
             (cons (if (combobulate-node-p pt-or-node)
                       pt-or-node
                     (combobulate-node-at-point))
                   (combobulate-all-nodes-at-point)))
           ;; then, get all the nodes point is in.
           (combobulate-get-parents (combobulate-node-at-point))))))
    (catch 'done
      (dolist (procedure
               (or procedures
                   ;; if we haven't been given any procedures, use the default
                   ;; procedures.
                   combobulate-default-procedures
                   (error "No procedures given and no default procedures found")))
        (dolist (node nodes)
          (when-let (procedure-result
                     (combobulate-procedure-try
                      procedure
                      node))
            (push procedure-result matches)
            (unless exhaustive (throw 'done procedure-result))))))
    (when combobulate-debug (message "Matches: %s" matches))
    matches))

(defun combobulate-procedure-try (procedure node)
  "Apply PROCEDURE to NODE and return the result, if any."
  (let ((procedure-result))
    (setq procedure-result (combobulate-procedure-apply procedure node))
    (pcase procedure-result
      ('nil nil)
      ((cl-struct combobulate-procedure-result
                  matched-selection
                  selected-nodes)
       ;; We define a successful result as one of the following:
       ;;
       ;; Either the matched selection is `n/a', indicating that there
       ;; were no `:selector' properties to further refine the
       ;; activated nodes.
       ;;
       ;; Or the matched selection is `t' *and* that there is at least
       ;; one selected node with a `@match' or `match' tag.
       (when (or (eq matched-selection 'n/a)
                 (and (eq matched-selection t)
                      (seq-find (pcase-lambda (`(,mark . ,_))
                                  (or (eq mark '@match) (eq mark 'match)))
                                selected-nodes)))
         procedure-result))
      (_ (error "Unknown procedure result `%s'" procedure-result)))))

(defun combobulate-procedure-validate (procedure)
  "Validate PROCEDURE."
  ;; assert that there are no keys beyond these three
  (when-let (unknown-keys (seq-difference (map-keys procedure)
                                          '(:activation-nodes :selector)))
    (error "Unknown key in procedure `%s'. Only `:activation-nodes',
`:selector' or `:filter' are valid" unknown-keys))
  (map-let (:activation-nodes :selector)
      procedure
    (unless (listp activation-nodes)
      (error "Expected `:activation-nodes' to be a list, but got `%s'" :activation-nodes))
    (dolist (activation-node activation-nodes)
      (when (and (plist-get activation-node :has-parent)
                 (plist-get activation-node :has-ancestor)
                 (plist-get activation-node :has-fields))
        (error "`:activation-node' allows only: `:has-parent', `:has-ancestor', or `:has-fields' but got `%s'" activation-node)))
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
                               ;; (discard-rules (plist-get matcher :discard-rules))
                               )
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

(defvar combobulate-procedure-include-anonymous-nodes nil
  "Whether to include anonymous nodes in the result of a procedure.")

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
           (anonymous (or anonymous combobulate-procedure-include-anonymous-nodes))
           (nodes (funcall fn node anonymous)))
      (combobulate-procedure--mark-nodes
       nodes
       :match-types match-rules
       :discard-types discard-rules
       :keep-anonymous anonymous
       :default-mark default-mark))))

(defun combobulate-procedure-debug-print-result (procedure-result)
  "Print PROCEDURE-RESULT in a human-readable format."
  (pcase procedure-result
    ((cl-struct combobulate-procedure-result
                (activation-node activation-node)
                (action-node action-node)
                (parent-node parent-node)
                (selected-nodes selected-nodes)
                (matched-nodes matched-nodes)
                (matched-activation matched-activation)
                (matched-selection matched-selection))
     (message "Activation node: %s" activation-node)
     (message "Action node: %s" action-node)
     (message "Parent node: %s" parent-node)
     (message "Selected nodes: %s" selected-nodes)
     (message "Matched nodes: %s" matched-nodes)
     (message "Matched activation: %s" matched-activation)
     (message "Matched selection: %s" matched-selection))))

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
    [:has-field FIELDS]
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

HAS-FIELD-RULE is a list of fields the action node be considered
to be inside of.

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
  (map-let (:activation-nodes :selector)
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
         (supertypes (combobulate-production-rules-get-supertypes))
         (all-keys (map-keys rules)))
    (if rules
        ;; some node types are supertypes, meaning they exist outside
        ;; the actual parse tree TS can generate. However, the rules
        ;; for these supertypes are defined and used in the parser and
        ;; exported in the node rules also. If we encounter a
        ;; supertype, we expand it inline.
        (mapcan
         (lambda (n)
           ;; check if n is a supertype; if it is, expand it.
           (if (member n supertypes)
               (combobulate-production-rules-get n)
             (list n)))
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
                   (t fields)))))
      (when combobulate-debug
        (error "Cannot find any rules named `%s'" node-type)))))

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

(defun combobulate-production-rules-get-supertypes (&optional language)
  "Return a list of the supertypes in LANGUAGE."
  (combobulate-production-rules--get combobulate-rules-supertypes-alist language))

(defun combobulate-production-rules-get-inverse-rules (&optional language)
  "Return a list of the inverted production rules in LANGUAGE."
  (combobulate-production-rules--get combobulate-rules-inverse-alist language))

(defun combobulate-procedure-expand-rules-1 (rules)
  "Internal function that expands RULES into a set of node types."
  (let ((collected-node-types))
    (dolist (element (ensure-list rules))
      (pcase element
        ;; Match a regular expression against the node types and
        ;; expand each matching production rule with `(rule TYPE)'
        (`(rule-rx ,regexp)
         (setq collected-node-types
               (append collected-node-types
                       (mapcan (lambda (n)
                                 (combobulate-procedure-expand-rules
                                  (list 'rule n)))
                               (combobulate-procedure-expand-rules
                                (list 'rx regexp))))))
        ;; Match a regular expression against the node types and
        ;; collect the matching node types.
        (`(rx ,regexp)
         (setq collected-node-types
               (append collected-node-types
                       (seq-filter (lambda (node-type)
                                     (string-match-p regexp node-type))
                                   (combobulate-production-rules-get-types)))))
        ;; A production rule with optional fields which defaults to
        ;; nil, because `production-rules-get' interprets that to mean
        ;; all fields.
        ((or `(rule ,rule-name . ,fields)
             (and `(rule ,rule-name) (let fields nil)))
         (setq collected-node-types
               (append collected-node-types
                       (apply #'combobulate-production-rules-get rule-name (list (ensure-list fields))))))
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

(provide 'combobulate-procedure)
;;; combobulate-procedure.el ends here
