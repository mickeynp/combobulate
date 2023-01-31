;;; test-query.el --- tests for the query system in combobulate  -*- lexical-binding: t; -*-

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

;;; Code:

(load-library "./test-prelude")


(ert-deftest combobulate-test-query-simple-1 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      ;; match invalid root node
      (should (equal (car (combobulate-query-search node '(function_definition))) nil)))))

(ert-deftest combobulate-test-query-simple-2 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (should (combobulate-node-eq (car (combobulate-query-search node '(try_statement))) node)))))

(ert-deftest combobulate-test-query-simple-3 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (should (combobulate-node-eq (car (combobulate-query-search anon-node '("try"))) anon-node)))))

(ert-deftest combobulate-test-query-simple-4 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      ;; no match
      (should (equal (car (combobulate-query-search node '("try"))) nil)))))

(ert-deftest combobulate-test-query-simple-5 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (combobulate-test-compare-structure
       (combobulate-query-search node '(try_statement "try"))
       (list node anon-node)))))

(ert-deftest combobulate-test-query-simple-6 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (should-not (combobulate-node-eq (car (combobulate-query-search node '(try_statement))) anon-node)))))

(ert-deftest combobulate-test-query-simple-7 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (should (equal (combobulate-query-search node '(if_statement)) nil)))))

(ert-deftest combobulate-test-query-simple-8 ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (should (equal (combobulate-query-search node '("if")) nil)))))

(ert-deftest combobulate-test-query-sibling-js ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :setup combobulate-test-js-jsx)
    (combobulate-test-skip-to-match "bar:")
    (let* ((node (combobulate-node-at-point '("object"))))
      (let-alist (append
                  (seq-map-indexed (lambda (el n) (cons (intern (format "c%d" n)) el))
                                   (combobulate-node-children node t)))
        (should (equal (mapcar #'combobulate-node-type (flatten-tree (combobulate-query-search node '(object "{" ((_) ",")+ ))))
                       '("object" "{" "pair" "," "pair" "," "pair" "," "pair" ",")))
        (should (equal (combobulate-query-search node '(object ((pair) ",")+ ))
                       (list node
                             ;; (pair) "," ...
                             (list .c1)
                             .c2
                             (list .c3)
                             .c4
                             (list .c5)
                             .c6
                             (list .c7)
                             .c8)))))))

(ert-deftest combobulate-test-query-sibling-quantifier ()
  (combobulate-test (:language tsx :mode tsx-ts-mode :setup combobulate-test-js-jsx)
    (combobulate-test-skip-to-match "bar:")
    (let* ((node (combobulate-node-at-point '("object"))))
      (let-alist (append
                  (seq-map-indexed (lambda (el n) (cons (intern (format "c%d" n)) el))
                                   (combobulate-node-children node t)))
        ;; non-existent ":"
        (should (equal (combobulate-query-search node '(object ((pair) ":"? ",")+ ))
                       ;; (pair) "," ...
                       (list node (list .c1) .c2 (list .c3) .c4 (list .c5) .c6 (list .c7) .c8)))
        ;; make "," optional
        (should (equal (combobulate-query-search node '(object ((pair) ","? )+))
                       ;; (pair) "," ...
                       (list node (list .c1) .c2 (list .c3) .c4 (list .c5) .c6 (list .c7) .c8)))
        ))))

(ert-deftest combobulate-test-query-sibling ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python-large)
    (let* ((node (combobulate-node-at-point '("try_statement")))
           (block-node (combobulate-node-child node 0)))
      (let-alist (append
                  (seq-map-indexed (lambda (el n) (cons (intern (format "c%d" n)) el))
                                   (combobulate-node-children node))
                  (seq-map-indexed (lambda (el n) (cons (intern (format "bc%d" n)) el))
                                   (combobulate-node-children block-node)))
        ;; sibling except clauses
        (should (equal (combobulate-query-search node '(try_statement ((except_clause) (except_clause))))
                       (list node (list .c1) (list .c2))))

        (should (equal (combobulate-query-search node '(try_statement ((except_clause) (finally_clause))))
                       (list node (list .c1) (list .c3))))

        (should (equal (combobulate-query-search node '(try_statement ((except_clause) (except_clause) (finally_clause))))
                       (list node (list .c1) (list .c2) (list .c3))))

        ;; sibling expression statements nested inside another block
        (should (equal (combobulate-query-search node '(try_statement
                                                        (block ((expression_statement)
                                                                (expression_statement)))))
                       (list node (list block-node
                                        (list .bc0)
                                        (list .bc1)))))
        ;; Test `+' quantifier
        (should (equal (combobulate-query-search node '(try_statement
                                                        (block
                                                            ((expression_statement)
                                                             (expression_statement))+
                                                             )))
                       (list node (list block-node
                                        (list .bc0)
                                        (list .bc1)
                                        (list .bc2)
                                        (list .bc3)))))
        ;; Test `+' quantifier with nested queries
        (should (equal (combobulate-query-search node '(try_statement
                                                        (block
                                                            ((if_statement
                                                              (block))
                                                             (if_statement
                                                              (block)))+
                                                              )))
                       (list node (list block-node
                                        (list .bc6 (list (combobulate-node-child .bc6 1)))
                                        (list .bc7 (list (combobulate-node-child .bc7 1)))))))
        ;; Test `?' quantifier with nested queries
        (should (equal (combobulate-query-search node '(try_statement
                                                        (block
                                                            ((if_statement
                                                              (block))
                                                             (if_statement
                                                              (block)))*
                                                              )))
                       (list node (list block-node
                                        (list .bc6 (list (combobulate-node-child .bc6 1)))
                                        (list .bc7 (list (combobulate-node-child .bc7 1)))))))
        (should (equal (combobulate-query-search node '(try_statement
                                                        (block
                                                            ((if_statement
                                                              (block))
                                                             (if_statement
                                                              (block)))
                                                          ?\ )))
                       (list node (list block-node
                                        (list .bc6 (list (combobulate-node-child .bc6 1)))
                                        (list .bc7 (list (combobulate-node-child .bc7 1)))))))
        (should (equal (combobulate-query-search node '(try_statement
                                                        (else_clause)?\ ))
                       (list node)))
        (should (equal (combobulate-query-search node '(try_statement
                                                        (else_clause)?\ (except_clause)?\ (except_clause)?\ ))
                       (list node (list .c1) (list .c2))))))))


(ert-deftest combobulate-test-query-field ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))

      (combobulate-test-compare-structure
       (combobulate-query-search node '(try_statement "try" ":" body: (block)))
       (list node
             ;; "try"
             (combobulate-node-child node 0 t)
             ;; ":"
             (combobulate-node-child node 1 t)
             ;; (block)
             (combobulate-node-child node 2 t)))
      ;; invalid field
      (should-error
       (combobulate-query-search node '(try_statement "try" ":" wrong: (block))))
      ;; valid field with nested expressions
      (combobulate-test-compare-structure
       (combobulate-query-search node '(try_statement "try" ":" body: (block (expression_statement))))
       (list node
             ;; "try"
             (combobulate-node-child node 0 t)
             ;; ":"
             (combobulate-node-child node 1 t)
             ;; (block (expression_statement))
             (list (combobulate-node-child node 2 t)
                   ;; (... (expression_statement))
                   (combobulate-node-child (combobulate-node-child node 2 t) 0)))))))

(ert-deftest combobulate-test-query-wildcard-match ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement"))))
      ;; match. test anonymous and named node match.
      (combobulate-test-compare-structure
       (mapcar 'cdr (combobulate-query-search
                     node '(try_statement _ @a _ @b _ @c _ @d) t))
       (mapcar 'list (list (combobulate-node-child node 0 t)
                           (combobulate-node-child node 1 t)
                           (combobulate-node-child node 2 t)
                           (combobulate-node-child node 3 t))))

      ;; match. node type named nodes only
      (should (equal (combobulate-query-search
                      node '(try_statement (_) *a (_) *b (_) *c (_) *d) t)
                     '((*a . "block")
                       (*b . "except_clause")
                       (*c . "except_clause")
                       (*d . "finally_clause"))))
      ;; match. anonymous and named alike.
      (should (equal (combobulate-query-search
                      node '(try_statement _ *a _ *b _ *c _ *d) t)
                     '((*a . "try")
                       (*b . ":")
                       (*c . "block")
                       (*d . "except_clause"))))
      ;; match. test only named
      (combobulate-test-compare-structure
       (mapcar 'cdr (combobulate-query-search
                     node '(try_statement (_) @a (_) @b (_) @c (_) @d) t))
       (list (combobulate-node-child node 0)
             (combobulate-node-child node 1)
             (combobulate-node-child node 2)
             (combobulate-node-child node 3))))))

(ert-deftest combobulate-test-query-quantifier-one-or-more ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let* ((node (combobulate-node-at-point '("try_statement")))
           (block-node (combobulate-node-child node 0)))
      ;; match. explicit quantifier followed by named node
      (combobulate-test-compare-structure
       (combobulate-query-search node '(try_statement (block (_)+ (comment))))
       (list node (list block-node
                        (list (combobulate-node-child block-node 0))
                        (list (combobulate-node-child block-node 1))
                        (list (combobulate-node-child block-node 2)))))
      ;; match
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block)+))
       (list node block-node))
      ;; no match. one or more import statements.
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (import_statement)*)))
       (list node (list (combobulate-node-child node 0))))
      ;; match. captures two expression statements next to one another
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (expression_statement)+)))
       (list node (list
                   ;; block
                   (combobulate-node-child node 0)
                   ;; exp. statement #1
                   (combobulate-node-child (combobulate-node-child node 0) 0)
                   ;; ... #2
                   (combobulate-node-child (combobulate-node-child node 0) 1))))
      ;; match. capture everything using greedy one-or-more quantifiers
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (expression_statement)+ (comment)+ (expression_statement)+ (raise_statement)+)))
       (list node (list
                   ;; (block)
                   (combobulate-node-child node 0)
                   ;; (children of block)
                   (combobulate-node-children (combobulate-node-child node 0)))))
      ;; match. invalid zero-or-more node mixed in with one-or-more
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (expression_statement)+
                                    (import_statement)* ;; missing
                                    (comment)+
                                    (expression_statement)+
                                    (import_statement)* ;; missing
                                    (raise_statement)+)))
       (list node (list
                   ;; (block)
                   (combobulate-node-child node 0)
                   ;; (children of block)
                   (combobulate-node-children (combobulate-node-child node 0)))))
      ;; using a wildcard, match everything
      (combobulate-test-compare-structure
       (combobulate-query-search
        block-node '(block (_)+))
       (list
        ;; (block)
        block-node
        ;; (children of block)
        (combobulate-node-children (combobulate-node-child node 0)))))))

(ert-deftest combobulate-test-query-quantifier-zero-or-more ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let* ((node (combobulate-node-at-point '("try_statement")))
           (block-node (combobulate-node-child node 0)))
      ;; match. zero or more `import_statement'.
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (import_statement)*)))
       (list node (list
                   ;; block
                   (combobulate-node-child node 0))))
      ;; match. captures two expression statements next to one another
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (expression_statement)*)))
       (list node (list
                   ;; block
                   (combobulate-node-child node 0)
                   ;; exp. statement #1
                   (combobulate-node-child (combobulate-node-child node 0) 0)
                   ;; ... #2
                   (combobulate-node-child (combobulate-node-child node 0) 1))))
      ;; match. capture everything using greedy zero-or-more quantifiers
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (expression_statement)* (comment)* (expression_statement)* (raise_statement)*)))
       (list node (list
                   ;; (block)
                   (combobulate-node-child node 0)
                   ;; (children of block)
                   (combobulate-node-children (combobulate-node-child node 0)))))
      ;; match. invalid zero-or-more node mixed in with real ones
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (expression_statement)*
                                    (import_statement)* ;; missing
                                    (comment)*
                                    (expression_statement)*
                                    (import_statement)* ;; missing
                                    (raise_statement)*)))
       (list node (list
                   ;; (block)
                   (combobulate-node-child node 0)
                   ;; (children of block)
                   (combobulate-node-children (combobulate-node-child node 0)))))
      ;; match. using a wildcard.
      (combobulate-test-compare-structure
       (combobulate-query-search
        block-node '(block (_) *))
       (list
        ;; (block)
        block-node
        ;; (children of block)
        (combobulate-node-children (combobulate-node-child node 0)))))))

(ert-deftest combobulate-test-query-quantifier-optional ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement"))))
      ;; match, because `_' matches everything
      (combobulate-test-compare-structure (combobulate-query-search
                                           node '(try_statement (_)? ))
                                          (list node (list (combobulate-node-child node 0))))
      ;; match, because `import_statement' is optional and missing
      (combobulate-test-compare-structure (combobulate-query-search
                                           node '(try_statement (import_statement)? ))
                                          (list node))
      ;;  no match because `import_statement' is required and missing
      (combobulate-test-compare-structure (combobulate-query-search
                                           node '(try_statement (import_statement)))
                                          nil)
      ;; match because it is optional and present.
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block)? ))
       (list node (list (combobulate-node-child node 0))))
      ;; match first comment, but fail the second time around.
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (comment)? (comment))))
       nil)
      ;; strict match the first comment, and optional match the
      ;; second time.
      (combobulate-test-compare-structure
       (combobulate-query-search
        node '(try_statement (block (comment) (comment)? )))
       (list node (list
                   ;; (block)
                   (combobulate-node-child node 0)
                   ;; (block (comment))
                   (list (combobulate-node-child
                          (combobulate-node-child node 0) 2))))))))

(ert-deftest combobulate-test-query-capture ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let ((node (combobulate-node-at-point '("try_statement")))
          (anon-node (combobulate-node-at-point)))
      (should (equal (combobulate-query-search
                      node
                      '(try_statement
                        "try" body:
                        (block
                            (expression_statement
                             (assignment left: (identifier) !left-a "=" right: (integer) !right-a))
                          (expression_statement
                           (assignment left: (identifier) !left-b "=" right: (integer) !right-b))
                          (comment)
                          (expression_statement
                           (call function: (identifier)
                                 arguments: (argument_list "(" )))))
                      t)
                     '((!left-a . "a")
                       (!right-a . "1")
                       (!left-b . "b")
                       (!right-b . "2")))))))

(ert-deftest combobulate-test-state-machine-1-cycle ()
  (let ((machine (combobulate-query--iter-state-machine '1 t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '1 t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'match) 'match-stop)))
  (let ((machine (combobulate-query--iter-state-machine '1 t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-children) 'failed-match))))

(ert-deftest combobulate-test-state-machine-+-cycle ()
  (let ((machine (combobulate-query--iter-state-machine '+ t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '+ t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '+ t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '+ t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-children) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '+ t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-children) 'failed-match))))

(ert-deftest combobulate-test-state-machine-*-cycle ()
  (let ((machine (combobulate-query--iter-state-machine '* t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '* t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '* t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine '* t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-children) 'skip)))

  (let ((machine (combobulate-query--iter-state-machine '* t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'match) 'match-continue))
    (should (eq (iter-next machine 'no-children) 'match-stop))))

(ert-deftest combobulate-test-state-machine-?-cycle ()
  (let ((machine (combobulate-query--iter-state-machine ?\  t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine ?\  t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'no-match) 'continue))
    (should (eq (iter-next machine 'match) 'match-stop)))

  (let ((machine (combobulate-query--iter-state-machine ?\  t)))
    (should (eq (iter-next machine) 'start))
    (should (eq (iter-next machine 'no-children) 'skip))))


;;; test-query.el ends here


