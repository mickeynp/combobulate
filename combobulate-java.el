;;; combobulate-javascript.el --- structured editing for JS+JSX and Typescript in combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Matija Obid <matija.obid@posteo.net>
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
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(require 'combobulate-envelope)
(require 'combobulate-query)
(require 'combobulate-display)

(defun combobulate-java--get-function-name (node)
  "Extract function nane from NODE."
  (setq method node)
  (concat "Method "
	  (car (combobulate-query-node-text
		'((method_declaration) (_) (_) (_) @name)
		node t))))

(defun combobulate-java-pretty-print (node default-name)
  "Pretty printer for JS and JSX NODEs.

If node is not method, return DEFAULT-NAME"
  (pcase (combobulate-node-type node)
    ("method_declaration" (combobulate-java--get-function-name node))
    (_ default-name)))

(defvar combobulate-java-definitions
  '((context-nodes)

    (envelope-procedure-shorthand-alist
     '((expressions
	. ((:activation-nodes
	    ((:nodes ((rule "primary_expression") (rule "expression")))))))
       (wrap-expressions
	. ((:activation-nodes
	    ((:nodes ((rule "primary_expression") (rule "expression")))))))
       (general-statement
	. ((:activation-nodes ((:nodes (rule "statement") :has-parent (irule "statement"))))))))

    (envelope-list
     `((:description
	"((..) (...))"
	:key "c"
	:name "cast"
	:mark-node t
	:shorthand wrap-expressions
	:template ("((" (p type "Type") ") (" r "))"))
       (:description
	"if (...) { ... }"
	:key "i"
	:name "if"
	:mark-node t
	:shorthand general-statement
	:template ("if (" (p cond "Condition") ") {" @
		   n> r> n>
		   "}"))
       (:description
	"try { ... } catch (...) { ... }"
	:key "t"
	:name "try"
	:mark-node t
	:shorthand general-statement
	:template ("try {"
		   n> r> n>
		   "} catch (" (p RuntimeException "Exception") " ex) {"
		   @ n>
		   n> "}"))
       (:description
	"try { ... } catch (...) { ... }"
	:key "f"
	:name "finally"
	:mark-node t
	:shorthand general-statement
	:template ("try {"
		   n> r> n>
		   "} finally {"
		   @ n>
		   n> "}"))))

    (pretty-print-node-name-function #'combobulate-java-pretty-print)
    ;; (highlight-queries-default)
    (procedures-edit)

    (procedures-sexp
     '((:activation-nodes ((:nodes ("type_arguments"
				    "generic_type"
				    "object_creation_expression"
				    (rule "statement")
				    "method_invocation"
				    "lambda_expression"
				    "method_declaration"
				    ))))))

    (procedures-defun
     '((:activation-nodes ((:nodes ("lambda_expression" "method_declaration" "class_declaration"))))))

    (procedures-sibling
     `((:activation-nodes
	;; Arrays, method call arguments, ...
	((:nodes
	  ((rule "expression")
	   ("argument_list" ",")) ;; ??
	  :has-parent ("arguments"
		       "argument_list"
		       "array_initializer")))
	:selector (:match-children t))

       (:activation-nodes
	;; Block expressions:
	((:nodes ((rule "statement"))
		 :has-parent ("block")))
	:selector (:match-children (:discard-rules ("line_comment" "block_comment"))))

       (:activation-nodes
	;; Jump through the comments.
	((:nodes ("line_comment"
		  "block_comment")
		 :has-parent ("block")))
	:selector (:match-children (:match-rules ("line_comment" "block_comment"))))

       (:activation-nodes
	;; Class first level statements:
	((:nodes ("class_declaration"
		  "field_declaration"
		  "method_declaration"
		  "constant_declaration"
		  "annotation")
		 :has-parent ("class_body"
			      "interface_body"
			      "modifiers")))
	:selector (:match-children (:discard-rules ("line_comment" "block_comment"))))))

    (display-ignored-node-types)
    (procedures-hierarchy
     '(( :activation-nodes ((:nodes "class_declaration" :position at))
         :selector (:choose node
                            :match-query
                            (:query (class_declaration (class_body (method_declaration @match))) :engine combobulate)))
       ( :activation-nodes ((:nodes ("marker_annotation" "annotation") :position at))
         :selector (:choose node
                            :match-siblings
                            (:match-rules ("marker_annotation" "annotation"))))
       ( :activation-nodes ((:nodes ("lambda_expression") :position in))
         :selector
         (:choose node
                  :match-query
                  (:query (lambda_expression (block (_ @match))) :engine combobulate)))
       ( :activation-nodes ((:nodes ("method_declaration")
                                    :position in))
         :selector (:choose node
                            :match-query
                            (:query (method_declaration (block (_ @match))) :engine combobulate)))
       ( :activation-nodes ((:nodes ((all)) :has-parent ((all))))
         :selector (:choose node
                            :match-children (:discard-rules ("block" "parenthesized_expression"))))
       ))
    (procedures-logical '((:activation-nodes ((:nodes (all))))))))


(defconst combobulate-java-extra-defcustoms '())

(define-combobulate-language
 :name java
 :language java
 :major-modes (java-mode java-ts-mode)
 :custom combobulate-java-definitions
 :extra-defcustoms combobulate-java-extra-defcustoms
 :setup-fn combobulate-java-setup)

(defun combobulate-java-setup (_)
  "Do nothing."
  nil)

(provide 'combobulate-java)

;;; combobulate-java.el ends here
