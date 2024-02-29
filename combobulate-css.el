;;; combobulate-css.el --- css mode support for combobulate  -*- lexical-binding: t; -*-

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

;; For `css-ts-mode'

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-rules)
(require 'subr-x)

(defun combobulate-css-pretty-print-node-name (node default-name)
  "Pretty printer for JS and JSX nodes"
  (pcase (combobulate-node-type node)
    ("declaration" (thread-first node
                                 (combobulate-node-child 0)
                                 (combobulate-node-text)))
    ("rule_set" (thread-first node
                              (combobulate-node-child 0)
                              (combobulate-node-text)))
    ("property_name" (thread-first node
                                   (combobulate-node-parent)
                                   (combobulate-node-child 1)
                                   (combobulate-node-text)))
    (_ default-name)))

(defun combobulate-css-setup (_)
  ;; NOTE This is subject to change
  (setq combobulate-envelope-procedure-shorthand-alist
        '((rhs . ((:activation-nodes ((:nodes ((rule "arguments")))))))))
  (setq combobulate-manipulation-envelopes
        '((:description
           "( ... )"
           :key "("
           :extra-key "M-("
           :mark-node t
           :shorthand rhs
           :name "wrap-parentheses"
           :template (> @ "(" r ")"))
          (:description
           "Envelop in a media query"
           :key "@m"
           :mark-node t
           :nodes ("rule_set")
           :name "media-query"
           :template ("@media "
                      (choice* :rest ("(min-width: " (p 768px "min-width") @ ") {"))
                      (choice* :rest ("(max-width: " (p 768px "max-width") @ ") {")) >
                      (choice* :rest ("(max-height: " (p 1024px "max-height") @ ") {"))
                      (choice* :rest ("(min-height: " (p 1024px "min-height") @ ") {"))
                      (choice* :rest ("(orientation: " (choice "portrait") (choice "landscape") @ ") {"))
                      (choice* :rest ("(aspect-ratio: " (choice "16/9") (choice "4/3") @ ") {"))
                      n> r> >
                      n > "}" >))))

  (setq combobulate-navigation-context-nodes
        '("class_name" "property_name" "feature_name" "float_name" "integer_value"
          "id_name" "plain_value" "color_value" "string_value"))
  (setq combobulate-pretty-print-node-name-function
        #'combobulate-css-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)

  (setq combobulate-manipulation-edit-procedures
        '((:activation-nodes
           ((:nodes ("declaration") :position in :has-ancestor "block"))
           :selector (:match-query (:query (block (declaration ":" (_) @match)+)
                                           :engine combobulate)))
          (:activation-nodes
           ((:nodes ("block")))
           :selector (:match-query ((block) (_)+ @match)))))

  (setq combobulate-navigation-sexp-procedures
        '((:activation-nodes ((:nodes ("comment" "property_name"
                                       (rule "selectors")
                                       (rule "arguments")))))))
  (setq combobulate-navigation-parent-child-procedures
        '((:activation-nodes ((:nodes ("block") :position at)) :selector (:choose node :match-children (:match-rules (rule "block"))))
          (:activation-nodes ((:nodes ("media_statement") :position at)) :selector (:choose node :match-children (:match-rules "block")))
          (:activation-nodes ((:nodes (all))) :selector (:choose node :match-children t))))

  (setq combobulate-navigation-sibling-procedures
        '(;; declarations' own property values should be siblings, but
          ;; not property_name as it's a child of declaration also,
          ;; and that'd mean the LHS and RHS are siblings of another,
          ;; which would be weird.
          (:activation-nodes
           ((:nodes
             ((rule "feature_query")
              (rule "arguments"))
             :has-parent ("feature_query" "arguments")))
           :selector (:choose parent
                              :match-children t))
          (:activation-nodes
           ((:nodes
             ((exclude (rule "declaration") "property_name"))
             :has-parent ("declaration")))
           :selector (:choose parent
                              :match-children (:discard-rules ("comment" "property_name"))))
          (:activation-nodes
           ((:nodes
             ((rule "block")
              (rule "stylesheet"))
             :has-parent ("stylesheet" "block")))
           :selector (:match-children (:discard-rules ("comment"))))
          ;; declarations are siblings in a block
          (:activation-nodes
           ((:nodes
             ("declaration")
             :has-parent ("block")))
           :selector (:match-children (:discard-rules ("comment" "property_name"))))))
  (setq combobulate-navigation-defun-procedures
        '((:activation-nodes ((:nodes (exclude (all) "declaration")))))))


(provide 'combobulate-css)
;;; combobulate-css.el ends here
