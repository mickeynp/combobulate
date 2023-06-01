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
                                   (combobulate-node-child 0)
                                   (combobulate-node-text)))
    (_ default-name)))

(defun combobulate-css-setup (_)
  ;; NOTE This is subject to change
  (setq combobulate-manipulation-envelopes
        '((:description "Envelop in a media query"
                        :key "@"
                        :mark-node t
                        :nodes ("rule_set")
                        :name "media-query"
                        :template ("@media (max-width: " (p 768px "max-width") @ ") {" >
                                   >
                                   n r> >
                                   n > "}" >))))

  (setq combobulate-pretty-print-node-name-function
        #'combobulate-css-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)

  (setq combobulate-manipulation-edit-procedures
        '((:activation-nodes
           ((:node "declaration" :position in :find-parent "block"))
           :match-query (block (declaration ":" (_) @match)+))
          (:activation-nodes
           ((:node "block" :position at-or-in))
           :match-query ((block) (_)+ @match))))

  (setq combobulate-navigation-sexp-nodes
        (append '("comment" "property_name")
                (combobulate-production-rules-get "selectors")
                (combobulate-production-rules-get "arguments")))
  (setq combobulate-manipulation-splicing-procedures nil)
  (setq combobulate-navigation-parent-child-nodes '("rule_set" "media_statement" "stylesheet"))
  (setq combobulate-navigation-drag-parent-nodes
        '("block" "arguments" "stylesheet" "declaration"))
  (setq combobulate-navigation-sibling-procedures
        `((:activation-nodes
           ((:node
             ,(append
               (combobulate-production-rules-get "feature_query")
               (combobulate-production-rules-get "arguments"))
             :position at-or-in
             :find-immediate-parent ("feature_query" "arguments")))
           :remove-types ("comment")
           :match-children t)
          (:activation-nodes
           ((:node
             ,(append
               (combobulate-production-rules-get "block")
               (combobulate-production-rules-get "stylesheet"))
             :position at-or-in
             :find-immediate-parent ("stylesheet" "block")))
           :remove-types ("comment")
           :match-children t)))
  (setq combobulate-navigation-defun-nodes
        (seq-difference (combobulate-production-rules-get "stylesheet")
                        ;; this is too granular for defun
                        '("declaration")))
  (setq combobulate-navigation-default-nodes
        (append
         (combobulate-production-rules-get "selectors")
         '("stylesheet" "rule_set" "feature_query" "float_value"
           "declaration" "property_name" "string_value" "plain_value" "integer_value"
           "color_value" "call_expression" "function_name" "media_statement" "arguments"))))


(provide 'combobulate-css)
;;; combobulate-css.el ends here
