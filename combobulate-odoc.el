;;; combobulate-odoc.el --- odoc mode support for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Tim McGilchrist

;; Author: Tim McGilchrist
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

;; Combobulate support for odoc (OCaml documentation) files using
;; tree-sitter.  Odoc is a markup language used in OCaml documentation
;; comments and .mld files.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-odoc-pretty-print-node-name (node default-name)
  "Pretty print the name of NODE."
  (combobulate-string-truncate
   (pcase (combobulate-node-type node)
     ("heading"
      (let ((marker (combobulate-node-child node 0)))
        (if marker
            (concat (combobulate-node-text marker) " ...")
          default-name)))
     ("bold" "{b ...}")
     ("italic" "{i ...}")
     ("emphasis" "{e ...}")
     ("code_span" (concat "[" (combobulate-node-text node) "]"))
     ("code_block" "{[...]}")
     ("code_block_with_lang"
      (let ((lang (combobulate-node-child node 0)))
        (if lang
            (concat "{@" (combobulate-node-text lang) "[...]}")
          "{@[...]}")))
     ("verbatim_block" "{v...v}")
     ("math_block" "{math ...}")
     ("math_span" "{m ...}")
     ("unordered_list" "{ul ...}")
     ("ordered_list" "{ol ...}")
     ("table_heavy" "{table ...}")
     ("table_light" "{t ...}")
     ("simple_reference" (combobulate-node-text node))
     ("simple_link" (combobulate-node-text node))
     ("tag" (combobulate-node-text (combobulate-node-child node 0)))
     ("module_name" (combobulate-node-text node))
     (_ default-name))
   40))

(eval-and-compile
  (defvar combobulate-odoc-definitions
    '((context-nodes
       '("word" "code_span" "math_span" "escape_sequence"
         "module_name" "param_name" "raise_name" "before_version"
         "reference_target" "link_target" "language"))
      (envelope-list nil)
      (pretty-print-node-name-function #'combobulate-odoc-pretty-print-node-name)
      (highlight-queries-default nil)
      (procedures-sexp
       '((:activation-nodes
          ((:nodes ("heading" "paragraph" "code_block" "code_block_with_lang"
                    "verbatim_block" "math_block" "unordered_list"
                    "ordered_list" "table_heavy" "table_light"
                    "module_list" "raw_markup" "tag"))))))
      (procedures-defun
       '((:activation-nodes ((:nodes ("heading" "paragraph" "code_block"
                                      "code_block_with_lang" "verbatim_block"
                                      "unordered_list" "ordered_list"
                                      "table_heavy" "table_light"
                                      "tag"))))))
      (procedures-sibling
       '(;; navigate between top-level blocks
         (:activation-nodes
          ((:nodes ("heading" "paragraph" "code_block" "code_block_with_lang"
                    "verbatim_block" "math_block" "unordered_list"
                    "ordered_list" "table_heavy" "table_light"
                    "light_list" "module_list" "raw_markup" "tag"
                    "paragraph_style")
            :has-parent ("document")))
          :selector (:match-children t))
         ;; navigate between list items (heavy lists)
         (:activation-nodes
          ((:nodes ("dash_list_item" "li_list_item")
            :has-parent ("unordered_list" "ordered_list")))
          :selector (:match-children
                     (:match-rules ("dash_list_item" "li_list_item"))))
         ;; navigate between light list items
         (:activation-nodes
          ((:nodes ("light_list_item")
            :has-parent ("light_list")))
          :selector (:match-children (:match-rules ("light_list_item"))))
         ;; navigate between table rows
         (:activation-nodes
          ((:nodes ("table_row")
            :has-parent ("table_heavy")))
          :selector (:match-children (:match-rules ("table_row"))))
         ;; navigate between table cells
         (:activation-nodes
          ((:nodes ("table_header_cell" "table_data_cell")
            :has-parent ("table_row")))
          :selector (:match-children
                     (:match-rules ("table_header_cell" "table_data_cell"))))
         ;; navigate between inline elements within a paragraph
         (:activation-nodes
          ((:nodes ("bold" "italic" "emphasis" "superscript" "subscript"
                    "code_span" "math_span" "simple_reference"
                    "reference_with_text" "simple_link" "link_with_text"
                    "word" "escape_sequence")
            :has-parent ("paragraph" "bold" "italic" "emphasis"
                         "superscript" "subscript"
                         "dash_list_item" "li_list_item"
                         "table_header_cell" "table_data_cell"
                         "heading")))
          :selector (:match-children t))
         ;; navigate between modules in module_list
         (:activation-nodes
          ((:nodes ("module_name")
            :has-parent ("module_list")))
          :selector (:match-children (:match-rules ("module_name"))))))
      (procedures-hierarchy
       '(;; navigate into lists
         (:activation-nodes
          ((:nodes ("unordered_list" "ordered_list") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("dash_list_item" "li_list_item"))))
         ;; navigate into list items
         (:activation-nodes
          ((:nodes ("dash_list_item" "li_list_item") :position at))
          :selector (:choose node :match-children t))
         ;; navigate into tables
         (:activation-nodes
          ((:nodes ("table_heavy") :position at))
          :selector (:choose node :match-children (:match-rules ("table_row"))))
         ;; navigate into table rows
         (:activation-nodes
          ((:nodes ("table_row") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("table_header_cell" "table_data_cell"))))
         ;; navigate into styled text
         (:activation-nodes
          ((:nodes ("bold" "italic" "emphasis" "superscript" "subscript"
                    "reference_with_text" "link_with_text") :position at))
          :selector (:choose node :match-children t))
         ;; navigate into headings
         (:activation-nodes
          ((:nodes ("heading") :position at))
          :selector (:choose node :match-children
                             (:match-rules (exclude (all) "heading_marker"
                                                    "heading_marker_with_label"))))
         ;; navigate from document into blocks
         (:activation-nodes
          ((:nodes ("document") :position at))
          :selector (:choose node :match-children t))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-default
       '((:activation-nodes
          ((:nodes ("heading" "paragraph" "code_block" "code_block_with_lang"
                    "verbatim_block" "unordered_list" "ordered_list"
                    "table_heavy" "table_light" "tag")))))))))

(define-combobulate-language
 :name odoc
 :language odoc
 :major-modes (odoc-ts-mode)
 :custom combobulate-odoc-definitions
 :setup-fn combobulate-odoc-setup)

(defun combobulate-odoc-setup (_))

(provide 'combobulate-odoc)
;;; combobulate-odoc.el ends here
