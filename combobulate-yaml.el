;;; combobulate-yaml.el --- yaml support for combobulate  -*- lexical-binding: t; -*-

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

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(eval-and-compile
  (defvar combobulate-yaml-definitions
    '((envelope-list)
      (context-nodes
       '("alias_name" "anchor_name" "block_sequence_item" "plain_scalar"
         "double_quote_scalar" "string_scalar" "single_quote_scalar"))
      (indent-after-edit nil)
      (envelope-indent-region-function nil)
      (procedures-edit
       '((:activation-nodes
          ((:nodes ("block_sequence")))
          :selector (:match-query
                     (:discard-rules
                      ("comment")
                      :query (block_sequence (block_sequence_item (flow_node)  @match)+))))
         (:activation-nodes
          ((:node ("block_mapping") :has-parent ("block_node")))
          :selector (:match-query
                     (:query
                      (block_node (block_mapping (_)+ @match))
                      :discard-rules ("comment"))))))

      (procedures-sexp
       '((:activation-nodes ((:nodes ("flow_node" "block_node"))))))
      (procedures-defun
       '((:activation-nodes ((:nodes ("stream" "block_node"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes ("stream" "flow_node" "block_node" "block_mapping_pair"))))))
      (procedures-sibling
       `((:activation-nodes
          ((:nodes
            ("comment")
            :has-parent ("block_mapping" "block_sequence" "document" "stream")))
          :selector (:match-children t))
         (:activation-nodes
          ((:nodes
            ("block_mapping_pair" "block_sequence_item")
            :has-parent ("block_mapping" "block_sequence")))
          :selector (:match-children t))))
      (procedures-hierarchy
       `(;; basic down navigation between pairs and sequences
         (:activation-nodes
          ((:nodes ("block_mapping_pair" "block_sequence_item")))
          :selector (:choose node :match-children t))))

      (procedures-default
       '((:activation-nodes ((:nodes ("block_sequence" "block_node" "block_mapping_pair")))))))))

(define-combobulate-language
 :name yaml
 :language yaml
 :major-modes (yaml-ts-mode yaml-mode)
 :custom combobulate-yaml-definitions
 :setup-fn combobulate-yaml-setup)

(defun combobulate-yaml-setup (_)
  ;; NOTE This is subject to change
  )

(provide 'combobulate-yaml)
;;; combobulate-yaml.el ends here
