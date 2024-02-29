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

(defgroup combobulate-yaml nil
  "Configuration switches for YAML"
  :group 'combobulate
  :prefix "combobulate-yaml-")



(defun combobulate-yaml-setup (_)
  ;; NOTE This is subject to change
  (setq combobulate-manipulation-envelopes
        '((:description
           "...:"
           :key "m"
           :mark-node t
           :nodes ("block_node")
           :name "block"
           :template
           (@ ":" n
              >))))
  (setq combobulate-navigation-context-nodes
        '("alias_name" "anchor_name" "block_sequence_item" "plain_scalar"
          "double_quote_scalar" "string_scalar" "single_quote_scalar"))
  (setq combobulate-manipulation-indent-after-edit nil)
  (setq combobulate-envelope-indent-region-function nil)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)

  (setq combobulate-manipulation-edit-procedures
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
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-procedures
        '((:activation-nodes ((:nodes ("flow_node" "block_node"))))))
  (setq combobulate-navigation-defun-procedures
        '((:activation-nodes ((:nodes ("stream" "block_node"))))))
  (setq combobulate-navigation-logical-procedures
        '((:activation-nodes ((:nodes ("stream" "flow_node" "block_node" "block_mapping_pair"))))))
  (setq combobulate-navigation-sibling-procedures
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
  (setq combobulate-navigation-parent-child-procedures
        `(;; basic down navigation between pairs and sequences
          (:activation-nodes
           ((:nodes ("block_mapping_pair" "block_sequence_item")))
           :selector (:choose node :match-children t))))

  (setq combobulate-navigation-default-procedures
        '((:activation-nodes ((:nodes ("block_sequence" "block_node" "block_mapping_pair")))))))

(provide 'combobulate-yaml)
;;; combobulate-yaml.el ends here
