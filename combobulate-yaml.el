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
              y>))
          ))

  (setq combobulate-manipulation-indent-after-edit nil)
  (setq combobulate-envelope-indent-region-function nil)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-indent-method 'first)
  (setq combobulate-manipulation-trim-empty-lines t)

  (setq combobulate-manipulation-edit-procedures
        '((:activation-nodes
           ((:node "block_sequence" :position at-or-in))
           :match-query (block_sequence (block_sequence_item (flow_node)  @match)+))
          (:activation-nodes
           ((:node "block_mapping" :find-parent "block_node" :position at-or-in))
           :match-query (block_node (block_mapping (_)+ @match))
           :remove-types ("comment"))))
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-nodes '("flow_node" "block_node"))
  (setq combobulate-manipulation-splicing-procedures nil)
  (setq combobulate-navigation-drag-parent-nodes '("block_node" "block_sequence" "stream"))
  (setq combobulate-navigation-defun-nodes '("stream" "block_node"))
  (setq combobulate-navigation-logical-nodes '("stream" "flow_node" "block_node"
                                               "block_mapping_pair"))
  (setq combobulate-navigation-sibling-procedures
        `((:activation-nodes
           ((:node
             ,(append '("block_mapping_pair" "block_sequence_item"))
             :position at-or-in
             :find-immediate-parent ("block_mapping" "block_sequence")))
           :remove-types ("comment")
           :match-children t)))
  (setq combobulate-navigation-parent-child-nodes
        '("block_sequence" "block_sequence_item" "block_node" "block_mapping_pair"))
  (combobulate-production-rules-set '("block_mapping_pair"
                                      :remove-types ("anchor")
                                      :expand-nodes (("block_mapping" :all t))))
  (combobulate-production-rules-set '("block_node"
                                      :remove-types ("anchor")
                                      :expand-nodes (("block_mapping" :all t))))
  (combobulate-production-rules-set '("document"
                                      :expand-nodes (("block_node" :all t))))
  (combobulate-production-rules-set '("stream"
                                      :expand-nodes (("document" :all t)
                                                     ("block_node" :all t)
                                                     ("block_mapping" :all t))))
  (setq combobulate-navigation-default-nodes
        '("block_sequence" "block_node" "block_mapping_pair")))

(provide 'combobulate-yaml)
;;; combobulate-yaml.el ends here
