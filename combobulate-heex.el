;;; combobulate-heex.el --- heex support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Victor Rodrigues

;; Author: Victor Rodrigues
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

;; Supports the tree-sitter-heex grammar.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-heex nil
  "Configuration switches for HEEx"
  :group 'combobulate
  :prefix "combobulate-heex-")

(eval-and-compile
  (defvar combobulate-heex-definitions
    '((context-nodes
       '("tag_name" "component_name" "slot_name" "attribute_name" "attribute_value"))
      (procedure-discard-rules nil)
      (display-ignored-node-types
       '("start_tag" "end_tag" "self_closing_tag" "tag_name"
         "start_component" "end_component" "self_closing_component" "component_name"
         "start_slot" "end_slot" "self_closing_slot" "slot_name"))
      (procedures-defun
       '((:activation-nodes ((:nodes ("tag" "component" "slot"))))))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ("attribute" "special_attribute")))
          :selector (:choose node
                             :match-siblings (:match-rules ("attribute" "special_attribute"))))
         (:activation-nodes
          ((:nodes
            ((exclude (all)
                      "start_tag" "end_tag" "self_closing_tag"
                      "start_component" "end_component" "self_closing_component"
                      "start_slot" "end_slot" "self_closing_slot"))
            :has-parent ("fragment" "tag" "component" "slot")))
          :selector (:match-children
                     (:match-rules
                      (exclude (all)
                               "start_tag" "end_tag" "self_closing_tag"
                               "start_component" "end_component" "self_closing_component"
                               "start_slot" "end_slot" "self_closing_slot"))))))
      (procedures-hierarchy
       '(;; `end_*' is kept so point can enter an element without children.
         (:activation-nodes
          ((:nodes ("tag" "component" "slot") :position at))
          :selector (:choose node
                             :match-children
                             (:discard-rules ("start_tag" "self_closing_tag"
                                              "start_component" "self_closing_component"
                                              "start_slot" "self_closing_slot"))))
         (:activation-nodes
          ((:nodes ("self_closing_tag" "self_closing_component" "self_closing_slot")
                   :position at))
          :selector (:choose node
                             :match-children (:match-rules ("attribute" "special_attribute"))))
         (:activation-nodes
          ((:nodes ("start_tag" "start_component" "start_slot"
                    "self_closing_tag" "self_closing_component" "self_closing_slot")
                   :position in))
          :selector (:choose node
                             :match-children (:match-rules ("attribute" "special_attribute"))))
         (:activation-nodes
          ((:nodes ("attribute" "special_attribute") :position in))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name heex
 :major-modes (heex-ts-mode)
 :custom combobulate-heex-definitions
 :setup-fn combobulate-heex-setup)

(defun combobulate-heex-setup (_))

(provide 'combobulate-heex)
;;; combobulate-heex.el ends here
