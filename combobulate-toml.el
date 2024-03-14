;;; combobulate-toml.el --- TOML mode support for Combobulate  -*- lexical-binding: t; -*-

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
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-toml-pretty-print-node-name (node default-name)
  "Pretty print the name of NODE."
  (combobulate-string-truncate
   (pcase (combobulate-node-type node)
     ("table" (concat "[" (combobulate-node-text (combobulate-node-child node 0)) "]"))
     ("table_array_element" (concat "[[" (combobulate-node-text (combobulate-node-child node 0)) "]]"))
     ("pair" (concat
              (combobulate-node-text (combobulate-node-child node 0))))
     ("string" (concat (combobulate-node-text node)))
     ;; ("bare_key" (concat (combobulate-node-text (combobulate-node-next-sibling node))))
     (_ default-name))
   40))

(defun combobulate-toml-setup (_)
  (setq combobulate-navigation-context-nodes
        '("string" "float" "integer"))

  (setq combobulate-manipulation-envelopes nil)

  (setq combobulate-pretty-print-node-name-function #'combobulate-toml-pretty-print-node-name)
  (setq combobulate-highlight-queries-default nil)
  (setq combobulate-procedures-edit
        `((:activation-nodes
           ((:nodes
             ("bare_key" "dotted_key")
             :has-ancestor ((irule "pair"))))
           :selector (:choose
                      parent
                      :match-query
                      (:query (_ (pair (_) @match)+) :engine combobulate)))
          ;; edit the value field of a pair
          (:activation-nodes
           ((:nodes
             ((exclude (rule "pair") "bare_key" "dotted_key"))
             :has-ancestor ((irule "pair"))))
           :selector (:choose
                      parent
                      :match-query
                      (:query (_ (pair (_) (_) @match)+) :engine combobulate)))))
  
  (setq combobulate-procedures-sexp
        '((:activation-nodes ((:nodes ("table"))))))
  (setq combobulate-procedures-defun '((:activation-nodes ((:nodes ("table" "table_array_element"))))))

  (setq combobulate-procedures-sibling
        '(;; array navigation
          (:activation-nodes
           ((:nodes
             ((rule "array"))
             :position at
             :has-parent ((rule "array"))))
           :selector (:match-children t))
          ;; table navigation
          (:activation-nodes
           ((:nodes
             ((rule "document"))
             :position at
             :has-parent ("document")))
           :selector (:match-children t))
          ;; pair-wise navigation (key side)
          (:activation-nodes
           ((:nodes ("pair") :position at :has-parent ("table" "table_array_element")))
           :selector (:match-children (:match-rules ("pair"))))
          (:activation-nodes
           ((:nodes
             ((rule "pair"))
             :has-fields "value"
             :has-ancestor ((irule "pair"))))
           :selector (:choose
                      parent
                      :match-query
                      (:query (object (pair (_) (_) @match)+) :engine combobulate)))))

  (setq combobulate-procedures-hierarchy
        '(;; in and out of tables
          (:activation-nodes
           ((:nodes ("table" "table_array_element") :position at))
           :selector (:choose node :match-children (:match-rules (exclude (all) "bare_key"))))
          ;; general navigation
          (:activation-nodes
           ((:nodes (exclude (all) "string") :position at))
           :selector (:choose node :match-children t))))
  (setq combobulate-procedures-logical '((:activation-nodes ((:nodes (all)))))))

(provide 'combobulate-toml)
;;; combobulate-toml.el ends here
