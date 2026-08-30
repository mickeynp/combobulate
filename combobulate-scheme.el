;;; combobulate-scheme.el --- Scheme support  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  coopi

;; Author: coopi <coldsideofyourpillow@disroot.org>
;; Keywords: convenience, languages, scheme

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

;; Structural editing and navigation for Scheme using tree-sitter-scheme.

;;; Code:

(require 'combobulate-navigation)
(require 'combobulate-rules)
(require 'combobulate-setup)

(eval-and-compile
  (defconst combobulate-scheme--datum-rules
    '((exclude (rule "program")
               "block_comment" "comment" "directive"))
    "Rules matching Scheme datums.")

  (defconst combobulate-scheme--compound-datum-rules
    '((exclude (irule "symbol")
               "comment" "program"))
    "Rules matching Scheme datums that can contain other datums.")

  (defconst combobulate-scheme-definitions
    `((context-nodes
       '("boolean" "character" "keyword" "number" "string" "symbol"))
      (envelope-default-list
       '((:description
          "( ... )"
          :key "("
          :extra-key "M-("
          :mark-node t
          :split-node t
          :nodes ((:activation-nodes
                   ((:nodes ,combobulate-scheme--datum-rules))))
          :name "wrap-parentheses"
          :template (@ "(" r ")"))))
      (plausible-separators nil)
      (procedure-discard-rules '("block_comment" "comment" "directive"))
      (procedures-default
       '((:activation-nodes
          ((:nodes ,combobulate-scheme--datum-rules)))))
      (procedures-defun
       '((:activation-nodes
          ((:nodes ,combobulate-scheme--datum-rules
            :has-parent ("program"))))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ,combobulate-scheme--compound-datum-rules))
          :selector
          (:choose node
           :match-children
           (:match-rules ,combobulate-scheme--datum-rules)))))
      (procedures-sexp
       '((:activation-nodes
          ((:nodes ,combobulate-scheme--datum-rules)))))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ,combobulate-scheme--datum-rules
            :has-parent ("program" "list" "vector" "byte_vector")))
          :selector
          (:choose parent
           :match-children
           (:match-rules ,combobulate-scheme--datum-rules))))))
    "Combobulate procedure definitions for Scheme."))

(define-combobulate-language
 :name scheme
 :language scheme
 :major-modes (scheme-mode)
 :custom combobulate-scheme-definitions
 :setup-fn combobulate-scheme-setup)

(defun combobulate-scheme-setup (_))

(provide 'combobulate-scheme)
;;; combobulate-scheme.el ends here
