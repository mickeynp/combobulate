;;; combobulate-html.el --- HTML and SGML-alike structured editing for combobulate  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-rules)

(defvar combobulate-js-ts-attribute-envelope-default)
(defvar combobulate-js-ts-attribute-envelope-alist)
(defvar-local combobulate-sgml-open-tag nil)
(defvar-local combobulate-sgml-close-tag nil)
(defvar-local combobulate-sgml-whole-tag nil)
(defvar-local combobulate-sgml-self-closing-tag nil)
(defvar-local combobulate-sgml-exempted-tags '("jsx_expression"))


(declare-function combobulate-execute-envelope "combobulate-manipulation")

(defun combobulate-maybe-auto-close-tag ()
  "Insert `>' or maybe insert the closing tag."
  (interactive)
  (self-insert-command 1 ?>)
  (save-excursion
    (forward-char -1)
    (when (not (eq ?/ (char-before)))
      (let ((open-node (combobulate-node-at-point (list combobulate-sgml-open-tag))))
        ;; only attempt an expansion if we are not inside one of the exempted tags.
        (when (and open-node
                   (= (combobulate-node-end open-node) (1+ (point)))
                   (not (combobulate-node-contains-node-p
                         (combobulate-node-at-point combobulate-sgml-exempted-tags)
                         open-node)))
          (forward-char 1)
          (insert (format "</%s>" (combobulate-node-text
                                   (or
                                    ;; for tsx/jsx
                                    (combobulate-node-child-by-field
                                     open-node "name")
                                    ;; for `html'
                                    (combobulate-node-child
                                     open-node 0))))))))))

;; Note: brittle.
;; (defun combobulate-maybe-close-tag-or-self-insert ()
;;   "Insert `/' or maybe close the nearest unopened tag."
;;   (interactive)
;;   (cl-flet ((get-text (node closing)
;;               (if (equal (combobulate-parser-language (combobulate-parser-node node)) 'html)
;;                   (thread-first node
;;                                 (combobulate-node-child
;;                                  (if (equal field combobulate-sgml-close-tag)
;;                                      -1
;;                                    0))
;;                                 (combobulate-node-child 0)
;;                                 (combobulate-node-text))
;;                 (thread-first node
;;                               (combobulate-node-child-by-field
;;                                (if closing "close_tag" "open_tag"))
;;                               (combobulate-node-child-by-field "name")
;;                               (combobulate-node-text)))))
;;     (if (looking-back "<" 1)
;;         (progn
;;           (let* ((element (combobulate-node-at-point (list combobulate-sgml-whole-tag))))
;;             (if (equal (get-text element nil)
;;                        (get-text element t))
;;                 (self-insert-command 1 ?/)
;;               (combobulate-message "Closing node" element)
;;               (combobulate-pulse-node (combobulate-node-child element 0))
;;               (insert (format "/%s>" (get-text element combobulate-sgml-open-tag))))))
;;       (self-insert-command 1 ?/))))

(defun combobulate-maybe-insert-attribute ()
  "Insert `=' or maybe a an attribute."
  (interactive)
  (if-let ((node (combobulate-node-at-point (list combobulate-sgml-open-tag
                                                  combobulate-sgml-self-closing-tag))))
      ;; Expanding an attribute is only desirable if we are inside an
      ;; open (or self-closing) node, but *also not* inside a node
      ;; type in `combobulate-sgml-exempted-tags' which happens to be
      ;; inside one of those node types.
      (if (combobulate-node-contains-node-p
           (combobulate-node-at-point combobulate-sgml-exempted-tags)
           node)
          (self-insert-command 1 ?=)
        (combobulate-atomic-change-group
          (catch 'done
            (unless (equal (combobulate-parser-language (combobulate-parser-node node)) 'html)
              (pcase-dolist (`(,attribute . ,envelope) combobulate-js-ts-attribute-envelope-alist)
                (when (looking-back (concat "\\<" attribute "\\>") (length attribute))
                  (combobulate-execute-envelope envelope node)
                  (throw 'done t))))
            ;; catch flagrant, out-of-place uses of `='.
            (if (looking-back "[[:alpha:]]" 1)
                (cond
                 ;; html can only do strings, so just use that.
                 ((equal (combobulate-parser-language (combobulate-parser-node node)) 'html)
                  (combobulate-execute-envelope "attr-string" node))
                 (combobulate-js-ts-attribute-envelope-default
                  (combobulate-execute-envelope combobulate-js-ts-attribute-envelope-default node))
                 (t (self-insert-command 1 ?=)))
              (self-insert-command 1 ?=)))))
    (self-insert-command 1 ?=)))

(defun combobulate-html-pretty-print (node default-name)
  (cond
   ((and node (member (combobulate-node-type node) '("element" "script_element" "style_element")))
    (format "<%s>" (thread-first node
                                 (combobulate-node-child 0)
                                 (combobulate-node-child 0)
                                 (combobulate-node-text))))
   ((and node (equal (combobulate-node-type node) "text"))
    (format "`%s'" (combobulate-node-text node)))
   ((and node (equal (combobulate-node-type node) "comment"))
    (combobulate-node-text node))
   (t default-name)))

(defun combobulate-html-setup (_)
  (setq combobulate-navigation-sexp-procedures
        '((:activation-nodes ((:nodes ("element" "attribute" "text" "script_element" "style_element"))))))
  (setq combobulate-navigation-context-nodes '("attribute_name" "attribute_value" "tag_name" "text"))
  (local-set-key (kbd "=") #'combobulate-maybe-insert-attribute)
  ;; (local-set-key (kbd "/") #'combobulate-maybe-close-tag-or-self-insert)
  (local-set-key (kbd ">") #'combobulate-maybe-auto-close-tag)

  (setq combobulate-sgml-open-tag "start_tag")
  (setq combobulate-sgml-close-tag "end_tag")
  (setq combobulate-sgml-whole-tag "element")
  (setq combobulate-sgml-self-closing-tag "self_closing_tag")
  (setq combobulate-manipulation-envelopes
        '((:description
           "<...> ... </...>"
           :name "tag"
           :mark-node t
           :nodes ("element" "self_closing_tag" "jsx_fragment" "script_element" "style_element" "text")
           :key "t"
           :template ("<" (p tag "Tag Name: ") ">" n>
                      r>
                      n> "</" (field tag) ">"))
          (:description
           "...={ ... }"
           :key "=e"
           :mark-node nil
           :point-placement 'stay
           :nodes ("start_tag" "self_closing_tag" "script_element" "style_element")
           :name "attr-expression"
           :template ("=" "{" @ "}"))
          (:description
           "...={{ ... }}"
           :key "=E"
           :mark-node nil
           :point-placement 'stay
           :nodes ("start_tag" "self_closing_tag")
           :name "attr-expression-object"
           :template ("=" "{{" @ "}}"))
          (:description
           "...=\" ... \""
           :key "=s"
           :mark-node nil
           :point-placement 'stay
           :nodes ("start_tag" "self_closing_tag")
           :name "attr-string"
           :template ("=" "\"" @ "\""))))

  (setq combobulate-manipulation-edit-procedures
        '((:activation-nodes
           ((:nodes
             ("attribute")
             :has-parent ("start_tag" "self_closing_tag" "script_element" "style_element")))
           :selector (:match-query (:query ((_) (attribute)+ @match)
                                           :engine combobulate)))
          ;; sibling-level editing
          (:activation-nodes
           ((:nodes
             ("self_closing_tag" "expression" "element" "document" "script_element" "style_element")
             :position at))
           :selector (:match-siblings (:discard-rules ("comment" "text"))))
          ;; editing an element's opening/closing tag
          (:activation-nodes
           ((:node
             ("element" "script_element" "style_element")
             :position in))
           :selector (:match-query
                      (:query
                       (_ (start_tag (tag_name) @match)
                          (end_tag (tag_name) @match))
                       :engine combobulate
                       :discard-rules ("comment"))))))
  (setq combobulate-navigation-parent-child-procedures
        '(;; seamless navigation between elements and their children.
          (:activation-nodes
           ((:nodes ("element" "script_element" "style_element") :position at))
           ;; do not discard "end_tag" as it lets us navigate into a
           ;; tag without any children. Bit of a hack...
           :selector (:choose node :match-children (:discard-rules ("start_tag" "tag_name"))))
          ;; go into attribute if point is inside the start tag
          (:activation-nodes
           ((:nodes ("start_tag" "self_closing_tag") :position in))
           :selector (:choose node
                              :match-children
                              (:match-rules ("attribute"))))
          ;; if we're inside an attribute, go to its value
          (:activation-nodes
           ((:nodes ("attribute") :position in))
           :selector (:choose node :match-children t))))
  ;; Ordinarily, we discard comments as they tend to be line
  ;; comments. This is not a problem in HTML where they have
  ;; beginnings and ends.
  (setq combobulate-procedure-discard-rules nil)
  (setq combobulate-display-ignored-node-types '("start_tag" "self_closing_tag" "end_tag" "tag_name"))
  (setq combobulate-navigation-sibling-procedures
        '((:activation-nodes
           ((:nodes
             ("attribute")))
           :selector (:choose node :match-siblings (:match-rules ("attribute"))))
          (:activation-nodes
           ((:nodes
             ((rule "document") "comment")
             :has-parent ((rule "document") "document" "start_tag" "self_closing_tag")))
           :selector (:match-children (:match-rules (exclude (all) "start_tag" "end_tag" "self_closing_tag"))))))
  (setq combobulate-pretty-print-node-name-function #'combobulate-html-pretty-print))

(provide 'combobulate-html)
;;; combobulate-html.el ends here
