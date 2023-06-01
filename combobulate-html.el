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
  "Insert `=' or maybe a JSX attribute."
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
        (atomic-change-group
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
  (if (and node (equal (combobulate-node-type node) "element"))
      (format "<%s>" (thread-first node
                                   (combobulate-node-child 0)
                                   (combobulate-node-child 0)
                                   (combobulate-node-text)))
    default-name))

(defun combobulate-html-setup (_)
  (setq combobulate-navigation-default-nodes '("element" "comment"))
  (setq combobulate-navigation-sexp-nodes '("element" "attribute" "text"))
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
           :nodes ("element" "self_closing_tag" "jsx_fragment")
           :key "t"
           :template ("<" (p tag "Tag Name: ") ">" n>
                      r>
                      n> "</" (field tag) ">"))
          (:description
           "...={ ... }"
           :key "=e"
           :mark-node nil
           :point-placement 'stay
           :nodes ("start_tag" "self_closing_tag")
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
           ((:node
             "attribute"
             :find-parent ("start_tag" "self_closing_tag")
             :position at-or-in))
           :match-query ((_) (attribute)+ @match))
          ;; sibling-level editing
          (:activation-nodes
           ((:node
             ("self_closing_tag" "expression" "element" "fragment")
             :position at))
           :remove-types ("comment" "text")
           :match-siblings (:keep-parent nil))
          ;; editing an element's opening/closing tag
          (:activation-nodes
           ((:node
             "element"
             :position in))
           :remove-types ("comment")
           :match-query (element (start_tag (tag_name) @match)
                                 (end_tag (tag_name) @match)))))

  (setq combobulate-navigation-sibling-procedures
        `((:activation-nodes
           ((:node
             ("element")
             :position at-or-in
             :find-immediate-parent ("element")))
           :match-children (:keep-types ("element")))
          (:activation-nodes
           ((:node
             ("attribute")
             :position at-or-in
             :find-parent ("start_tag" "self_closing_tag")))
           :match-children (:keep-types ("attribute")))))

  (setq combobulate-pretty-print-node-name-function #'combobulate-html-pretty-print))

(provide 'combobulate-html)
;;; combobulate-html.el ends here
