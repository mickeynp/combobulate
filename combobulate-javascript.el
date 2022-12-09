;;; combobulate-javascript.el --- structured editing for JS+JSX and Typescript in combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
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

(defun combobulate-jsx-has-open-close-tags-p (node)
  ;; self-closing contains neither `:open_tag' nor `:close_tag'
  (when (eq (combobulate-node-type node) 'jsx_self_closing_element) nil)
  (let* ((opening-node (combobulate-jsx--get-open-tag node))
         (closing-node (combobulate-jsx--get-close-tag node)))
    (and (eq (combobulate-node-type node) 'jsx_element) opening-node closing-node)))

(defun combobulate--jsx-get-twinned-tag (node)
  "Returns the twinned tag of NODE.

If NODE is a self-closing element, it returns NODE.

If NODE is the opening tag of a pair then it returns the closing
tag, and vice versa."
  (let ((parent-node (combobulate-node-parent node)))
    (pcase (combobulate-node-type node)
      ('jsx_self_closing_element node)
      ('jsx_opening_element (combobulate-jsx--get-close-tag parent-node))
      ('jsx_closing_element (combobulate-jsx--get-open-tag parent-node)))))

(defun combobulate-jsx--get-open-tag (node)
  (combobulate-node-chield-by-field node :open_tag))

(defun combobulate-jsx--get-close-tag (node)
  (combobulate-node-chield-by-field node :close_tag))

(defun combobulate-jsx--get-tag-identifier (node)
  "Given a JSX Element return its name tag node"
  (combobulate-node-chield-by-field node :name))

(defun combobulate-javascript-jsx--wrap (start end)
  "Transforms the JSX Element node at point to a JSX Expression."
  (interactive)
  (when-let (node (combobulate-node-at-point '(jsx_element jsx_self_closing_element)))
    (let ((pp-node (combobulate-pretty-print-node node)))
      (goto-char (combobulate-node-end node))
      (insert end)
      (goto-char (combobulate-node-start node))
      (insert start)
      pp-node)))

(defun combobulate-javascript-jsx-element-to-comment ()
  "Transforms the JSX Element node at point to a JSX Expression."
  (interactive)
  (message "Converted %s to a comment" (combobulate-javascript-jsx--wrap "{/*" "*/}")))


(defun combobulate-javascript-jsx-element-to-expression ()
  "Transforms the JSX Element node at point to a JSX Expression."
  (interactive)
  (message "Converted %s to an expression" (combobulate-javascript-jsx--wrap "{" "}")))


(defun combobulate-javascript-jsx-vanish-node ()
  "Vanishes the JSX Element node at point, leaving its children untouched.

If the node at point is self-closing then it is removed in full."
  (interactive)
  (when-let (node (combobulate-node-at-point '(jsx_element jsx_self_closing_element)))
    (let* ((opening-node (combobulate-jsx--get-open-tag node))
           (closing-node (combobulate-jsx--get-close-tag node))
           (pp-node (combobulate-pretty-print-node node)))
      (unless (or (eq (combobulate-node-type node) 'jsx_self_closing_element)
                  (and (eq (combobulate-node-type opening-node) 'jsx_opening_element)
                       (eq (combobulate-node-type closing-node) 'jsx_closing_element)))
        (error "Cannot vanish element. Must be a JSX self-closing element or a JSX element"))
      ;; self closing elements have neither an `:open_tag' nor a `:close_tag'
      (when (eq (combobulate-node-type node) 'jsx_self_closing_element)
        (setq opening-node node
              closing-node node))
      (save-excursion
        (delete-region
         (combobulate-node-start closing-node)
         (combobulate-node-end closing-node))
        (goto-char (combobulate-node-start closing-node))
        (delete-blank-lines)
        (delete-region (combobulate-node-start opening-node)
                       (combobulate-node-end opening-node))
        (goto-char (combobulate-node-start opening-node))
        (delete-blank-lines))
      (message "Vanished %s..." pp-node))))

(defun combobulate-javascript-jsx-rename-element ()
  "Renames the current element point is on"
  (interactive)
  (when-let (node (combobulate-node-looking-at '(jsx_opening_element jsx_closing_element jsx_self_closing_element)))
    (combobulate--mc-edit-nodes
     (list (combobulate-jsx--get-tag-identifier node)
           (combobulate-jsx--get-tag-identifier (combobulate--jsx-get-twinned-tag node))))))

(defvar combobulate-javascript-key-map
  (let ((map (make-sparse-keymap)))
    ;; Transformations
    (define-key map (kbd "v") #'combobulate-javascript-jsx-vanish-node)
    (define-key map (kbd "e") #'combobulate-javascript-jsx-element-to-expression)
    (define-key map (kbd ";") #'combobulate-javascript-jsx-element-to-comment)
    (define-key map (kbd "r") #'combobulate-javascript-jsx-rename-element)
    map))

(defun combobulate-javascript-pretty-print-node-name (node default-name)
  "Pretty printer for JS and JSX nodes"
  (cl-flet ((make-tag-text (node) (concat "<" (combobulate-node-text node) ">")))
    (pcase (combobulate-node-type node)
      ;; Turn JSX elements into something resembling an SGML-styled
      ;; tag as the default name is too generic to be useful.
      ('jsx_element (make-tag-text (combobulate-jsx--get-tag-identifier (combobulate-jsx--get-open-tag node))))
      ('jsx_self_closing_element (make-tag-text (combobulate-jsx--get-tag-identifier node)))
      ('jsx_opening_element (make-tag-text (combobulate-jsx--get-tag-identifier node)))
      ('jsx_closing_element (make-tag-text (combobulate-jsx--get-tag-identifier node)))
      (_ default-name))))




(defun combobulate-setup-js-ts ()
  (local-set-key (kbd "C-c o t") combobulate-javascript-key-map)
  (setq combobulate-pretty-print-node-name-function #'combobulate-javascript-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-node-cluster-queries
        '((object (pair \. (_) @match))
          (jsx_self_closing_element (jsx_attribute ((property_identifier) @match)))
          (jsx_opening_element (jsx_attribute ((property_identifier) @match)))))
  (setq-local combobulate-navigation-sexp-nodes '("jsx_opening_element"
                                                  "jsx_closing_element"
                                                  "jsx_text"
                                                  "jsx_expression"
                                                  "jsx_self_closing_element"))
  (setq-local combobulate-navigation-defun-nodes '("arrow_function" "function_declaration"))
  (setq-local combobulate-navigation-default-nodes '("program"
                                                     "arrow_function"
                                                     "function_declaration"
                                                     "function"
                                                     "jsx_attribute"
                                                     "lexical_declaration"
                                                     "ternary_expression"
                                                     "export_statement"
                                                     "jsx_fragment"
                                                     "jsx_element"
                                                     "jsx_expression"
                                                     "jsx_self_closing_element"
                                                     )))


(provide 'combobulate-javascript)
;;; combobulate-javascript.el ends here
