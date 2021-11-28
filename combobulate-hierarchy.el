;;; combobulate-hierarchy.el --- hierarchical tree management for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

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

;;; Code:

(defvar combobulate-debug nil
  "Enables additional debug information useful for Combobulate developers")

(defvar combobulate-pretty-print-node-separator "⏺"
  "Character(s) used to separate nodes in the pretty printer")

(defvar combobulate-pretty-print-function #'combobulate--pretty-print-node
  "Buffer local function that pretty prints a combobulate node")
(make-variable-buffer-local 'combobulate-pretty-print-function)

(defvar combobulate-pretty-print-node-name-function #'combobulate--pretty-print-node-name
  "Buffer local function that pretty prints the node name

This variable must be funcalled by the function in
`combobulate-pretty-print-function'.")

(make-variable-buffer-local 'combobulate-pretty-print-node-name-function)

(defface combobulate-highlighted-node-face '((t (:inherit font-lock-doc-face)))
  "Face for combobulate nodes that are prominently displayed in the UI"
  :group 'combobulate)

;; Pretty printing and display options

(defun combobulate--pretty-print-node-name (node default-name)
  "Pretty prints the name of NODE"
  (if node (concat (mapconcat 'capitalize (split-string (symbol-name (tsc-node-type node)) "[_-]") " "))
    default-name))

(defun combobulate-pretty-print-node (node &optional highlighted)
  "Pretty prints NODE and optionally HIGHLIGHTED"
  (combobulate--pretty-print-node node highlighted))

(defun combobulate--pretty-print-node (node &optional highlighted)
  "Internal method that pretty prints NODE and returns a string of text.

If HIGHLIGHTED then the node is highlighted with
`combobulate-highlighted-node-face'. "
  (let ((s (funcall combobulate-pretty-print-node-name-function node (combobulate--pretty-print-node-name node ""))))
    (concat (format "%s %s"
                    (if highlighted (propertize s 'face 'combobulate-highlighted-node-face) s)
                    (if combobulate-debug (tsc-node-byte-range node) "")))))


;;; Node comparison functions

(defun combobulate--on-node-p (node)
  "Returns t if the current node at point is equal to NODE"
  (or (= (tsc-node-start-position node) (point))
      ;; (tsc-node-eq (tree-sitter-node-at-point) node)
      ))

(defun combobulate--point-in-node-range-p (node)
  "Returns t if point is contained between NODE's start and end positions"
  (and (>= (point) (tsc-node-start-position node))
       (< (point) (or (tsc-node-end-position node) (point-max)))))

(defun combobulate--point-in-node-overlaps-p (node)
  "Returns t if point overlaps NODE's start position"
  (>= (point) (tsc-node-start-position node)))

(defun combobulate--node-contains-node-p (node-a node-b)
  "Returns t if NODE-A is wholly contained inside NODE-B"
  (and (>= (tsc-node-start-position node-a)
           (tsc-node-start-position node-b))
       (< (tsc-node-end-position node-a)
          (tsc-node-end-position node-b))))

(defun combobulate--node-before-node-p (node-a node-b)
  "Returns t if NODE-A is positioned before NODE-B"
  (and node-a node-b
       (< (tsc-node-start-position node-a)
          (tsc-node-start-position node-b))))

(defun combobulate--node-after-node-p (node-a node-b)
  "Returns t if NODE-A is positioned after NODE-B"
  (and node-a node-b
       (> (tsc-node-start-position node-a)
          (tsc-node-start-position node-b))))

(defun combobulate--node-visible-window-p (node)
  "Returns t if NODE is visible in the current window"
  (and node
       ;; Should we disallow partial occlusion of a node?

       ;; This appears to be faster than `pos-visible-in-window-p'. More research needed.
       (>= (tsc-node-start-position node)
           (save-excursion (goto-char (point-min))
                           (forward-line (1- (line-number-at-pos (window-start))))
                           (point)))
       (<= (tsc-node-start-position node)
           (save-excursion (goto-char (point-min))
                           (forward-line (1- (line-number-at-pos (window-end))))
                           (point)))
       ;; Too slow?
       ;; (pos-visible-in-window-p (tsc-node-start-position node) (selected-window))
       ))

(defun combobulate--node-on-or-after-node-p (node-a node-b)
  "Returns t if NODE-A is positioned on or after NODE-B"
  (and node-a node-b
       (>= (tsc-node-start-position node-a)
           (tsc-node-start-position node-b))))

(defun combobulate--node-before-point-p (node)
  "Returns t if NODE's start position is less than point"
  (< (tsc-node-start-position node) (point)))

(defun combobulate--node-after-or-on-point-p (node)
  "Returns t if NODE's start position is equal to or greater than point"
  (>= (tsc-node-start-position node) (point)))




(provide 'combobulate-hierarchy)
;;; combobulate-hierarchy.el ends here
