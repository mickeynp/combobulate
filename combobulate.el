;;; combobulate.el --- edit and navigate text by syntactic constructs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((tree-sitter 0.13) (multiple-cursors 20210323.1128) (hydra 20200711.1210) (avy 20201226.1734))
;; Homepage: https://www.github.com/mickeynp/combobulate.el
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
;; Navigate and transform source code using the concrete syntax tree
;; provided by the Emacs package `tree-sitter' and its parent module.

;;; Code:

;; Requirements:
(unless (functionp 'module-load)
  (error "You must have dynamic module support compiled for `combobulate.el' to function"))

(require 'seq)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(eval-when-compile
  (require 'cl-lib))
(require 'multiple-cursors-core)
(require 'hydra)


(defvar combobulate--last-node-path nil)
(defvar combobulate--last-navigation-command nil)
(defvar combobulate-flash-node t
  "When non-nil, show a notification when a navigable node changes in a buffer")
(defvar combobulate--last-navigation-node nil
  "The last node combobulate navigated to")


(defvar combobulate-strict-node-at-point-check t
  "Use a strict method for finding the node at point if non-nil.

The strict node at point check may not match the expectations of
where a node begins and ends from a conceptual perspective. This
is especially true if the point is at the end of a line or on an
empty or whitespaced line.")

(defun combobulate-node-at-point (node-types)
  "Return the smallest syntax node at point whose type is one of NODE-TYPES "
  (let* ((root (tsc-root-node tree-sitter-tree))
         (p (if combobulate-strict-node-at-point-check
                (point)
              (save-excursion
                (skip-chars-backward "\t\n ")
                (when (eolp)
                  (forward-char -1))
                (point))))
         (node (tsc-get-descendant-for-position-range root p p)))
    (when node-types
      (let ((this node))
        (catch 'done
          (while this
            (let ((smallest-node (tsc-get-descendant-for-position-range
                                  this
                                  (tsc-node-start-position this)
                                  (tsc-node-start-position this))))
              (cond
               ((member (tsc-node-type this) node-types) (throw 'done this))
               ((member (tsc-node-type smallest-node) node-types) (throw 'done smallest-node))
               (t (setq this (tsc-get-parent this)))))))))))

(defun combobulate-all-nodes-at-point ()
  "Returns all nodes at `point' where the start position of each node is equal to point

The returned list is ordered smallest-to-largest by the node's extent."
  (let ((nodes)
        (sub-node (tsc-get-descendant-for-position-range (tsc-root-node tree-sitter-tree) (point) (point))))
    (while (and sub-node (= (tsc-node-start-position sub-node) (point)))
      (push sub-node nodes)
      (setq sub-node (tsc-get-parent sub-node)))
    (reverse nodes)))


(defun combobulate-node-looking-at (node-types)
  "Returns the node point is looking at if it is one of NODE-TYPES."
  (when-let (node (combobulate-node-at-point node-types))
    (when (combobulate--point-in-node-range-p node)
      node)))

(defun combobulate-node-point (node &optional end)
  "Returns the `point' of NODE at its beginning or END

If NODE is nil, then nil is returned."
  (when node
    (if end (tsc-node-end-position node)
      (tsc-node-start-position node))))

(defvar combobulate--navigation-node-queries '(((try_statement "try" @match)))
  "Node types used")

(defvar combobulate-navigation-node-types nil
  "Node types used for navigation")
(make-variable-buffer-local 'combobulate-navigation-node-types)

(defun combobulate-manipulation-node-cluster-queries nil
  "Alist of (NODE-TYPE . QUERY) for selecting clusters of nodes

Each Alist entry must have a valid NODE-TYPE that anchors the
query. If you want to map all the elements of a list, for
instance, the anchor element should be the parent of those
elements --- for instance `list' or `tuple'.

The QUERY must be a valid query that has at least one capture
name called `@match'. The QUERY should ideally have the NODE-TYPE
in it also to ensure the query match is limited to just
NODE-TYPE. However, if there is another sub-type that matches,
then that can be used instead.

For instance:

    (dictionary . (pair \\. (_) @match)

Matches all the key-portion of key-value pairs in a dictionary")
(make-variable-buffer-local 'combobulate-manipulation-node-cluster-queries)

(defhydra combobulate-menu (:color red :hint nil)
  "
  ^Navigation^             ^Marking and Editing^
------------------------------------------------------
  _j_: Jump (avy)
  _u_: Navigate Up         _C-p_: Drag up
  _d_: Navigate Down       _C-n_: Drag down
  _n_: Navigate Next       _C-e_: Edit Cluster DWIM
  _p_: Navigate Prev       _M-k_: Kill Node DWIM
  _f_: Navigate Forward    _m_:   Mark DWIM
  _b_: Navigate Backward
  _q_: Quit
"
  ("j" combobulate-avy-jump :color blue)
  ("u" combobulate-navigate-up)
  ("d" combobulate-navigate-down)
  ("n" combobulate-navigate-next)
  ("p" combobulate-navigate-previous)
  ("f" combobulate-navigate-forward)
  ("b" combobulate-navigate-backward)
  ("m" combobulate-mark-node-dwim)
  ("C-p" combobulate-drag-up)
  ("C-n" combobulate-drag-down)
  ("C-e" combobulate-edit-cluster-dwim :color blue)
  ("M-k" combobulate-kill-node-dwim)
  ("q" nil))

(defun combobulate--query-from-node (query node)
  "Executes QUERY against NODE and returns the results"
  (tsc-query-captures (tsc-make-query tree-sitter-language query) node nil))

(defun combobulate--get-ascendant-node (node matcher match-siblings match-parents)
  (if (funcall matcher (tsc-node-type node))
      node
    (let ((sibling-node (tsc-get-prev-sibling node))
          (parent-node (tsc-get-parent node)))
      (if (and sibling-node match-siblings)
          (combobulate--get-ascendant-node sibling-node matcher match-siblings match-parents)
        (when (and parent-node match-parents)
          (combobulate--get-ascendant-node parent-node matcher match-siblings match-parents))))))

(defun combobulate--get-nearest-navigable-node ()
  "Returns the nearest navigable node to point"
  (combobulate-node-at-point combobulate-navigation-node-types))

(defun combobulate--get-parents (node)
  "Get all parent nodes of NODE"
  (reverse
   (let ((parents '()))
     (while (setq node (tsc-get-parent node))
       (push node parents))
     parents)))

(defun combobulate-get-ascendant-node-by-type (node node-types match-siblings match-parents)
  "Returns the ascendant of NODE, and siblings if MATCH-SIBLINGS is t, matching any of NODE-TYPES"
  (combobulate--get-ascendant-node
   node `(lambda (match-node) (member match-node ',node-types))
   match-siblings match-parents))

(defun combobulate--flash-node (node)
  "Flashes NODE on the screen"
  (when-let (nav-node (or (car (combobulate--nav-get-smallest-node-at-point))
                          (combobulate--get-nearest-navigable-node)))
    (when (and node combobulate-flash-node)
      (message (combobulate-render-nav-orientation nav-node)))))

(defun combobulate--move-point-to-node (node &optional end)
  "Moves the point to NODE and if END is set to the end of the node"
  (unless node
    (error "Cannot move to node"))
  (goto-char (if end (tsc-node-end-position node)
               (tsc-node-start-position node)))
  (setq combobulate--last-navigation-node node)
  (combobulate--flash-node node))

(defun combobulate--current-node (&optional node)
  "Get the current node at point"
  (let ((current-node (tree-sitter-node-at-point node)))
    (if (not (tsc-get-parent current-node))
        (save-excursion
          (skip-chars-backward "\n\t ")
          (forward-char -1)
          (tree-sitter-node-at-point))
      current-node)))

(defun combobulate--get-ascendant-nodes (start-node)
  "Returns a list of ascendant -- parent -- nodes of START-NODE"
  (and start-node
       (seq-filter (lambda (node) (member (tsc-node-type node) combobulate-navigation-node-types))
                   (combobulate--get-parents start-node))))


(defun combobulate--make-navigation-query ()
  "Generates a query that matches all navigable node types in `combobulate-navigation-node-types'"
  `([,@(mapcar 'list combobulate-navigation-node-types)] @node))


(defun combobulate--query-tree (query filter-fn)
  (when tree-sitter-tree
    (seq-filter filter-fn (mapcar 'cdr (combobulate--query-from-node query (tsc-root-node tree-sitter-tree))))))

(defun combobulate--navigate (direction &optional arg)
  "Attempt to hierarchically navigate in DIRECTION and return the new node

The DIRECTION is either directed or undirected. The former uses
the nearest _navigable_ node that `point' is in to infer where it
must go next. The undirected option, on the other hand, uses the
_nearest_ node to `point', which is almost always a node on or
immediately near `point'.

Each DIRECTION must be one of:

 `up'        which moves to the next navigable parent
 `down'      which moves to the first child
 `next'      which moves to the next sibling
 `prev'      which moves to the previous sibling
 `forward'   which moves to the end of the node point is on
 `backward'  which moves to the beginning of the node point is on

The nearest navigable node is used for the aforementioned
directions as the starting point.

There are also undirected navigational directions:

 `undirected-next'   which is the next possible navigable node ahead of point
 `undirected-up'     which is the first navigable parent from point
"
  (let ((node (combobulate--get-nearest-navigable-node))
        (node-at-point (tree-sitter-node-at-point)))
    (cond ((eq direction 'up) (combobulate--nav-get-parent node))
          ((eq direction 'down) (combobulate--nav-get-child node))
          ((eq direction 'next) (combobulate--nav-get-next-sibling node))
          ((eq direction 'previous) (combobulate--nav-get-prev-sibling node))
          ((eq direction 'forward) (combobulate--nav-forward t))
          ((eq direction 'backward) (combobulate--nav-backward t))
          ;; these ones are undirected and do not depend on the
          ;; nearest navigable node
          ((eq direction 'undirected-next) (combobulate--nav-get-child node-at-point))
          ((eq direction 'undirected-up) (combobulate--nav-get-parent node-at-point))
          (t (error "Unknown direction: %s" direction)))))

(defun combobulate-navigate-down-list-maybe ()
  "Maybe navigate down into a list or the nearest navigable node

This command mimics the existing \\[down-list] command
but with added support for navigable nodes."
  (interactive)
  (when-let (targets (seq-filter
                      ;; Filter elements that are either nil or where the location
                      ;; it wants to jump to is _behind_ `point'. Then, find the
                      ;; minimum of the remaining elements and go to that.
                      (lambda (elem) (and elem (> elem (point))))
                      (list (save-excursion
                              (ignore-errors (down-list 1 nil) (point)))
                            (combobulate-node-point (combobulate--navigate 'down))
                            (combobulate-node-point (combobulate--navigate 'undirected-next)))))
    (when-let (target (apply #'min targets))
      (goto-char target)
      (combobulate--flash-node (combobulate--get-nearest-navigable-node)))))

(defun combobulate-navigate-up-list-maybe ()
  "Maybe navigate up out of a list or to the nearest navigable node

This command mimics the existing \\[backward-up-list] command
but with added support for navigable nodes."
  (interactive)
  ;; NOTE: Roll up into `combobulate-navigate-down-list-maybe'
  (when-let (targets (seq-filter
                      (lambda (elem) (and elem (< elem (point))))
                      (list (save-excursion (ignore-errors (up-list -1 t nil) (point)))
                            (combobulate-node-point (combobulate--navigate 'undirected-up))
                            (combobulate-node-point (combobulate--navigate 'up)))))
    (when-let (target (apply #'max targets))
      (goto-char target)
      (combobulate--flash-node (combobulate--get-nearest-navigable-node)))))

(defun combobulate-navigate-forward-sexp-maybe (&optional arg)
  "Maybe move forward by sexp or ahead of the nearest navigable node

This command mimics the existing \\[forward-sexp] command but it
also attempts to handle navigable nodes, including
syntax-table-specific whitespaces and punctuation characters must
like \\[forward-sexp]."
  (interactive "^p")
  (if-let (node (combobulate--navigate 'forward))
      (combobulate--move-point-to-node node t)
    (forward-sexp)))

(defun combobulate-navigate-backward-sexp-maybe (&optional arg)
  "Maybe move backward by sexp or behind the nearest navigable node

This command mimics the existing \\[backward-sexp] command but it
also attempts to handle navigable nodes, including
syntax-table-specific whitespaces and punctuation characters must
like \\[backward-sexp]."
  (interactive "^p")
  (if-let (node (combobulate--navigate 'backward))
      (combobulate--move-point-to-node node)
    (backward-sexp)))


(defun combobulate-navigate-up (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'up)))

(defun combobulate-navigate-down (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'down)))

(defun combobulate-navigate-undirected-next (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'undirected-next)))

(defun combobulate-navigate-next (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'next)))

(defun combobulate-navigate-previous (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'previous)))

(defun combobulate-navigate-forward (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'forward) t))

(defun combobulate-navigate-backward (&optional arg)
  (interactive "^p")
  (combobulate--move-point-to-node (combobulate--navigate 'backward)))

(defun combobulate-mark-last-node ()
  "Marks the last node navigated to"
  (interactive)
  (if combobulate--last-navigation-node
      (progn
        (mark-paragraph)
        (push-mark (tsc-node-end-position combobulate--last-navigation-node) t t)
        (combobulate--move-point-to-node combobulate--last-navigation-node))
    (error "There is no recorded last node")))

(defun combobulate--query-at-point (query node-types)
  (if-let ((start-node (combobulate-node-at-point node-types)))
      (tsc-query-captures (tsc-make-query tree-sitter-language query) start-node nil)
    (error "There is no valid node at point")))

(defmacro combobulate-register (&rest combobulation)
  "Registers against LANG a COMBOBULATION."
  nil
  `(dolist (comb ',combobulation)
     (message "%s" comb)))


(defvar combobulate-setup-functions-alist
  '((python-mode . combobulate-setup-python)
    (typescript-mode . combobulate-setup-js-ts)
    (js2-mode . combobulate-setup-js-ts)
    (js-mode . combobulate-setup-js-ts)
    (html-mode . combobulate-setup-html))
  "Alist of setup functions to call when \\[combobulate-mode] is enabled.")

(defvar combobulate-options-key-map (make-sparse-keymap "Combobulate Options"))

(defvar combobulate-key-map
  (let ((map (make-sparse-keymap "Combobulate")))
    (define-key map (kbd "C-c o") combobulate-options-key-map)
    (define-key map (kbd "C-c o o") #'combobulate-menu/body)
    (define-key map (kbd "C-c o j") #'combobulate-avy-jump)
    (define-key map (kbd "C-M-u") #'combobulate-navigate-up-list-maybe)
    (define-key map (kbd "C-M-d") #'combobulate-navigate-down-list-maybe)
    (define-key map (kbd "M-a") #'combobulate-navigate-previous)
    (define-key map (kbd "M-e") #'combobulate-navigate-next)
    ;; TODO: Debatable; make these work like forward/backward-list
    (define-key map (kbd "C-M-p") #'combobulate-navigate-previous)
    (define-key map (kbd "C-M-n") #'combobulate-navigate-next)
    (define-key map (kbd "M-k") #'combobulate-kill-node-dwim)
    (define-key map (kbd "M-h") #'combobulate-mark-node-dwim)
    map))

(make-variable-buffer-local 'combobulate-options-key-map)
(make-variable-buffer-local 'combobulate-pretty-print-function)

(define-minor-mode combobulate-mode "Navigate and edit text by syntactic constructs

\\{combobulate-key-map}"
  :init-value nil :lighter "©" :keymap combobulate-key-map
  (if-let ((setup-fn (alist-get major-mode combobulate-setup-functions-alist)))
      (progn
        ;; Work around obtuse hydra behaviour
        (add-to-list 'mc/cmds-to-run-once 'combobulate-menu/combobulate-edit-cluster-dwim-and-exit)
        (funcall setup-fn)
        (tree-sitter-mode 1)
        (tree-sitter-hl-mode 1))))


;; Language overrides
(add-to-list 'tree-sitter-major-mode-language-alist '(js2-mode . tsx))
(add-to-list 'tree-sitter-major-mode-language-alist '(js-mode . tsx))
(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx))


(require 'combobulate-hierarchy)
(require 'combobulate-python)
(require 'combobulate-display)
(require 'combobulate-experiments)
(require 'combobulate-manipulation)
(require 'combobulate-html)
(require 'combobulate-navigation)
(require 'combobulate-javascript)
(provide 'combobulate)
;;; combobulate.el ends here
