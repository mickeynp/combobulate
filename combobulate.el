;;; combobulate.el --- edit and navigate text by syntactic constructs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs 29))
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
;; provided by the Emacs 29's builtin support for tree-sitter.
;;

;;; Code:

;; Requirements:
(require 'seq)
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

(defun combobulate-node-at-point (&optional node-types)
  "Return the smallest syntax node at point whose type is one of NODE-TYPES "
  (let* ((p (if combobulate-strict-node-at-point-check
                (point)
              (save-excursion
                (skip-chars-backward "\t\n ")
                (when (eolp)
                  (forward-char -1))
                (point))))
         (node (treesit-node-at p)))
    (if node-types
        (let ((this node))
          (catch 'done
            (while this
              (let ((smallest-node (combobulate-node-descendant-for-range
                                    this
                                    (combobulate-node-start this)
                                    (combobulate-node-start this))))
                (cond
                 ((member (combobulate-node-type this) node-types) (throw 'done this))
                 ((member (combobulate-node-type smallest-node) node-types) (throw 'done smallest-node))
                 (t (setq this (combobulate-node-parent this))))))))
      node)))

(defun combobulate-all-nodes-at-point (&optional backward)
  "Returns all nodes at `point' where the start position of each node is equal to point

The returned list is ordered smallest-to-largest by the node's extent."
  (let ((nodes)
        (sub-node (combobulate-node-descendant-for-range (combobulate-root-node) (if backward (1- (point)) (point)) (point))))
    (while (and sub-node (= (if backward (combobulate-node-end sub-node) (combobulate-node-start sub-node)) (point)))
      (push sub-node nodes)
      (setq sub-node (combobulate-node-parent sub-node)))
    (reverse nodes)))

(defsubst combobulate-node-range (node)
  (cons (combobulate-node-start node) (combobulate-node-end node)))

(defsubst combobulate-node-start (node)
  (treesit-node-start node))

(defsubst combobulate-node-end (node)
  (treesit-node-end node))

(defsubst combobulate-node-text (node &optional with-properties)
  (treesit-node-text node (not with-properties)))

(defsubst combobulate-node-child (node n)
  (treesit-node-child node n t))

(defsubst combobulate-node-parent (node)
  (treesit-node-parent node))

(defsubst combobulate-node-child-by-field (node field)
  (treesit-node-child-by-field-name node field))

(defsubst combobulate-node-eq (node1 node2)
  (treesit-node-eq node1 node2))

(defsubst combobulate-root-node ()
  (treesit-buffer-root-node))

(defsubst combobulate-node-descendant-for-range (node beg end &optional all)
  (treesit-node-descendant-for-range node beg end (not all)))

(defsubst combobulate-node-p (node)
  (treesit-node-p node))

(defsubst combobulate-node-type (node)
  (treesit-node-type node))

(defun combobulate-node-looking-at (node-types)
  "Returns the node point is looking at if it is one of NODE-TYPES."
  (when-let (node (combobulate-node-at-point node-types))
    (when (combobulate--point-in-node-range-p node)
      node)))

(defun combobulate-node-point (node &optional end)
  "Returns the `point' of NODE at its beginning or END

If NODE is nil, then nil is returned."
  (when node
    (if end (combobulate-node-end node)
      (combobulate-node-start node))))

(defvar combobulate--navigation-node-queries '(((try_statement "try" @match)))
  "Node types used")

(defvar-local combobulate-navigation-default-nodes nil
  "Primary node types used for navigation.

May be let-bound and set to more specialized node-types.")

(defvar-local combobulate-navigation-sexp-nodes nil
  "Node types used for sexp-based navigation")

(defvar combobulate-skip-prefix-regexp " \t\n+"
  "Skip prefix regexp used to skip past whitespace characters.")

(cl-defmacro with-navigation-nodes ((&key nodes skip-prefix backward) &rest body)
  (declare (indent 1))
  `(let ((combobulate-navigation-default-nodes (or ,nodes combobulate-navigation-default-nodes)))
     (when ,skip-prefix
       (if ,backward
           (skip-chars-backward combobulate-skip-prefix-regexp)
         (skip-chars-forward combobulate-skip-prefix-regexp)))
     ,@body))

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
  (treesit-query-capture node (treesit-query-compile (treesit-node-language node) query) nil))

(defun combobulate--get-ascendant-node (node matcher match-siblings match-parents)
  (if (funcall matcher (combobulate-node-type node))
      node
    (let ((sibling-node (combobulate-node-prev-sibling node))
          (parent-node (combobulate-node-parent node)))
      (if (and sibling-node match-siblings)
          (combobulate--get-ascendant-node sibling-node matcher match-siblings match-parents)
        (when (and parent-node match-parents)
          (combobulate--get-ascendant-node parent-node matcher match-siblings match-parents))))))

(defun combobulate--get-nearest-navigable-node ()
  "Returns the nearest navigable node to point"
  (combobulate-node-at-point combobulate-navigation-default-nodes))

(defun combobulate-get-parents (node)
  "Get all parent nodes of NODE"
  (reverse
   (let ((parents '()))
     (while (setq node (combobulate-node-parent node))
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
      (message (combobulate-draw-node-tree nav-node)))))

(defun combobulate-move-to-node (node &optional end)
  "Moves the point to NODE and if END is set to the end of the node"
  (unless node
    (error "Cannot move to node"))
  (goto-char (if end (combobulate-node-end node)
               (combobulate-node-start node)))
  (setq combobulate--last-navigation-node node)
  (combobulate--flash-node node))

(defun combobulate--current-node (&optional node)
  "Get the current node at point"
  (let ((current-node (combobulate-node-at-point node)))
    (if (not (combobulate-node-parent current-node))
        (save-excursion
          (skip-chars-backward "\n\t ")
          (forward-char -1)
          (combobulate-node-at-point))
      current-node)))

(defun combobulate--get-ascendant-nodes (start-node)
  "Returns a list of ascendant -- parent -- nodes of START-NODE"
  (and start-node
       (seq-filter (lambda (node) (member (combobulate-node-type node) combobulate-navigation-default-nodes))
                   (combobulate-get-parents start-node))))


(defun combobulate--make-navigation-query ()
  "Generates a query that matches all navigable node types in `combobulate-navigation-default-nodes'"
  `([,@(mapcar (lambda (node) (list (make-symbol node))) combobulate-navigation-default-nodes)] @node))


(defun combobulate--query-tree (query filter-fn)
  (when (combobulate-node-p (treesit-buffer-root-node))
    (seq-filter filter-fn (mapcar 'cdr (combobulate--query-from-node query (treesit-buffer-root-node))))))

(defun combobulate--navigate (direction &optional arg)
  "Attempt to hierarchically navigate in DIRECTION and return the new node

The DIRECTION is either directed or undirected. The former uses
the nearest _navigable_ node -- which, depending on the
direction, can be any of the combobulate navigation node lists --
that `point' is in to infer where it must go next. The undirected
option, on the other hand, uses the _nearest_ node to `point',
which is almost always a node on or immediately near `point'.

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
        (node-at-point (combobulate-node-at-point)))
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
                      (list (save-excursion (ignore-errors (down-list 1 nil) (point)))
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
                      (list (save-excursion (ignore-errors (backward-up-list 1 t t) (point)))
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
      (combobulate-move-to-node node t)
    (forward-sexp)))

(defun combobulate-navigate-backward-sexp-maybe (&optional arg)
  "Maybe move backward by sexp or behind the nearest navigable node

This command mimics the existing \\[backward-sexp] command but it
also attempts to handle navigable nodes, including
syntax-table-specific whitespaces and punctuation characters must
like \\[backward-sexp]."
  (interactive "^p")
  (if-let (node (combobulate--navigate 'backward))
      (combobulate-move-to-node node)
    (backward-sexp)))


(defun combobulate-navigate-up (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'up)))

(defun combobulate-navigate-down (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'down)))

(defun combobulate-navigate-undirected-next (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'undirected-next)))

(defun combobulate-navigate-next (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'next)))

(defun combobulate-navigate-previous (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'previous)))

(defun combobulate-navigate-forward (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'forward) t))

(defun combobulate-navigate-backward (&optional arg)
  (interactive "^p")
  (combobulate-move-to-node (combobulate--navigate 'backward)))

(defvar combobulate-setup-functions-alist
  '((python-mode . combobulate-setup-python)
    (python-ts-mode . combobulate-setup-python)
    (typescript-mode . combobulate-setup-js-ts)
    (typescript-ts-mode . combobulate-setup-js-ts)
    (tsx-ts-mode . combobulate-setup-js-ts)
    (js2-mode . combobulate-setup-js-ts)
    (js-mode . combobulate-setup-js-ts)
    (html-mode . combobulate-setup-html))
  "Alist of setup functions to call when \\[combobulate-mode] is enabled.

Because tree-sitter-enabled modes are different from the ordinary
ones, you may wish to customize `major-mode-remap-alist' to
silently treat the older modes as their newer TS-enabled
counterparts.")

(defvar-local combobulate-options-key-map (make-sparse-keymap "Combobulate Options"))

(defvar-local combobulate-key-map
    (let ((map (make-sparse-keymap "Combobulate")))
      (define-key map (kbd "C-c o") combobulate-options-key-map)
      (define-key map (kbd "C-c o o") #'combobulate-menu/body)
      (define-key map (kbd "C-c o j") #'combobulate-avy-jump)
      (define-key map (kbd "C-M-u") #'combobulate-navigate-up-list-maybe)
      (define-key map (kbd "C-M-d") #'combobulate-navigate-down-list-maybe)
      (define-key map (kbd "C-M-a") #'combobulate-beginning-of-defun)
      (define-key map (kbd "C-M-e") #'combobulate-end-of-defun)
      (define-key map (kbd "M-a") #'combobulate-navigate-previous)
      (define-key map (kbd "M-e") #'combobulate-navigate-next)
      ;; TODO: Debatable; make these work like forward/backward-list
      (define-key map (kbd "C-M-p") #'combobulate-navigate-previous)
      (define-key map (kbd "C-M-n") #'combobulate-navigate-next)
      (define-key map (kbd "M-k") #'combobulate-kill-node-dwim)
      (define-key map (kbd "M-h") #'combobulate-mark-node-dwim)
      map))

(make-variable-buffer-local 'forward-sexp-function)

(defvar-local combobulate-navigation-defun-nodes nil
  "Node names or queries used to navigate by defun.

See `combobulate-beginning-of-defun' and `combobulate-end-of-defun'.")

(defvar-local combobulate-navigation-default-nodes nil
  "Node names or queries used for general navigation and as a placeholder.

The macro `with-navigation-nodes' binds to this variable and
locally overrides the navigation nodes by Combobulate's node
tools.")

(defvar-local combobulate-navigation-sexp-nodes nil
  "Node names or queries used to navigate by sexp.

See `combobulate-forward-sexp-function'.")

(defvar-local combobulate-manipulation-trim-whitespace nil
  "Trim whitespace after combobulate manipulates the tree.

It can be one of the following values:

`nil' does nothing; `backward' only deletes whitespaces behind
where point is left; `all' deletes forward and backward.")

(defun combobulate-setup ()
  "Setup combobulate in the current buffer and raise an error if it is not supported.

This can be used to reinitialize mode-specific setups if they have changed."
  (interactive)
  (if-let ((setup-fn (alist-get major-mode combobulate-setup-functions-alist)))
      (progn
        (setq-local forward-sexp-function #'combobulate-forward-sexp-function)
        (funcall setup-fn))
    (error "Combobulate is not supported in this mode: `%s'. Customize
`combobulate-setup-functions-alist' to change this." major-mode)))

(define-minor-mode combobulate-mode "Navigate and edit text by syntactic constructs

\\{combobulate-key-map}"
  :init-value nil :lighter "©" :keymap combobulate-key-map
  (add-to-list 'mc/cmds-to-run-once 'combobulate-menu/combobulate-edit-cluster-dwim-and-exit)
  (combobulate-setup))


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
