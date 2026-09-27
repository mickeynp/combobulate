;;; combobulate-elixir.el --- elixir support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the tree-sitter-elixir grammar.
;;
;; In that grammar `def', `case', `if' and `Enum.map(...)' are all
;; `call' nodes, and a pipeline is a chain of nested `binary_operator'
;; nodes.  Whether a node is a function head, a clause head or a
;; pipeline stage depends on its surroundings, not on its type, so
;; the sibling and hierarchy procedures delegate to the functions
;; below through a tree-sitter `:pred' predicate.
;;
;; The keys for next, previous and down run commands that call those
;; functions directly, because the procedure queries walk the whole
;; enclosing block and get slow in large modules.  The procedures
;; still serve the rest of Combobulate.
;;
;; Defun navigation is remapped to the `treesit-*-defun' commands,
;; which use the major mode's `treesit-defun-type-regexp' predicate.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(declare-function combobulate-heex-navigate-next-same-kind "combobulate-heex")
(declare-function combobulate-heex-navigate-previous-same-kind "combobulate-heex")

(defgroup combobulate-elixir nil
  "Configuration switches for Elixir"
  :group 'combobulate
  :prefix "combobulate-elixir-")

(defconst combobulate-elixir--wrappers
  '("source" "do_block" "else_block" "rescue_block" "catch_block" "after_block"
    "body" "block" "arguments" "keywords" "map_content")
  "Node types that only group other nodes.")

(defconst combobulate-elixir--sequences
  '("arguments" "list" "tuple" "map_content" "keywords" "bitstring")
  "Node types whose children are comma-separated elements.")

(defconst combobulate-elixir--blocks
  '("source" "do_block" "else_block" "rescue_block" "catch_block" "after_block"
    "body" "block" "anonymous_function")
  "Node types whose children are statements or clauses.")

(defun combobulate-elixir--type-p (node types)
  (and node (member (treesit-node-type node) types)))

(defun combobulate-elixir--child-of-type (node type)
  (seq-find (lambda (child) (equal (treesit-node-type child) type))
            (treesit-node-children node t)))

(defun combobulate-elixir--do-block (node)
  "Return the `do_block' of NODE if NODE is a call that has one."
  (and (equal (treesit-node-type node) "call")
       (combobulate-elixir--child-of-type node "do_block")))

(defun combobulate-elixir--pipe-p (node)
  (and (equal (treesit-node-type node) "binary_operator")
       (equal (treesit-node-text (treesit-node-child-by-field-name node "operator") t) "|>")))

(defun combobulate-elixir--pipe-stages (node)
  "Return the head and stages of the pipeline that the `|>' NODE belongs to."
  (while (combobulate-elixir--pipe-p (treesit-node-parent node))
    (setq node (treesit-node-parent node)))
  (let ((stages))
    (while (combobulate-elixir--pipe-p node)
      (push (treesit-node-child-by-field-name node "right") stages)
      (setq node (treesit-node-child-by-field-name node "left")))
    (cons node stages)))

(defun combobulate-elixir--elements (node)
  "Return the named children of NODE, with `keywords' replaced by its pairs.

Comments are left out because Combobulate never navigates to them."
  (mapcan (lambda (child)
            (pcase (treesit-node-type child)
              ("keywords" (treesit-node-children child t))
              ("comment" nil)
              (_ (list child))))
          (treesit-node-children node t)))

(defun combobulate-elixir--do-keyword-p (arguments)
  "Return non-nil if ARGUMENTS has a `do:' pair, as in `def f, do: x'."
  (let ((keywords (combobulate-elixir--child-of-type arguments "keywords")))
    (and keywords
         (seq-find (lambda (pair)
                     (equal (string-trim (treesit-node-text
                                          (treesit-node-child-by-field-name pair "key") t))
                            "do:"))
                   (treesit-node-children keywords t)))))

(defun combobulate-elixir--head-p (arguments)
  "Return non-nil if ARGUMENTS is the head of a clause or of a `do' block call.

A call written with a `do:' keyword counts as having a `do' block.
`with' and `for' are excluded because their heads hold clauses that
are worth navigating between."
  (let ((owner (treesit-node-parent arguments)))
    (or (equal (treesit-node-type owner) "stab_clause")
        (and (equal (treesit-node-type owner) "call")
             (or (combobulate-elixir--do-block owner)
                 (combobulate-elixir--do-keyword-p arguments))
             (not (member (treesit-node-text (treesit-node-child-by-field-name owner "target") t)
                          '("with" "for")))))))

(defun combobulate-elixir--point-at (pos)
  "Return POS, or the line's first non-blank position if POS is in indentation."
  (save-excursion
    (goto-char pos)
    (when (looking-back "^[ \t]*" (line-beginning-position))
      (skip-chars-forward " \t"))
    (point)))

(defun combobulate-elixir--node-at (pos)
  "Return the smallest named node at POS, skipping leading indentation."
  (let ((pos (combobulate-elixir--point-at pos)))
    (treesit-node-descendant-for-range (treesit-buffer-root-node 'elixir) pos pos t)))

(defun combobulate-elixir--outermost-at (node)
  "Return the largest node that starts where NODE starts and is not a wrapper."
  (let ((start (treesit-node-start node))
        (best (unless (combobulate-elixir--type-p node combobulate-elixir--wrappers) node)))
    (while (and (setq node (treesit-node-parent node))
                (= (treesit-node-start node) start))
      (unless (combobulate-elixir--type-p node combobulate-elixir--wrappers)
        (setq best node)))
    best))

(defun combobulate-elixir--siblings (pos)
  "Return the nodes that are siblings of the node at POS."
  (let* ((at (combobulate-elixir--node-at pos))
         (node (or (combobulate-elixir--outermost-at at) at))
         (parent))
    (catch 'done
      (while (setq parent (treesit-node-parent node))
        (cond
         ((combobulate-elixir--pipe-p parent)
          (throw 'done (combobulate-elixir--pipe-stages parent)))
         ((and (equal (treesit-node-type parent) "arguments")
               (combobulate-elixir--head-p parent))
          (setq node (treesit-node-parent parent)))
         ((equal (treesit-node-type parent) "keywords")
          (throw 'done (combobulate-elixir--elements (treesit-node-parent parent))))
         ((combobulate-elixir--type-p parent (append combobulate-elixir--sequences
                                                     combobulate-elixir--blocks))
          (throw 'done (combobulate-elixir--elements parent)))
         (t (setq node parent)))))))

(defun combobulate-elixir--thing-at (pos)
  "Return the node at POS, grown from a call's target to the whole call."
  (let ((node (combobulate-elixir--node-at pos))
        (parent))
    (while (and (setq parent (treesit-node-parent node))
                (pcase (treesit-node-type parent)
                  ((or "call" "access_call")
                   (treesit-node-eq node (treesit-node-child-by-field-name parent "target")))
                  ((or "dot" "unary_operator") t)))
      (setq node parent))
    node))

(defun combobulate-elixir--inside (node)
  "Return the nodes directly inside NODE that navigating down can land on."
  (pcase (treesit-node-type node)
    ("call"
     (let ((container (or (combobulate-elixir--do-block node)
                          (combobulate-elixir--child-of-type node "arguments"))))
       (and container (combobulate-elixir--elements container))))
    ("stab_clause"
     (let ((body (treesit-node-child-by-field-name node "right")))
       (and body (combobulate-elixir--elements body))))
    ("binary_operator"
     (if (combobulate-elixir--pipe-p node)
         (combobulate-elixir--pipe-stages node)
       (list (treesit-node-child-by-field-name node "right"))))
    ("unary_operator"
     (let ((operand (treesit-node-child-by-field-name node "operand")))
       (if (equal (treesit-node-type operand) "call")
           (combobulate-elixir--inside operand)
         (list operand))))
    ("pair" (list (treesit-node-child-by-field-name node "value")))
    ("map"
     (let ((content (combobulate-elixir--child-of-type node "map_content")))
       (and content (combobulate-elixir--elements content))))
    ((or "string" "charlist" "sigil" "quoted_atom" "quoted_keyword") nil)
    (_ (combobulate-elixir--elements node))))

(defun combobulate-elixir--down-target (pos)
  "Return the node that navigating down from POS lands on."
  (let* ((pos (combobulate-elixir--point-at pos))
         (at (combobulate-elixir--node-at pos))
         (thing (combobulate-elixir--thing-at pos)))
    (cl-flet ((first-after (nodes)
                (seq-find (lambda (n) (> (treesit-node-start n) pos)) nodes)))
      (or (first-after (combobulate-elixir--inside thing))
          ;; On a leaf that starts an assignment, pipeline or clause, enter that instead.
          (and (= (treesit-node-start at) pos)
               (let ((outermost (combobulate-elixir--outermost-at at)))
                 (and outermost (first-after (combobulate-elixir--inside outermost)))))
          ;; On a leaf in a function or clause head, enter the body the head introduces.
          (let ((node thing) (target))
            (while (and (not target) (setq node (treesit-node-parent node)))
              (let ((body (pcase (treesit-node-type node)
                            ("call" (combobulate-elixir--do-block node))
                            ("stab_clause" (treesit-node-child-by-field-name node "right")))))
                (when (and body (< pos (treesit-node-start body)))
                  (setq target (first-after (combobulate-elixir--elements body))))))
            target)))))

(defvar-local combobulate-elixir--cache nil
  "The last navigation result, as (KIND POINT TICK RESULT STARTS).

STARTS is a hash table of the start positions of the nodes in
RESULT, so the `:pred' predicates can reject most nodes cheaply.")

(defun combobulate-elixir--cached (kind fn)
  "Return FN applied to point and a table of its start positions.

The `:pred' predicates run once per candidate node, so the answer is
computed once and reused while point and the buffer are unchanged."
  (pcase-let ((`(,k ,pt ,tick . ,rest) combobulate-elixir--cache))
    (if (and (eq k kind) (eql pt (point)) (eql tick (buffer-chars-modified-tick)))
        rest
      (let* ((result (funcall fn (point)))
             (starts (make-hash-table)))
        (dolist (node (ensure-list result))
          (puthash (treesit-node-start node) t starts))
        (setq combobulate-elixir--cache
              (list kind (point) (buffer-chars-modified-tick) result starts))
        (list result starts)))))

(defun combobulate-elixir--sibling-p (node)
  (pcase-let ((`(,siblings ,starts) (combobulate-elixir--cached 'sibling #'combobulate-elixir--siblings)))
    (and (gethash (treesit-node-start node) starts)
         (seq-find (lambda (sibling) (treesit-node-eq sibling node)) siblings))))

(defun combobulate-elixir--down-p (node)
  "Match the down target, or, when there is none, the nodes around point.

Matching the nodes around point when there is no target stops the
procedure from retrying the query on every ancestor; navigation then
ignores them because they do not start after point."
  (pcase-let ((`(,target ,starts) (combobulate-elixir--cached 'down #'combobulate-elixir--down-target)))
    (if target
        (and (gethash (treesit-node-start node) starts)
             (treesit-node-eq target node))
      (<= (treesit-node-start node) (point) (treesit-node-end node)))))

(defun combobulate-elixir--grows-p (node parent backward)
  "Return non-nil if the sexp NODE extends to PARENT, which shares its edge."
  (let ((field (treesit-node-field-name node)))
    (pcase (treesit-node-type parent)
      ("call" (or (equal field "target")
                  (and backward
                       (or (equal (treesit-node-type node) "do_block")
                           (and (equal (treesit-node-type node) "arguments")
                                (equal (treesit-node-type (treesit-node-child node 0)) "("))))))
      ("access_call" (or backward (equal field "target")))
      ("dot" t)
      ("unary_operator" backward)
      ("arguments" (and (not backward)
                        (equal (treesit-node-type (treesit-node-parent parent)) "stab_clause")))
      ("stab_clause" (and (not backward) (equal field "left"))))))

(defun combobulate-elixir--sexp-at (pos backward)
  "Return the expression that starts at POS, or ends at POS if BACKWARD."
  (let* ((root (treesit-buffer-root-node 'elixir))
         (node (if backward
                   (and (> pos (point-min))
                        (treesit-node-descendant-for-range root (1- pos) pos t))
                 (treesit-node-descendant-for-range root pos pos t)))
         (edge (lambda (n) (if backward (treesit-node-end n) (treesit-node-start n))))
         (parent))
    (when (and node (= (funcall edge node) pos))
      (while (and (setq parent (treesit-node-parent node))
                  (= (funcall edge parent) pos)
                  (combobulate-elixir--grows-p node parent backward))
        (setq node parent))
      node)))

(defun combobulate-elixir--in-heex-p ()
  "Return non-nil if point is inside a `~H' sigil."
  ;; Emacs only sets the HEEx parser's ranges on redisplay.
  (treesit-update-ranges (point) (min (point-max) (1+ (point))))
  (eq (treesit-language-at (point)) 'heex))

(defun combobulate-elixir-forward-sexp (&optional arg)
  "Move forward over ARG Elixir expressions, or backward if ARG is negative.

From `def' this moves over the whole definition, and from
`Keyword.get' over the whole call.  Where no expression starts,
fall back to `forward-sexp-default-function'.  Inside a `~H' sigil,
use Combobulate's HEEx navigation."
  (setq arg (or arg 1))
  (if (combobulate-elixir--in-heex-p)
      (combobulate-forward-sexp-function arg)
    (let ((backward (< arg 0)))
      (dotimes (_ (abs arg))
        (forward-comment (if backward (- (buffer-size)) (buffer-size)))
        (let ((node (combobulate-elixir--sexp-at (point) backward)))
          (if node
              (goto-char (if backward (treesit-node-start node) (treesit-node-end node)))
            (forward-sexp-default-function (if backward -1 1))))))))

(defun combobulate-elixir--skip-indentation ()
  "Move to the line's first node when point is in indentation.

Combobulate otherwise resolves indentation to the enclosing block."
  (when (looking-back "^[ \t]*" (line-beginning-position))
    (skip-chars-forward " \t")))

(defun combobulate-elixir--anchor ()
  "Return the start of the smallest non-wrapper node at point.

This is the position Combobulate's sibling navigation works from.
`else', `rescue', `catch' and `after' blocks count as nodes here
because they are siblings of the statements before them."
  (let ((node (combobulate-elixir--node-at (point))))
    (while (and node (combobulate-elixir--type-p
                      node '("source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content")))
      (setq node (treesit-node-parent node)))
    (if node (treesit-node-start node) (point))))

(defun combobulate-elixir--navigate (arg fallback find)
  "Move ARG times to the node FIND returns, or run FALLBACK inside HEEx.

The commands below compute their targets directly instead of going
through the procedure queries, which walk the whole enclosing block
and get slow in large modules."
  (combobulate-elixir--skip-indentation)
  (if (combobulate-elixir--in-heex-p)
      (funcall fallback arg)
    (dotimes (_ (or arg 1))
      (combobulate-visual-move-to-node (funcall find)))))

(defun combobulate-elixir-navigate-next (&optional arg)
  "Move to the next sibling ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-navigate-next
   (lambda ()
     (skip-chars-forward combobulate-skip-prefix-regexp)
     (let ((anchor (combobulate-elixir--anchor)))
       (seq-find (lambda (node) (> (treesit-node-start node) anchor))
                 (combobulate-elixir--siblings anchor))))))

(defun combobulate-elixir-navigate-previous (&optional arg)
  "Move to the previous sibling ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-navigate-previous
   (lambda ()
     (combobulate-elixir--skip-indentation)
     (let ((anchor (combobulate-elixir--anchor)))
       (car (last (seq-filter (lambda (node) (< (treesit-node-start node) anchor))
                              (combobulate-elixir--siblings anchor))))))))

(defun combobulate-elixir--kind (node)
  "Return a string naming the kind of NODE for same-kind navigation.

Calls are grouped by keyword, with private forms such as `defp'
counting as their public form.  Module attributes are grouped by
name and binary operators by operator."
  (let ((field-text (lambda (n field)
                      (treesit-node-text (treesit-node-child-by-field-name n field) t))))
    (pcase (treesit-node-type node)
      ("call"
       (let ((name (funcall field-text node "target")))
         (if (string-match (rx bos (group "def" (* alpha)) "p" eos) name)
             (match-string 1 name)
           name)))
      ("unary_operator"
       (let ((operand (treesit-node-child-by-field-name node "operand")))
         (concat (funcall field-text node "operator")
                 (if (equal (treesit-node-type operand) "call")
                     (funcall field-text operand "target")
                   (treesit-node-text operand t)))))
      ("binary_operator" (concat "binary_operator " (funcall field-text node "operator")))
      (type type))))

(defun combobulate-elixir--same-kind-target (direction)
  "Return the nearest sibling in DIRECTION of the same kind as the one at point."
  (let* ((anchor (combobulate-elixir--anchor))
         (siblings (combobulate-elixir--siblings anchor))
         (current (seq-find (lambda (node)
                              (and (<= (treesit-node-start node) anchor)
                                   (< anchor (treesit-node-end node))))
                            siblings))
         (kind (and current (combobulate-elixir--kind current)))
         (same (seq-filter (lambda (node) (equal (combobulate-elixir--kind node) kind))
                           siblings)))
    (when kind
      (if (eq direction 'next)
          (seq-find (lambda (node) (> (treesit-node-start node) anchor)) same)
        (car (last (seq-filter (lambda (node) (< (treesit-node-start node) anchor)) same)))))))

(defun combobulate-elixir-navigate-next-same-kind (&optional arg)
  "Move to the next sibling of the same kind ARG times.

From `def' this skips `@doc', `@spec' and other statements to reach
the next `def' or `defp'; from `@doc' it reaches the next `@doc'."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-heex-navigate-next-same-kind
   (lambda ()
     (skip-chars-forward combobulate-skip-prefix-regexp)
     (combobulate-elixir--same-kind-target 'next))))

(defun combobulate-elixir-navigate-previous-same-kind (&optional arg)
  "Move to the previous sibling of the same kind ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-heex-navigate-previous-same-kind
   (lambda () (combobulate-elixir--same-kind-target 'previous))))

(defun combobulate-elixir--trimmed-range (node)
  "Return the range of NODE without trailing whitespace.

The grammar ends the last clause of a `case' or `fn' after the
newline before `end', so swapping untrimmed ranges moves that
newline."
  (save-excursion
    (goto-char (treesit-node-end node))
    (skip-chars-backward " \t\n" (treesit-node-start node))
    (cons (treesit-node-start node) (point))))

(defun combobulate-elixir--drag (direction)
  "Swap the sibling at point with its neighbour in DIRECTION.

Return the position where the sibling at point now starts."
  (let* ((anchor (combobulate-elixir--anchor))
         (siblings (combobulate-elixir--siblings anchor))
         (self (seq-find (lambda (node)
                           (and (<= (treesit-node-start node) anchor)
                                (< anchor (treesit-node-end node))))
                         siblings))
         (other (and self
                     (if (eq direction 'up)
                         (car (last (seq-filter (lambda (node) (< (treesit-node-start node)
                                                                  (treesit-node-start self)))
                                                siblings)))
                       (seq-find (lambda (node) (> (treesit-node-start node) (treesit-node-start self)))
                                 siblings)))))
    (unless self
      (user-error "Nothing to drag at point"))
    (unless other
      (user-error "No sibling to swap with in that direction"))
    (when (xor (equal (treesit-node-type self) "pair") (equal (treesit-node-type other) "pair"))
      (user-error "Keyword pairs must stay after the other elements"))
    (pcase-let* ((`(,first ,second) (if (eq direction 'up) (list other self) (list self other)))
                 (first-range (combobulate-elixir--trimmed-range first))
                 (second-range (combobulate-elixir--trimmed-range second))
                 (self-length (- (cdr (combobulate-elixir--trimmed-range self))
                                 (treesit-node-start self))))
      (transpose-subr-1 first-range second-range)
      (if (eq direction 'up)
          (car first-range)
        (- (cdr second-range) self-length)))))

(defun combobulate-elixir-drag-up (&optional arg)
  "Swap the sibling at point with the previous one ARG times.

Uses the same siblings as \\[combobulate-elixir-navigate-previous]."
  (interactive "^p")
  (combobulate-elixir--drag-command arg 'up #'combobulate-drag-up))

(defun combobulate-elixir-drag-down (&optional arg)
  "Swap the sibling at point with the next one ARG times.

Uses the same siblings as \\[combobulate-elixir-navigate-next]."
  (interactive "^p")
  (combobulate-elixir--drag-command arg 'down #'combobulate-drag-down))

(defun combobulate-elixir--drag-command (arg direction fallback)
  (combobulate-elixir--skip-indentation)
  (if (combobulate-elixir--in-heex-p)
      (funcall fallback arg)
    (dotimes (_ (or arg 1))
      (let ((start (combobulate-elixir--drag direction)))
        (combobulate-visual-move-to-node
         (combobulate-elixir--outermost-at (combobulate-elixir--node-at start)))))))

(defun combobulate-elixir-kill-node-dwim (&optional arg)
  "Like `combobulate-kill-node-dwim', but keep the node's trailing whitespace.

The last clause of a `case' or `fn' includes the newline before
`end', and killing it would pull `end' onto the previous line."
  (interactive "p")
  (if (combobulate-elixir--in-heex-p)
      (combobulate-kill-node-dwim arg)
    (dotimes (_ (or arg 1))
      (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
        (when-let* ((nearest (save-excursion
                               (combobulate-skip-whitespace-forward t)
                               (combobulate--get-nearest-navigable-node)))
                    (node (or (combobulate-nav-get-self-sibling nearest) nearest))
                    (range (combobulate-elixir--trimmed-range node))
                    (proxy (combobulate-proxy-node-make-from-range (car range) (cdr range))))
          (unless (combobulate-node-on-or-after-point-p proxy)
            (error "No node to kill"))
          (let ((text (combobulate--consume-node proxy t)))
            (if (memq last-command '(combobulate-kill-node-dwim combobulate-elixir-kill-node-dwim))
                (kill-append text nil)
              (kill-new text)))
          (combobulate-message "Killed node" proxy))))))

(defconst combobulate-elixir--unsplicable
  '("do_block" "else_block" "rescue_block" "catch_block" "after_block"
    "body" "stab_clause" "keywords" "map_content")
  "Node types that splicing must not replace.

Replacing a `do' block with its contents drops `do' and `end', and
replacing a clause drops its `->', which leaves invalid code.")

(defun combobulate-elixir--splice (command arg)
  "Run the splice COMMAND with ARG, offering only choices that keep valid code."
  (combobulate-elixir--skip-indentation)
  (let* ((anchor (combobulate-elixir--anchor))
         (current (seq-find (lambda (node)
                              (and (<= (treesit-node-start node) anchor)
                                   (< anchor (treesit-node-end node))))
                            (combobulate-elixir--siblings anchor))))
    (when (equal (treesit-node-type current) "stab_clause")
      (user-error "Clauses cannot live outside their `case', `cond' or `fn'")))
  (let ((proffer (symbol-function 'combobulate-proffer-choices)))
    (cl-letf (((symbol-function 'combobulate-proffer-choices)
               (lambda (nodes &rest args)
                 (apply proffer
                        (or (seq-remove (lambda (node)
                                          (combobulate-elixir--type-p node combobulate-elixir--unsplicable))
                                        nodes)
                            (user-error "Nothing to splice here without breaking the code"))
                        args))))
      (funcall command arg))))

(defun combobulate-elixir-splice-up (&optional arg)
  "Like `combobulate-splice-up', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-up arg))

(defun combobulate-elixir-splice-down (&optional arg)
  "Like `combobulate-splice-down', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-down arg))

(defun combobulate-elixir-splice-self (&optional arg)
  "Like `combobulate-splice-self', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-self arg))

(defun combobulate-elixir-splice-parent (&optional arg)
  "Like `combobulate-splice-parent', without choices that break the code."
  (interactive "^p")
  (combobulate-elixir--splice #'combobulate-splice-parent arg))

(defun combobulate-elixir-navigate-up (&optional arg)
  "Like `combobulate-navigate-up', but skipping leading indentation."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (combobulate-navigate-up arg))

(defun combobulate-elixir-navigate-down (&optional arg)
  "Move into the node at point ARG times."
  (interactive "^p")
  (combobulate-elixir--navigate
   arg #'combobulate-navigate-down
   (lambda ()
     (combobulate-elixir--skip-indentation)
     (combobulate-elixir--down-target (point)))))

(defun combobulate-elixir--keywords (node)
  "Return the start positions of the keywords that delimit NODE.

For a call with a `do' block these are the call's target, `do',
any `else', `rescue', `catch' or `after', and `end'.  For an
anonymous function they are `fn' and `end'."
  (let ((keywords (lambda (parent)
                    (seq-keep (lambda (child)
                                (and (member (treesit-node-type child)
                                             '("do" "else" "rescue" "catch" "after" "fn" "end"))
                                     (treesit-node-start child)))
                              (treesit-node-children parent)))))
    (pcase (treesit-node-type node)
      ("call"
       (let ((do-block (combobulate-elixir--do-block node)))
         (and do-block
              (append (list (treesit-node-start node))
                      (mapcan (lambda (child)
                                (if (member (treesit-node-type child)
                                            '("else_block" "rescue_block" "catch_block" "after_block"))
                                    (funcall keywords child)
                                  (and (member (treesit-node-type child) '("do" "end"))
                                       (list (treesit-node-start child)))))
                              (treesit-node-children do-block))))))
      ("anonymous_function" (funcall keywords node)))))

(defun combobulate-elixir--sequence-target (direction)
  "Return the next keyword position in DIRECTION among the constructs around point."
  (let ((node (treesit-node-at (point) 'elixir))
        (target))
    (while (and node (not target))
      (let ((positions (combobulate-elixir--keywords node)))
        (setq target (if (eq direction 'next)
                         (seq-find (lambda (pos) (> pos (point))) positions)
                       (car (last (seq-filter (lambda (pos) (< pos (point))) positions))))))
      (setq node (treesit-node-parent node)))
    target))

(defun combobulate-elixir-navigate-sequence-next (&optional arg)
  "Move to the next keyword of the construct at point ARG times.

From `def' this visits `do', then `end'; from `with' also `else'.
Outside any construct, fall back to `combobulate-navigate-sequence-next'."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (dotimes (_ (or arg 1))
    (let ((target (and (not (combobulate-elixir--in-heex-p))
                       (combobulate-elixir--sequence-target 'next))))
      (if target
          (goto-char target)
        (setq this-command 'combobulate-navigate-sequence-next)
        (combobulate-navigate-sequence-next)))))

(defun combobulate-elixir-navigate-sequence-previous (&optional arg)
  "Move to the previous keyword of the construct at point ARG times.

Outside any construct, fall back to `combobulate-navigate-sequence-previous'."
  (interactive "^p")
  (combobulate-elixir--skip-indentation)
  (dotimes (_ (or arg 1))
    (let ((target (and (not (combobulate-elixir--in-heex-p))
                       (combobulate-elixir--sequence-target 'previous))))
      (if target
          (goto-char target)
        (setq this-command 'combobulate-navigate-sequence-previous)
        (combobulate-navigate-sequence-previous)))))

(defun combobulate-elixir-pretty-print-node-name (node _default-name)
  "Pretty printer for Elixir nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("call"
       (let ((target (combobulate-node-child-by-field node "target"))
             (arguments (combobulate-node-child node 1)))
         (if (and arguments (equal (combobulate-node-type arguments) "arguments"))
             (concat (combobulate-node-text target) " "
                     (combobulate-node-text (combobulate-node-child arguments 0)))
           (combobulate-node-text target))))
      ("stab_clause"
       (concat (combobulate-node-text (combobulate-node-child-by-field node "left")) " ->"))
      (_ (car (split-string (combobulate-node-text node) "\n")))))
   40))

(eval-and-compile
  (defvar combobulate-elixir-definitions
    '((context-nodes
       '("identifier" "alias" "atom" "keyword"))
      (plausible-separators '("," "\n"))
      (pretty-print-node-name-function #'combobulate-elixir-pretty-print-node-name)
      (navigate-down-into-lists nil)
      (procedures-sibling
       '(;; Statements, definitions and clauses when point is at their start.
         (:activation-nodes
          ((:nodes ((exclude (all) "source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content"))
                   :position at
                   :has-parent ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                "after_block" "body" "block" "anonymous_function")))
          :selector (:choose parent :match-children t))
         ;; Elements of argument lists and collections.  Plain children
         ;; carry the `@match' marks that splicing and dragging expect.
         (:activation-nodes
          ((:nodes ((exclude (all) "source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content"))
                   :has-parent ("arguments" "list" "tuple" "map_content" "keywords" "bitstring")))
          :selector (:choose parent :match-children t))
         ;; Everything else, including heads, pipelines and keyword lists.
         ;; The query runs on the nearest block so it stays cheap.
         (:activation-nodes
          ((:nodes ((exclude (all) "source" "do_block" "body" "block" "arguments"
                             "keywords" "map_content"))
                   :has-ancestor ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                  "after_block" "body" "block" "anonymous_function")))
          :selector (:choose parent
                             :match-query
                             (:query (((_) @match (:pred combobulate-elixir--sibling-p @match)))
                                     :engine treesitter)))))
      (procedures-hierarchy
       '(;; The target is usually inside the node at point, so query that first.
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :position at))
          :selector (:choose node
                             :match-query
                             (:query (((_) @match (:pred combobulate-elixir--down-p @match)))
                                     :engine treesitter)))
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :has-ancestor ("source" "do_block" "else_block" "rescue_block" "catch_block"
                                  "after_block" "body" "block" "anonymous_function")))
          :selector (:choose parent
                             :match-query
                             (:query (((_) @match (:pred combobulate-elixir--down-p @match)))
                                     :engine treesitter)))
         ;; Lists the node types that navigating up may stop at.
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name elixir
 :major-modes (elixir-ts-mode)
 :custom combobulate-elixir-definitions
 :setup-fn combobulate-elixir-setup)

(defun combobulate-elixir-setup (_)
  (setq-local forward-sexp-function #'combobulate-elixir-forward-sexp)
  (let ((map (combobulate-read map)))
    (define-key map [remap combobulate-navigate-beginning-of-defun] #'treesit-beginning-of-defun)
    (define-key map [remap combobulate-navigate-end-of-defun] #'treesit-end-of-defun)
    (define-key map [remap combobulate-mark-defun] #'mark-defun)
    (define-key map [remap combobulate-navigate-next] #'combobulate-elixir-navigate-next)
    (define-key map [remap combobulate-navigate-previous] #'combobulate-elixir-navigate-previous)
    (define-key map [remap combobulate-navigate-up] #'combobulate-elixir-navigate-up)
    (define-key map [remap combobulate-navigate-down] #'combobulate-elixir-navigate-down)
    (define-key map [remap combobulate-navigate-sequence-next] #'combobulate-elixir-navigate-sequence-next)
    (define-key map [remap combobulate-navigate-sequence-previous]
                #'combobulate-elixir-navigate-sequence-previous)
    (define-key map [remap combobulate-drag-up] #'combobulate-elixir-drag-up)
    (define-key map [remap combobulate-drag-down] #'combobulate-elixir-drag-down)
    (define-key map [remap combobulate-kill-node-dwim] #'combobulate-elixir-kill-node-dwim)
    (define-key map [remap combobulate-splice-up] #'combobulate-elixir-splice-up)
    (define-key map [remap combobulate-splice-down] #'combobulate-elixir-splice-down)
    (define-key map [remap combobulate-splice-self] #'combobulate-elixir-splice-self)
    (define-key map [remap combobulate-splice-parent] #'combobulate-elixir-splice-parent)))

(provide 'combobulate-elixir)
;;; combobulate-elixir.el ends here
