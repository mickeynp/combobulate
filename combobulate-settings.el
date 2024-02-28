;;; combobulate-settings.el --- settings for combobulate  -*- lexical-binding: t; -*-

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

(require 'diff-mode)

(declare-function combobulate-baseline-indentation-default "combobulate-manipulation")

(defgroup combobulate nil
  "Structured Editing and Movement with Combobulate"
  :group 'languages
  :prefix "combobulate-")

(defgroup combobulate-envelope nil
  "Settings for Combobulate's code generation templates."
  :group 'combobulate
  :prefix "combobulate-envelope-")

(defgroup combobulate-faces nil
  "Faces used by Combobulate."
  :group 'combobulate)

(defcustom combobulate-mark-node-or-thing-at-point 'symbol
  "Maybe mark the thing at point first before extending the mark to the first node.

Can be anything `thing-at-point' supports, theoretically, though
practically speaking the most useful ones are `symbol' and `word'
for all but the most esoteric requirements."
  :group 'combobulate
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Symbol" symbol)
          (const :tag "Word" word)))

(defcustom combobulate-proffer-default-to-command 'done
  "What should happen in a proffer prompt when an unknown key is pressed.

When set to `done' the proffer UI will accept the proffered
choice and re-insert the key you just typed into the event
loop. That means you can seamlessly type from a proffer prompt
and not lose key strokes. This is the default behavior.

If it is set to `cancel' then the selection is aborted as though
you'd pressed `C-g'."
  :group 'combobulate
  :type '(choice
          (const :tag "Accept and Commit" done)
          (const :tag "Cancel" cancel)))

(defcustom combobulate-proffer-allow-numeric-selection t
  "Allow numeric selection in proffer prompts that support numeric selection.

If non-nil, you can type a number to select a choice in a proffer
prompt. This is useful if you have a lot of choices and want to
quickly select one without having to type the whole thing out.

If nil, then numeric selection is disabled."
  :group 'combobulate
  :type 'boolean)


(defcustom combobulate-flash-node t
  "Display a tree outline of nodes near point if non-nil."
  :group 'combobulate
  :type 'boolean)

(defcustom combobulate-beginning-of-defun-behavior 'parent
  "Control the behavior of the beginning of defun command.

Use `parent' and Combobulate will look at the nearest defun to
which it belongs and only visit its immediate parent.

Use `root' and Combobulate will instead attempt to find the defun
most closes to the root of the tree -- i.e., a defun that itself
has no defun parents.

Use `self-and-sibling-first' and Combobulate will instead first go to
the beginning of defun point is in, before cycling through
sibling defuns of the point's defun before moving up to its
parent, and so on, until it reaches the root.

Use `linear' and Combobulate will simply move to the first defun
it finds, regardless of hierarchy."
  :group 'combobulate
  :type '(choice
          (const :tag "Parent" parent)
          (const :tag "Root" root)
          (const :tag "Self and Sibling First" self-and-sibling-first)
          (const :tag "Linear" linear)))

(defcustom combobulate-navigate-next-move-to-end nil
  "Make \\[combobulate-navigate-next] move to the end of the node.

If non-nil, the point is placed at the end of the next sibling
node. This is the default behavior in major modes -- like elisp
mode -- that properly places the point at the end of the sibling
node you navigated to.

However, this can result in placing the point at the end of a
node that is also technically inside the parent of another, due
to how node ranges can overlap.

Setting this to nil always places the point at the beginning of
the node."
  :type 'boolean
  :group 'combobulate)

(defcustom combobulate-pulse-node-wait-time 0.5
  "How long to wait (in seconds) at the pulsed node before returning."
  :type 'float
  :group 'combobulate)

(defcustom combobulate-pulse-node t
  "If non-nil, Combobulate will pulse important nodes."
  :type 'boolean
  :group 'combobulate)

(defcustom combobulate-proffer-indicators "○●"
  "Indicator symbols used in Combobulate's proffer prompts."
  :type 'string
  :group 'combobulate)

(defcustom combobulate-key-prefix "C-c o"
  "Prefix key for Combobulate commands. Leave blank to disable.

This is the prefix key for all Combobulate commands. It is
recommended that you set this to something that is easy to type
and remember, but that does not conflict with any other key.

When you change this key prefix you must restart Emacs for the
change to take effect.

Note this must be a string readable by the `kbd' macro, and not a
vector or an escaped string."
  :type 'string
  :group 'combobulate)


(defface combobulate-refactor-highlight-face '((((background light))
                                                :background "gray80")
                                               (((background dark))
                                                :background "gray20")
                                               (t :inherit secondary-selection))
  "Face for text that is important or relevant to a refactoring operation."
  :group 'combobulate-faces)

(defface combobulate-refactor-label-face '((t
                                            :background "DarkOrchid4"
                                            :foreground "thistle1"))
  "Face for the overlay label for a range during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-refactor-field-face '((t (:background "MediumPurple4" :foreground "MediumPurple1")))
  "Face for prompts and fields during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-refactor-inactive-field-face '((t (:background "#342261" :foreground "#6e4bc0")))
  "Face for prompts and fields during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-refactor-choice-face '((t (:background "aquamarine4" :foreground "aquamarine1")))
  "Face for choices during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-refactor-inactive-choice-face '((t (:background "azure4" :foreground "azure1")))
  "Face for choices during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-refactor-cursor-face '((t (:background "VioletRed4" :foreground "VioletRed1")))
  "Face for fake cursors during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-dimmed-indicator-face '((t (:foreground "slate gray")))
  "Face for dimmed indicators, like the indentation display."
  :group 'combobulate-faces)

(defface combobulate-active-indicator-face '((t (:foreground "dodger blue")))
  "Face for active indicators, like the indentation display."
  :group 'combobulate-faces)

(defface combobulate-error-indicator-face '((t
                                             (:box
                                              (:line-width
                                               (1 . 1)
                                               :color "FireBrick2" :style flat-button)
                                              :foreground "FireBrick1" :background "FireBrick4")))
  "Face for error indicators, like the indentation display."
  :group 'combobulate-faces)

(defface combobulate-tree-branch-face '((t (:foreground "slate gray")))
  "Face for the branches and guides in the display tree."
  :group 'combobulate-faces)

(defface combobulate-tree-highlighted-node-face '((t (:inherit font-lock-property-name-face)))
  "Face for combobulate nodes that are prominently displayed in the UI"
  :group 'combobulate-faces)

(defface combobulate-tree-pulse-node-face '((t (:inherit secondary-selection)))
  "Face for nodes that are briefly pulsed on the screen."
  :group 'combobulate-faces)

(defface combobulate-tree-normal-node-face '((t (:inherit default)))
  "Face for regular combobulate nodes in the display tree"
  :group 'combobulate-faces)

;;;; Other settings

(defvar-local combobulate-navigation-defun-procedures nil
  "Node procedures used to navigate by defun.

See `combobulate-beginning-of-defun' and `combobulate-end-of-defun'.")

(defvar-local combobulate-navigation-parent-child-procedures nil
  "Node procedures used to navigate by parent and child nodes.")

(defvar-local combobulate-navigation-default-procedures '((:activation-nodes ((:nodes (all)))))
  "Node procedures as the default in the absence of more specific procedures.

The `combobulate-navigation-default-nodes' variable is populated
with the node types from all the expanded activation node
procedure rules.")

(defvar-local combobulate-navigation-default-nodes nil
  "Node procedures used for general navigation and as a placeholder.

The macro `with-navigation-nodes' binds to this variable and
locally overrides the navigation nodes by Combobulate's node
tools.")

(defvar-local combobulate-manipulation-edit-procedures nil
  "List of edit procedures.")

(defvar-local combobulate-default-procedures nil
  "List of default procedures.

This is typically set by `with-navigation-nodes' by passing a
`:procedures' property with the procedures to use..")

(defvar-local combobulate-procedure-discard-rules '("comment")
  "List of rules to always apply to discard operations.")

(defvar-local combobulate-navigation-sexp-procedures nil
  "Node procedures used to navigate by sexp.

See `combobulate-forward-sexp-function'.")

(defvar-local combobulate-navigation-logical-procedures
    '((:activation-nodes ((:nodes (all)))))
  "Node procedures used to navigate by logical units.")

(defvar-local combobulate-navigation-defun-procedures nil
  "Node procedures used to navigate by defun.")

(defvar-local combobulate-display-ignored-node-types nil
  "Node types that will not appear in the tree display")

(defvar-local combobulate-manipulation-plausible-separators '(",")
  "List of characters that could be valid node separators.")

(defvar-local combobulate-manipulation-indent-after-edit t
  "Non-nil indents the inserted text after a Combobulate refactor text operation.

This should probably be nil in whitespace-sensitive languages.")

(defvar-local combobulate-manipulation-trim-whitespace nil
  "Trim whitespace after Combobulate manipulates the tree.

It can be one of the following values:

`nil' does nothing; `backward' only deletes whitespaces behind
where point is left; `all' deletes forward and backward.")

(defvar-local combobulate-calculate-indent-function
    #'combobulate-baseline-indentation-default
  "Function that determines the baseline indentation of a given position.

The function must take one argument, POS, and from that point
determine the indentation.")

(defvar-local combobulate-manipulation-trim-empty-lines t
  "Non-nil trims empty lines after Combobulate manipulates the tree.")

(defvar-local combobulate-navigation-sibling-skip-prefix nil
  "If non-nil, skip prefixes in the direction of travel when finding a sibling.")

(defvar-local combobulate-navigation-drag-parent-nodes nil
  "Node types that can be dragged up or down")

(defvar-local combobulate-navigation-rules nil
  "Contains the auto-generated production rules.

This must be set in the setup function for the respective mode.")

(defvar-local combobulate-navigation-rules-all nil
  "Contains the auto-generated production rules.

This must be set in the setup function for the respective mode.")

(defvar-local combobulate-navigation-rules-inverted nil
  "Contains the auto-generated inverted production rules.

This must be set in the setup function for the respective mode.")

(defvar-local combobulate-navigation-rules-overrides-inverted nil
  "Alist of override rules for the inverted production rules.

A RULE must be an alist with the KEY being the look-up item and
the VALUE a list of rules:

   \\='((KEY . (VALUE ... VALUE_N)))")

(defvar-local combobulate-envelope-indent-region-function #'indent-region
  "Function to call to indent an envelope after it is inserted.

Note that this defaults to `indent-region', but that may work
well in indentation-sensitive languages like YAML or Python.")

(defvar-local combobulate-envelope-deindent-function nil
  "Function to call to calculate the previous indentation level of point.

The function must determine, from its current position in the
buffer, the *preceding* indentation level.

This is little use to anything except whitespace-sensitive
languages like YAML and Python.")

(defvar combobulate-envelope-symbol-prefix "combobulate-envelop-"
  "Prefix to use for symbol functions and variables for envelopes.")

(defvar-local combobulate-envelope-procedure-shorthand-alist nil
  "Alist of shorthand symbols for envelope procedures.

Each entry must be an alist with the key being the shorthand
symbol and the value being a valid combobulate procedure.

Shorthands are used in lieu of inlining the procedure in the
`:nodes' property for an envelope. It is local to a language.")

(defvar-local combobulate-manipulation-envelopes nil
  "Code generators that wrap -- envelop -- nodes")

(defvar combobulate-manipulation-envelopes-custom nil
  "Alist of (LANGUAGE . ENVELOPES).

Where LANGUAGE must be a valid `treesit-parser-language' symbol
to bind the envelopes against. ENVELOPES must be a list of
envelopes.")

(defvar-local combobulate-navigation-sibling-procedures nil
  "Nodes used for sibling movement")

(defvar-local combobulate-manipulation-node-cluster-queries nil
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

    (\"dictionary\" . (pair \\. (_) @match)

Matches all the key-portion of key-value pairs in a dictionary")

(defvar-local combobulate-highlight-queries-default nil
  "List of Combobulate-provided node queries to highlight.

This list is set internally by the setup function responsible for
configuring Combobulate in a tree-sitter buffer.

Each query should be a well-formed tree-sitter query. Capture
groups should use the name of the face to highlight with. See
`combobulate-query-node-match-faces' for a selection of example
faces to use.

Users who wish to programmatically add their own queries should
use `combobulate-highlight-queries-alist' instead.")

(defvar-local combobulate-highlight-queries-alist nil
  "List of node queries to highlight.

The list is of the form:

    \\='((:language LANGUAGE :query QUERY)
         ...)

Where LANGUAGE is a valid `treesitter-parser-language' symbol and
QUERY is a well-formed tree-sitter query. Capture groups should
use the name of the face to highlight with. See
`combobulate-highlight-queries-default' for more information.")

(put 'combobulate-highlight-queries-alist 'safe-local-variable #'listp)

(defvar-local combobulate-navigation-context-nodes nil
  "List of contextual nodes for use with querying and highlighting.

Most language grammars have one or two nodes that are \"atoms\"
and usually hold the literal text of the nodes around it. For
instance function declarations or variable assignments will
typically contain an `identifier' (or similar) node that holds
the name of the function or the variable being assigned to.

For many language's it's `identifier' or `string', but it could
be any number of nodes.")

(defvar combobulate-setup-functions-alist
  '((python . combobulate-python-setup)
    (tsx . combobulate-js-ts-setup)
    (javascript . combobulate-js-ts-setup)
    (typescript . combobulate-js-ts-setup)
    (jsx . combobulate-js-ts-setup)
    (json . combobulate-json-setup)
    (css . combobulate-css-setup)
    (yaml . combobulate-yaml-setup)
    (html . combobulate-html-setup))
  "Alist of setup functions to call when \\[combobulate-mode] is enabled.

Because tree-sitter-enabled modes are different from the ordinary
ones, you may wish to customize `major-mode-remap-alist' to
silently treat the older modes as their newer TS-enabled
counterparts.")

(defvar combobulate-debug nil
  "Enables additional debug information useful for Combobulate developers")

;;;; Pretty printing and display options

(declare-function combobulate--pretty-print-node "combobulate-navigation")
(declare-function combobulate-pretty-print-node-name "combobulate-navigation")

(defvar-local combobulate-pretty-print-function #'combobulate--pretty-print-node
  "Buffer local function that pretty prints a combobulate node")

(defvar-local combobulate-pretty-print-node-name-function #'combobulate-pretty-print-node-name
  "Buffer local function that pretty prints the node name

This variable must be called by the function in
`combobulate-pretty-print-function'.")


(defvar combobulate-after-setup-hook nil
  "Hook run after Combobulate is done setting up.

This is the right place to add your hooks if you want to change
Combobulate's node configuration.")

(provide 'combobulate-settings)
;;; combobulate-settings.el ends here
