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

(defcustom combobulate-highlight-context t
  "Highlight contextual nodes that belong to the same sequence.

This is similar to `show-paren-mode' but for Combobulate's notion of
what a \"pair\" is."
  :type 'boolean
  :group 'combobulate)

(defcustom combobulate-navigate-down-into-lists t
  "Whether to navigate into the first list-like structure ahead
of point in the absence of other suitable nodes."
  :type 'boolean
  :group 'combobulate)

;;; Group for cursor editing
(defgroup combobulate-cursor nil
  "Settings for Combobulate's cursor editing."
  :group 'combobulate
  :prefix "combobulate-cursor-")

(defcustom combobulate-cursor-tool 'combobulate
  "The tool to use for cursor editing.

This can be `combobulate' or `multiple-cursors'.

`combobulate' is the default and uses Combobulate's own
refactoring system to perform cursor editing.

`multiple-cursors' uses the third-party package `multiple-cursors.el'."
  :group 'combobulate-cursor
  :type '(choice
          (const :tag "Combobulate" combobulate)
          (const :tag "Multiple Cursors" multiple-cursors)))

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
  "Face for inactive prompts and fields during editing or refactoring."
  :group 'combobulate-faces)

(defface combobulate-refactor-disabled-field-face '((t (:background "gray20" :foreground "gray80")))
  "Face for disabled prompts and fields during editing or refactoring."
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

(defface combobulate-active-indicator-face '((t (:foreground "SlateBlue1")))
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

(defface combobulate-highlight-context-face '((t (:inherit combobulate-refactor-field-face)))
  "Face for the context of the current node in the display tree."
  :group 'combobulate-faces)

;;;; Other settings

(defvar combobulate-navigable-nodes nil
  "List of node types used for general navigation and as a placeholder.

The macro `with-navigation-nodes' binds all activation nodes to
this variable, so that Combobulate can act on those node types
without requiring that they are lisetd explicitly.")

(defvar-local combobulate-default-procedures nil
  "List of default procedures to use for general navigation and editing.

This is typically set by `with-navigation-nodes' by passing a
`:procedures' property with the list of procedures to use.")


(defvar combobulate-envelope-symbol-prefix "combobulate-envelop-"
  "Prefix to use for symbol functions and variables for envelopes.")

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

(defvar combobulate-setup-functions-alist
  '((python . combobulate-python-setup)
    (tsx . combobulate-js-ts-setup)
    (javascript . combobulate-js-ts-setup)
    (go . combobulate-go-setup)
    (typescript . combobulate-js-ts-setup)
    (jsx . combobulate-js-ts-setup)
    (json . combobulate-json-setup)
    (css . combobulate-css-setup)
    (yaml . combobulate-yaml-setup)
    (toml . combobulate-toml-setup)
    (ocaml . combobulate-ocaml-setup)
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


(defvar combobulate-after-setup-hook nil
  "Hook run after Combobulate is done setting up.

This is the right place to add your hooks if you want to change
Combobulate's node configuration.")

(provide 'combobulate-settings)
;;; combobulate-settings.el ends here
