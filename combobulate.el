;;; combobulate.el --- edit and navigate text by syntactic constructs  -*- lexical-binding: t; -*-

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
;; Navigate and transform source code using the concrete syntax tree
;; provided by the Emacs 29's builtin support for tree-sitter.
;;

;;; Code:

;; Requirements:
(require 'seq)
(require 'subr-x)
(require 'tempo)
(require 'map)
(eval-when-compile
  (require 'cl-lib))


(require 'combobulate-settings)
(declare-function combobulate-procedure-collect-activation-nodes "combobulate-procedure")

(defvar-local combobulate-options-envelope-key-map
    (make-sparse-keymap "Combobulate Envelopes")
  "Dynamically set key map of Combobulate envelopes.")

(defvar combobulate-query-key-map
  (let ((map (make-sparse-keymap "Combobulate Query")))
    (define-key map (kbd "q") #'combobulate-query-builder)
    (define-key map (kbd "p") #'combobulate-query-builder-match-node-at-point)
    (define-key map (kbd "r") #'combobulate-query-builder-root-to-point)
    map))

(defvar combobulate-highlight-key-map
  (let ((map (make-sparse-keymap "Combobulate Highlight")))
    (define-key map (kbd "h") #'combobulate-highlight-dwim-at-point)
    (define-key map (kbd "q") #'combobulate-highlight-query)
    (define-key map (kbd "c") #'combobulate-highlight-clear)
    map))


(defvar combobulate-xref-key-map
  (let ((map (make-sparse-keymap "Combobulate Xref")))
    (define-key map (kbd "b") #'combobulate-xref-find-query-buffer-references)
    map))

(defvar combobulate-edit-key-map
  (let ((map (make-sparse-keymap "Combobulate Edit")))
    (define-key map (kbd "c") #'combobulate-edit-cluster-dwim)
    (define-key map (kbd "t") #'combobulate-edit-node-type-dwim)
    (define-key map (kbd "x") #'combobulate-edit-node-by-text-dwim)
    (define-key map (kbd "s") #'combobulate-edit-node-siblings-dwim)
    map))

(defvar combobulate-options-key-map
  (let ((map (make-sparse-keymap "Combobulate Options")))
    (define-key map (kbd "j") #'combobulate-avy-jump)
    (define-key map (kbd "o") #'combobulate)
    (define-key map (kbd "c") #'combobulate-clone-node-dwim)
    (define-key map (kbd "t") combobulate-edit-key-map)
    (define-key map (kbd "x") combobulate-xref-key-map)
    (define-key map (kbd "h") combobulate-highlight-key-map)
    (define-key map (kbd "B") combobulate-query-key-map)
    map))

(defvar combobulate-key-map
  (let ((map (make-sparse-keymap "Combobulate")))
    (define-key map (kbd "C-M-a") #'combobulate-navigate-beginning-of-defun)
    (define-key map (kbd "C-M-d") #'combobulate-navigate-down)
    (define-key map (kbd "C-M-e") #'combobulate-navigate-end-of-defun)
    (define-key map (kbd "C-M-h") #'combobulate-mark-defun)
    (define-key map (kbd "C-M-n") #'combobulate-navigate-next)
    (define-key map (kbd "C-M-p") #'combobulate-navigate-previous)
    (define-key map (kbd "C-M-t") #'combobulate-transpose-sexps)
    (define-key map (kbd "C-M-u") #'combobulate-navigate-up)
    (define-key map (kbd "M-<up>") #'combobulate-splice-up)
    (define-key map (kbd "M-<down>") #'combobulate-splice-down)
    (define-key map (kbd "M-<left>") #'combobulate-splice-self)
    (define-key map (kbd "M-<right>") #'combobulate-splice-parent)
    (define-key map (kbd "M-N") #'combobulate-drag-down)
    (define-key map (kbd "M-P") #'combobulate-drag-up)
    (define-key map (kbd "M-a") #'combobulate-navigate-logical-previous)
    (define-key map (kbd "M-e") #'combobulate-navigate-logical-next)
    (define-key map (kbd "M-h") #'combobulate-mark-node-dwim)
    (define-key map (kbd "M-k") #'combobulate-kill-node-dwim)
    map))

(when combobulate-key-prefix
  (define-key combobulate-key-map (kbd combobulate-key-prefix) combobulate-options-key-map))

(make-variable-buffer-local 'forward-sexp-function)

(defun combobulate--setup-envelopes (envelopes)
  "Prepare ENVELOPES for interactive use.

Each envelope is read and an interactive function for it
created."
  (mapcar
   (lambda (envelope)
     (map-let (:description :name :template :point-placement) envelope
       (let ((fn-name (intern (string-replace
                               " " "-"
                               (concat
                                combobulate-envelope-symbol-prefix
                                (symbol-name major-mode)
                                "-"
                                name)))))
         (set fn-name template)
         ;; Store the function symbol for later recall in things like
         ;; transient.
         (setf envelopes
               (plist-put envelope
                          :function
                          (defalias fn-name
                            `(lambda () ,description
                               (interactive)
                               (combobulate-execute-envelope ,name)))))
         (setf envelopes (plist-put envelope :point-placement (or point-placement 'start))))))
   envelopes))

(defun combobulate-setup-1 ()
  (if-let* ((lang (car-safe (treesit-parser-list)))
            (parser-lang (treesit-parser-language lang))
            (setup-fn (alist-get parser-lang combobulate-setup-functions-alist)))
      (progn
        ;; prepare the sexp functions so they use our version
        (setq-local forward-sexp-function #'combobulate-forward-sexp-function)
        (setq-local transpose-sexps-function #'combobulate-transpose-sexp-function)
        (funcall setup-fn parser-lang)
        ;; Handle the highlighting. For some reason we have to force
        ;; Emacs to load the file/directory-local variables as they're
        ;; otherwise loaded after? Perhaps this has to do with the
        ;; fact that Combobulate is often run in a major mode hook?
        (hack-dir-local-variables-non-file-buffer)
        (hack-local-variables)
        ;; install the highlighter rules
        (combobulate-highlight-install parser-lang)
        ;; `combobulate-default-nodes' draws its nodes from
        ;; `combobulate-procedures-default'.
        (setq-local combobulate-default-nodes
                    (combobulate-procedure-collect-activation-nodes
                     combobulate-procedures-default))
        ;; this should come after the funcall to `setup-fn' as we need
        ;; the procedures setup and ready before we continue.
        (when combobulate-key-prefix
          (local-set-key
           (kbd (format "%s e" combobulate-key-prefix))
           ;; todo: this should be a single-shot setup per mode.
           (let ((map (make-sparse-keymap)))
             (dolist (envelope (combobulate--setup-envelopes combobulate-manipulation-envelopes))
               (map-let (:function :key :extra-key) envelope
                 (define-key map (kbd key) function)
                 (when extra-key
                   (define-key combobulate-key-map (kbd extra-key) function))))
             map)))
        (run-hooks 'combobulate-after-setup-hook))
    (user-error "Combobulate cannot find a setup function for this tree sitter language and major mode: %s (%s).

Customize `combobulate-setup-functions-alist' to change the language setup alist." parser-lang major-mode)))

(defun combobulate-maybe-activate (&optional raise-if-missing)
  "Maybe activate Combobulate in the current buffer.

Do not call this directly. Please use \\[combobulate-mode] to
enable Combobulate."
  ;; (unless combobulate-mode
  ;;   (error "Do not call `combobulate-maybe-activate' directly. Invoke \\[combobulate-mode] instead"))
  ;; Combobulate can activate in any major mode provided it's listed
  ;; in `combobulate-registered-languages-alist'.
  ;;
  (pcase-dolist (`(,language ,major-modes ,minor-mode-fn) combobulate-registered-languages-alist)
    (cond
     ((member major-mode major-modes)
      ;; Only error out if RAISE-IF-MISSING is non-nil. The expected
      ;; behaviour is that Combobulate may get activated in major
      ;; modes for which no grammar exists. Raising an error
      ;; unconditionally in that case would be annoying to users.
      (if (and raise-if-missing (not (combobulate-language-available-p language)))
          (error "Cannot activate Combobulate in buffer `%s' because tree-sitter is missing a language library.

The major mode `%s' is registered as supporting the tree-sitter
language `%s' but that language is not known to tree-sitter.

Try reinstalling the grammar for that language and try again."
                 (current-buffer) major-mode language)
        ;; Language parser exists. Make sure that, if we already have
        ;; an initialised language in the buffer that it is not
        ;; different from the one we think it should be.
        (when-let ((existing-parsers (mapcar #'combobulate-parser-language (combobulate-parser-list))))
          (unless (member language existing-parsers)
            (error "Cannot activate Combobulate in buffer `%s' because of a parser mismatch.

The buffer's language is `%s' and does not match Combobulate's
expected language of `%s'. This can happen if you have major
modes with conflicting ideas of what type of language to use."
                   (current-buffer) (car-safe existing-parsers) language)))
        ;; Okay. All good, then... Create the language parser.
        (combobulate-create-language language (current-buffer) t)
        (let ((toggle (if combobulate-mode
                          ;; We exploit the fact that the variable holding whether the
                          ;; minor mode is enabled or not is the same as the name of the
                          ;; function to toggle it.
                          ;; (symbol-value `,minor-mode-fn)
                          -1 1)))
          (funcall minor-mode-fn toggle)))))))

;;;###autoload
(defun combobulate-mode (&rest _)
  "Navigate and edit by syntactic constructs.

This is a helper command that tries to activate the right
Combobulate minor mode suitable for the current buffer."
  (interactive)
  ;; This is no longer an actual minor mode, but instead a function.
  (combobulate-maybe-activate))

(defun combobulate-setup (&optional arg)
  "Setup combobulate in the current buffer.

This can be used to reinitialize mode-specific setups if they
have changed."
  (interactive "P")
  (if arg
      (let ((prog (make-progress-reporter "Reloading Combobulate...")))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when combobulate-mode
              (combobulate-setup-1)))
          (progress-reporter-update prog))
        (progress-reporter-done prog))
    (combobulate-setup-1)))

(defun combobulate-get (language shorthand &optional check)
  "Generate a symbol that belongs to LANGUAGE named SHORTHAND.

If CHECK is non-nil raise an assertion if the variable with that
SHORTHAND for that LANGUAGE is unbound."
  (let ((var (intern (format "combobulate-%s-%s" language shorthand))))
    (when (and (not (boundp var)) check)
      (cl-assert (boundp var) nil
                 "Variable `%s' does exist in language `%s' with shorthand `%s'"
                 var language shorthand))
    var))

(defmacro combobulate-read (shorthand &optional language)
  "Read a variable belonging to LANGUAGE with SHORTHAND.

Both LANGUAGE and SHORTHAND must be unquoted. If LANGUAGE is nil,
use the primary language for the current buffer.

SHORTHAND is the postfix to use to find the variable named after
the LANGUAGE. That name generated by `combobulate-get' and is
usually named `combobulate-LANGUAGE-SHORTHAND'.

A complete list of known shorthands are found in
`combobulate-LANGUAGE-defined-variables'."
  `(symbol-value (combobulate-get
                  (or ,language (combobulate-primary-language))
                  ',shorthand t)))

(defvar combobulate-registered-languages-alist nil
  "Alist of tree-sitter languages and major modes supported by Combobulate.

Each entry must be of the form

   (LANGUAGE MAJOR-MODES MINOR-MODE-FN)

Where LANGUAGE is the tree-sitter language symbol; MAJOR-MODES is
a list of supported major modes; and MINOR-MODE-FN is a minor
mode function that Combobulate must use for that language and
those major modes.")

(defun combobulate-register-language (language major-modes minor-mode-fn)
  (if-let ((def (cdr (assoc language combobulate-registered-languages-alist))))
      ;; Check if the definition's identical to what we want to
      ;; add. If it is, do nothing; if it is not, raise an error.
      (unless (equal def (list major-modes minor-mode-fn))
        (error "Language `%s' is already registered with a different definition." language))
    (push (list language major-modes minor-mode-fn)
          combobulate-registered-languages-alist)))

(defvar combobulate-mode nil
  "Non-nil if the language-specific minor mode is enabled in this buffer.

Note that `combobulate-mode' is not a true minor mode: it is a
helper command that calls the proper Combobulate minor mode
suitable for the major mode of the current buffer.")

(cl-defmacro define-combobulate-language (&key name language
                                               major-modes custom
                                               setup-fn
                                               (keymap-var nil)
                                               (extra-defcustoms nil)
                                               (extra-defvars nil))
  "Define a new language for Combobulate.

NAME is the name of the language as it'll be known to
Combobulate; LANGUAGE is the tree sitter language symbol, and
MAJOR-MODES is a list of major modes that should be set up for
this language."
  (cl-assert (symbolp language) t "LANGUAGE must be a list")
  (cl-assert (symbolp name) t "NAME must be a symbol")
  ;; (cl-assert (symbolp setup-fn) t "SETUP-FN must be a symbol")
  (let ((group-name (intern (format "combobulate-language-%s" name))))
    ;; Create a customize group for the language
    (let ((known-vars)
          (defvars
           ;; ((SHORTHAND DOCSTRING DEFAULT-VALUE) ... )
           (append
            '((procedures-defun
               "Procedures that control navigation to the next/previous defun."
               nil)
              (procedures-hierarchy
               "Procedures that control navigation in and out of node hierarchies."
               nil)
              (procedures-default
               "Node procedures used by Combobulate when more specific procedures don't apply.

The `combobulate-default-nodes' variable is populated
with the node types from all the expanded activation node
procedure rules."
               '((:activation-nodes ((:nodes (all))))))
              (default-nodes
               "List of active node types used for node discovery.

This variable is almost always auto-populated by Combobulate when
a set of procedures are activated, and should only be let-bound
or set with `with-navigation-nodes'."
               nil)
              (procedures-edit
               "Procedures used to mark clusters of editable nodes.

This is used by some Combobulate commands that edit nodes. Most
commonly multi-cursor editing."
               nil)
              (default-procedures
               "List of active procedures used by Combobulate's procedure system.

This variable is almost always auto-populated by Combobulate from
one of the other procedure variables, and should only be
let-bound or set with `with-navigation-nodes'.")
              (procedures-sexp
               "Procedures that control navigation to the next/previous sexp.

This is used by by Combobulate's backwards-compatible sexp
navigation system, and it applies to all `-sexp' commands in
Emacs."
               nil)
              (procedures-logical
               "Procedures that control logical navigation forward and backward."
               '((:activation-nodes ((:nodes (all))))))
              (procedures-sibling
               "Procedures that control navigation between sibling nodes."
               nil)
              (display-ignored-node-types
               "List of node types to never display in the Combobulate display tree."
               nil)
              (plausible-separators
               "List of strings of plausible separators found in this language.

This is a fairly blunt way of instructing Combobulate of the type
of separators to expect in a language. It's used to help with
editing. The default value is usually good enough for most
languages."
               '(","))
              (procedure-discard-rules
               "List of discard rules to apply to procedure matches.

This is a generic filter to apply across all procedures. Any rule
specified in this list will be applied to all procedures,
regardless of the procedure's activation rules."
               '("comment"))
              (indent-after-edit
               "Non-nil indents the inserted text after a Combobulate refactor text operation.

This should probably be nil in whitespace-sensitive languages."
               t)
              (indent-calculate-function
               "Function that determines the baseline indentation of a given position.

The function must take one argument, POS, and from that point
determine the indentation."
               #'combobulate-baseline-indentation-default)
              (envelope-indent-region-function
               "Function to call to indent an envelope after it is inserted.

Note that this defaults to `indent-region', but that may work
well in indentation-sensitive languages like YAML or Python."
               nil)
              (envelope-deindent-function
               "Function to call to calculate the previous indentation level of point.

The function must determine, from its current position in the
buffer, the *preceding* indentation level.

This is little use to anything except whitespace-sensitive
languages like YAML and Python."
               nil)
              (envelope-procedure-shorthand-alist
               "Alist of shorthand symbols for envelope procedures.

Each entry must be an alist with the key being the shorthand
symbol and the value being a valid combobulate procedure.

Shorthands are used in lieu of inlining the procedure in the
`:nodes' property for an envelope, and instead lets you refer to
a procedure by a `:shorthand' property matching the key in this
alist."
               nil)
              (envelope-list
               "List of envelope definitions for this language.

Each entry is a list of properties that define the envelope. The
properties are:

  (:description STRING
   :name STRING
   :key STRING
   :mark-node <t/nil>
   :template ENVELOPE
   [:extra-key STRING]
   [:shorthand SHORTHAND-SYMBOL]
   [:nodes NODES]
   [:point-placement <stay/after/before>])

Where `:description' is a human-readable description of the
envelope.

`:name' is the name of the envelope that will be
inserted into the envelope command.

`:key' is the key binding (in `kbd' macro format) to bind to the
envelope such that it is created under `C-c o e
<key>'. `:extra-key' is an optional key binding that is bound to
any arbitrary key binding.

`:shorthand' is a shorthand symbol to use in place of
`:nodes' (see the shorthand procedures alist variable)

 `:nodes'is a list of nodes to look for at/around point before
 expanding the envelope template.

`:mark-node' is a boolean that determines whether to mark the
node that `:nodes' (or `:shorthand') matches and remove it and
insert it into the `r'/`r>' registers.

`:template' is the template to insert. See
`combobulate-envelope-expand-instructions' for a complete
description of the template format.

`:point-placement' is a symbol that determines where point is
placed after the envelope is inserted. It can be `stay',
`after', or `before'."
               nil)
              (highlight-queries-default
               "List of Combobulate-provided node queries to highlight.

This list is set internally by the setup function responsible for
configuring Combobulate in a tree-sitter buffer.

Each query should be a well-formed tree-sitter query. Capture
groups should use the name of the face to highlight with. See the
keys in `combobulate-query-match-face-alist' for a selection of
example faces to use.

Users who wish to programmatically add their own queries using
file/directory-local variables, or through customization, should
use `combobulate-highlight-queries-alist' instead."
               nil)
              (context-nodes
               "List of contextual nodes for use with querying and highlighting.

Most language grammars have one or two nodes that are \"atoms\"
and usually hold the literal text of the nodes around it. For
instance function declarations or variable assignments will
typically contain an `identifier' (or similar) node that holds
the name of the function or the variable being assigned to.

For many languages it's usually something like `identifier' or
`string', but it could be any number of nodes."
               nil)
              (pretty-print-function
               "Function that pretty prints a Combobulate node."
               #'combobulate--pretty-print-node)
              (pretty-print-node-name-function
               "Function that pretty prints a node name."
               #'combobulate-pretty-print-node-name))
            (symbol-value extra-defvars)))
          (defcustoms
           (append
            `((major-modes
               "List of major modes where this language applies.

Combobulate will set up tree-sitter to use that language as the
primary buffer language if the major mode does not do this
itself. This is also useful for modes that lack tree-sitter
support where you still want to use Combobulate's features."
               ',major-modes
               :type '(repeat symbol)))
            (symbol-value extra-defcustoms)))
          (known-variable-shorthands)
          (decls))
      (cl-flet*
          ((intern-var (lang-name variable)
             (combobulate-get lang-name variable))
           (intern-lang-var (variable)
             (push (if (stringp variable)
                       (intern variable)
                     variable)
                   known-variable-shorthands)
             (intern-var name variable)))
        (push `(custom-declare-group
                ',group-name nil ,(format "Language settings for `%s'" name)
                :group 'combobulate)
              decls)
        ;; Create the `defvar' forms.
        (pcase-dolist (`(,shorthand-var ,docstring ,default) defvars)
          ;; (push shorthand-var known-variable-shorthands)
          (push `(defvar ,(intern-lang-var shorthand-var) ,default ,docstring) decls))
        ;; Create the `defcustom' forms.
        (pcase-dolist (`(,shorthand-var ,docstring ,default . ,rest) defcustoms)
          ;; (push shorthand-var known-variable-shorthands)
          (push `(defcustom ,(intern-lang-var shorthand-var) ,default ,docstring
                   ,@rest
                   :group ',group-name)
                decls))
        ;; Walk through the `:custom' property and assign the values.
        (pcase-dolist (`(,shorthand-var . ,value) (symbol-value custom))
          (cl-assert (member shorthand-var known-variable-shorthands) t
                     "Key `%s' is not a known variable shorthand in definition `%s'"
                     shorthand-var name)
          ;; use the new `setopt' helper macro here. If we ever add
          ;; edge-triggers to `defcustom's they'll be properly executed.
          (push `(setopt ,(intern-lang-var shorthand-var) ,(car-safe (ensure-list value)))
                decls))
        ;; Store a representation of all variable shorthand names used.

        ;; Create (or reuse) a key map and create a minor mode for
        ;; LANGUAGE.
        (let ((language-keymap (or keymap-var (intern-lang-var "map")))
              (envelope-keymap (or keymap-var (intern-lang-var "envelope-map")))
              (minor-mode-fn (intern-lang-var "minor-mode")))
          ;; General key map for this language
          (push `(defvar-keymap ,language-keymap
                   :doc ,(format "Keymap for Combobulate language for `%s'." language)
                   :full nil
                   :parent combobulate-key-map)
                decls)
          ;; This is the envelope-specific keymap
          (push `(defvar-keymap ,envelope-keymap
                   :doc ,(format "Keymap for Combobulate envelopes for `%s'." language)
                   :full nil)
                decls)
          (push `(keymap-set ,language-keymap "e" ,envelope-keymap)
                decls)
          ;; Create a minor mode for this language.
          (push `(define-minor-mode ,minor-mode-fn
                   ,(format "Combobulate minor mode the `%s' tree-sitter language." language)
                   :init-value nil
                   :lighter "©"
                   :keymap ,language-keymap
                   ;; Recycle `combobulate-mode' as the variable used
                   ;; to store the toggle state of this minor mode
                   ;; even though it is effectively 'shared' across
                   ;; all the various minor modes we may create when
                   ;; this macro is called. This has two beneficial
                   ;; advantages:
                   ;;
                   ;;  1. `M-x combobulate-mode' is the default
                   ;;  entrypoint for the package, and people expect
                   ;;  the "mode" variable to match the command name.
                   ;;
                   ;;  2. It is not possible to engage more than one
                   ;;  minor mode in the same buffer.
                   :variable combobulate-mode
                   ;; Hm... leaving this to nil is probably for the
                   ;; best. We do not want to encourage people to go
                   ;; around experimenting with minor mode as that
                   ;; could circumvent other setup processes.
                   :interactive nil
                   ;; This is the generic setup function that is
                   ;; always run.
                   (combobulate-setup-1)
                   ;; If a language has a custom setup function, we
                   ;; run it with the language we are being
                   ;; triggered in.
                   ,(when setup-fn
                      `(,setup-fn ',language)))
                decls)
          (push `(combobulate-register-language ',language ',major-modes #',minor-mode-fn)
                decls)
          (push `(defconst ,(intern-lang-var "defined-variables")
                   ',known-variable-shorthands
                   ,(format "List of variable shorthands known to `%s'.

Each shorthand is a symbol referencing a variable belonging to
that language. They are best accessed with the `combobulate-read'
macro which optionally takes a language argument to retrieve that
language's setting." language))
                decls)))
      `(progn ,@(nreverse decls)))))

;;; internal
(require 'combobulate-rules)
(require 'combobulate-procedure)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-envelope)
(require 'combobulate-contrib)
(require 'combobulate-display)
(require 'combobulate-ui)
(require 'combobulate-misc)
(require 'combobulate-query)
;;; end internal

;;; language support
(require 'combobulate-toml)
(require 'combobulate-html)
(require 'combobulate-python)
(require 'combobulate-js-ts)
(require 'combobulate-css)
(require 'combobulate-yaml)
(require 'combobulate-json)
(require 'combobulate-go)
;;; end language support

(provide 'combobulate)
;;; combobulate.el ends here


