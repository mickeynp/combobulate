;;; combobulate-query.el --- highlight, search, edit and interactively build queries  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

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

;; This library borrowed the after-change and overlay machinery from
;; Tuấn-Anh Nguyễn's "query builder" extension found in his
;; tree-sitter.el package (`https://github.com/emacs-tree-sitter'),
;; though nowadays it's mostly been rewritten and altered.
;;
;;
;; Other, new, features include: syntax highlighting; code completion;
;; directory-local variable support; edit with multiple cursors; xref
;; support, and much more.
;;
;;; Code:

(require 'combobulate-manipulation)
(require 'combobulate-navigation)
(require 'combobulate-misc)
(require 'combobulate-interface)
(require 'combobulate-rules)
;;; base mode for the query builder
(require 'scheme)
;;; for plist stuff
(require 'map)
;;; for directory local stuff
(require 'files-x)
;; for xref
(require 'xref)

(eval-when-compile
  (require 'cl-lib))

(defvar combobulate-mode)

(defvar combobulate-query-ring-index 0
  "Index of the current query in `combobulate-query-ring'.")

(defvar combobulate-query-ring (make-ring 50)
  "History of previous queries.")

(require 'savehist)
(add-to-list 'savehist-additional-variables 'combobulate-query-ring)


(defconst combobulate-query-builder-predicate-names '("#match" "#equal" "#pred")
  "Known Emacs-supported predicate names.")

(defvar combobulate-query-builder-parser nil
  "Tree sitter parser to query against.")

(defconst combobulate-query-builder-buffer-name "*combobulate-query-builder*"
  "Name of the builder buffer.")

(defvar combobulate-query-builder-target-buffer-name nil
  "Target buffer to run the queries against.")

(defvar combobulate-query-default-query nil
  "Default query to use.

If this value is set, it will be used as the default query when
`combobulate-query-builder' is executed.")

(defvar-local combobulate-query-builder-active-parser nil
  "The active tree-sitter parser to query against.")

(defvar-local combobulate-query-builder-font-lock-keywords nil
  "Font lock keywords for `combobulate-query-mode'.")

(defvar-local combobulate-query-builder-field-names nil
  "List of field names used by the active parser.")

(defvar-local combobulate-query-builder-rule-names nil
  "List of rule names used by the active parser.")

(defvar-local combobulate-query-builder-rules nil
  "List of rules sourced from the combobulate rules file.")

(defvar-local combobulate-query-builder-match-capture-faces-alist nil
  "Alist of match captures and the face to use.")

(defgroup combobulate-query nil
  "Query-related options for Combobulate."
  :group 'combobulate)

(defgroup combobulate-query-faces nil
  "Faces for Combobulate query builder."
  :group 'combobulate-query)

(defface combobulate-query-query-constant-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for the various supported rule names."
  :group 'combobulate-faces)

(defface combobulate-query-query-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for `field:' names."
  :group 'combobulate-faces)

(defface combobulate-query-query-predicate-builtin-face
  '((t (:foreground "tomato2")))
  "Face for #predicates."
  :group 'combobulate-faces)

(defface combobulate-query-query-builtin-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for @match capturing groups."
  :group 'combobulate-faces)

(defface combobulate-query-query-string-face
  '((t (:inherit font-lock-string-face)))
  "Face for strings."
  :group 'combobulate-faces)

(defface combobulate-query-query-doc-markup-face
  '((t (:inherit font-lock-warning-face)))
  "Face for miscellaneous quantifiers and operators."
  :group 'combobulate-faces)

(defface combobulate-query-query-anonymous-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for anonymous named and unnamed nodes."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-vibrant-veggie-face
  '((t (:background "DarkOliveGreen4" :foreground "DarkOliveGreen1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-regal-ripples-face
  '((t (:background "RoyalBlue4" :foreground "RoyalBlue1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-gleaming-gold-face
  '((t (:background "DarkGoldenrod4" :foreground "DarkGoldenrod1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-silver-shadows-face
  '((t (:background "SlateGray4" :foreground "SlateGray1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-fiery-flames-face
  '((t (:background "Firebrick4" :foreground "Firebrick1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-mysterious-mauve-face
  '((t (:background "DarkOrchid4" :foreground "DarkOrchid1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-radiant-rind-face
  '((t (:background "DarkOrange4" :foreground "DarkOrange1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-serene-shade-face
  '((t (:background "DarkSeaGreen4" :foreground "DarkSeaGreen1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defface combobulate-query-highlight-majestic-mercury-face
  '((t (:background "SteelBlue4" :foreground "SteelBlue1")))
  "Face for highlighting query matches."
  :group 'combobulate-faces)

(defcustom combobulate-query-node-match-faces
  '(combobulate-query-highlight-regal-ripples-face
    combobulate-query-highlight-gleaming-gold-face
    combobulate-query-highlight-silver-shadows-face
    combobulate-query-highlight-fiery-flames-face
    combobulate-query-highlight-mysterious-mauve-face
    combobulate-query-highlight-radiant-rind-face
    combobulate-query-highlight-serene-shade-face
    combobulate-query-highlight-majestic-mercury-face
    combobulate-query-highlight-vibrant-veggie-face)
  "Faces for highlight captures in the target buffer."
  :group 'combobulate-query
  :type '(repeat face))

(defcustom combobulate-query-match-face-alist
  '(("hl.default" . 'combobulate-query-highlight-gleaming-gold-face)
    ("hl.gold" . 'combobulate-query-highlight-gleaming-gold-face)
    ("hl.fiery" . 'combobulate-query-highlight-fiery-flames-face)
    ("hl.ripples" . 'combobulate-query-highlight-regal-ripples-face)
    ("hl.silver" . 'combobulate-query-highlight-silver-shadows-face)
    ("hl.mauve" . 'combobulate-query-highlight-mysterious-mauve-face)
    ("hl.veggie" . 'combobulate-query-highlight-vibrant-veggie-face)
    ("hl.serene" . 'combobulate-query-highlight-serene-shade-face)
    ("hl.mercury" . 'combobulate-query-highlight-majestic-mercury-face)
    ("hl.rind" . 'combobulate-query-highlight-radiant-rind-face)
    ("hl.comment" . 'font-lock-comment-face)
    ;; for combobulate's edit nodes facility
    ("before" . 'combobulate-query-highlight-silver-shadows-face)
    ("after" . 'combobulate-query-highlight-regal-ripples-face)
    ("mark" . 'region))
  "Alist of match names and the face to use.

This is used to highlight matches in the target buffer by
shortening the name of the face to something simple and
memorable.

The car of each element is the name of the match as a string but
without the `@' symbol.  So, \"hl.yellow\" corresponds to
`@hl.yellow'. The cdr is the face to use for highlighting."
  :group 'combobulate-query
  :type '(alist :key-type string :value-type face))

(defcustom combobulate-query-builder-edit-max-nodes 500
  "Maximum number of nodes Combobulate will attempt to edit."
  :group 'combobulate-query
  :type 'integer)

(defcustom combobulate-query-builder-fit-to-window t
  "Whether to resize the query builder window when text is inserted."
  :group 'combobulate-query
  :type 'boolean)

(defcustom combobulate-xref-show-xrefs-function #'xref-show-definitions-completing-read
  "Function to use to show xrefs.

This is used by `combobulate-query-builder-show-xrefs' to show
the xrefs for a query. This variable is used to temporarily bind
`xref-show-xrefs-function' to this value. This is useful if you
want the default xrefs functionality for normal xrefs but want to
use something else for Combobulate.

By default it uses `xref-show-definitions-completing-read' which
shows the xrefs in a completing read prompt."
  :group 'combobulate-query
  :type 'function)

(defun combobulate-query-builder-prop-name-to-field-name (prop-name)
  "Turn a :PROP-NAME into `field-name:'."
  (concat (substring (symbol-name prop-name) 1) ":"))

(defun combobulate-query-builder-thing-to-field-name (thing &optional as-string)
  "Turn a THING (string, symbol or :key) into `thing:'.

IF AS-STRING is non-nil, then the result is returned as a string
instead of a symbol."
  (and thing
       ;; stringp, symbolp, and plists are supported
       (let ((field
              (cond ((stringp thing)
                     (make-symbol (if (string-suffix-p ":" thing)
                                      thing
                                    (concat thing ":"))))
                    ((symbolp thing)
                     (let ((name (symbol-name thing)))
                       (combobulate-query-builder-thing-to-field-name
                        (if (string-prefix-p ":" name) (substring name 1) name))))
                    (t (error "Unknown thing type: %s" thing)))))
         (if as-string (symbol-name field) field))))

(defun combobulate-query-builder-expand-match-face (query &optional additional-match-faces)
  "Given a QUERY, expand all `@hl.*' matches with the appropriate face.

The QUERY can be either a Lisp form or a string, but it is always
returned as a string.

Faces are sourced from `combobulate-query-match-face-alist'.  If
ADDITIONAL-MATCH-FACES is non-nil, then it is an alist of match
faces to also replace.  NOTE that neither ADDITIONAL-MATCH-FACES
nor `combobulate-query-match-face-alist' should have `@' prefixed
to its keys.

Note that ADDITIONAL-MATCH-FACES are converted first, giving you
the opportunity to map generic constructs like `@capture' to a
known combobulate highlighter, for instance."
  (let ((query (if (stringp query) query (prin1-to-string query))))
    (dolist (match-face (append additional-match-faces
                                combobulate-query-match-face-alist))
      (let ((match-name (car match-face))
            (face (cdr match-face)))
        (setq query (replace-regexp-in-string
                     (concat (concat "@" match-name))
                     (concat "@" (symbol-name (cadr face)))
                     query))))
    query))


(defun combobulate-query-builder-completion-at-point-function ()
  "Completion function for `combobulate-query-mode'.

This function is used by `completion-at-point-functions' to provide
completion candidates for the current point."
  (cl-flet* ((get-symbol () (thing-at-point 'symbol t))
             (go-up () (ignore-errors (backward-up-list 1)))
             (go-down () (ignore-errors (down-list)))
             (at-char-p (s) (looking-back s 1))
             (skip-syntax (direction &optional syntax)
               (if (eq direction 'forward)
                   (skip-syntax-forward (or syntax "w_"))
                 (skip-syntax-backward (or syntax "w_")))))
    (save-excursion
      (let* ((start) (end (point))
             ;; Note this moves point as a side-effect of each check. The
             ;; order is therefore important.
             (has-prefix-text (prog1
                                  (> (skip-syntax-backward "w_") 0)
                                (setq start (point))))
             ;; Are we at the start of a form?
             (at-form-start (at-char-p "("))
             ;; Are we at the start of a match group?
             (at-match-group-start (at-char-p "@"))
             ;; Move to the end of the current text.
             (has-postfix-text (prog1
                                   (> (skip-syntax-forward "w_") 0)
                                 (setq end (point))))
             ;; Are we now at the end of a form?
             (at-form-end (at-char-p ")"))
             ;; Use `syntax-ppss' to tell us if we're inside a
             ;; string or not.
             (inside-string (nth 3 (syntax-ppss)))
             (parent-rule-name (save-excursion
                                 ;; Move up out of the current form.
                                 ;; (parent (chi|ld)) -> (parent |(child))
                                 (go-up)
                                 ;; Move up out of of the parent form
                                 ;; (parent (child)) -> |(parent (child))
                                 (go-up)
                                 ;; go back down again to get at the parent
                                 ;; (parent (child)) -> (|parent (child))
                                 (go-down)
                                 (get-symbol)))
             (parent-field-name (save-excursion
                                  (go-up)
                                  (skip-syntax 'backward " ")
                                  (when (string-suffix-p ":" (get-symbol))
                                    (string-trim (or (get-symbol) "") nil ":"))))
             ;; (at-field-name (save-excursion
             ;;                  (and (get-symbol) (string-suffix-p (or (get-symbol) "") ":"))))
             (current-rule-name (save-excursion
                                  (go-up)
                                  (go-down)
                                  (get-symbol))))
        (list start end
              (cond
               ;; it's a string - do nothing.
               (inside-string nil)
               ;; it's a match group
               (at-match-group-start (map-keys combobulate-query-match-face-alist))
               ;; it's a rule name
               ((or has-prefix-text at-form-start has-postfix-text at-form-end)
                (append (if-let (rule (assoc parent-rule-name combobulate-query-builder-rules))
                            (flatten-list
                             (cons (if parent-field-name
                                       (or (plist-get (cadr rule) (intern (concat ":" parent-field-name)))
                                           combobulate-query-builder-rule-names)
                                     (map-values (cadr rule)))
                                   nil))
                          combobulate-query-builder-rule-names)
                        (copy-tree combobulate-query-builder-predicate-names)))
               ;; it's a field name
               ((and (not at-form-start) (not at-form-end))
                (when-let (rule (assoc current-rule-name combobulate-query-builder-rules))
                  (mapcar #'combobulate-query-builder-prop-name-to-field-name
                          (seq-remove (lambda (prop) (equal prop :*unnamed*))
                                      (map-keys (cadr rule))))))
               (t nil)))))))

(defun combobulate-query-builder-change-parser ()
  "Change the parser used by `combobulate-query-mode'."
  (interactive)
  (unless (buffer-live-p combobulate-query-builder-target-buffer-name)
    (error "Buffer `%s' is not live" combobulate-query-builder-target-buffer-name))
  (let ((parsers (mapcar (lambda (p) (cons (format "%s" p) p)) (combobulate-parser-list))))
    (setq combobulate-query-builder-parser
          (cond
           ((length= parsers 1) (cdr (car parsers)))
           ((length> parsers 1)
            (cdr (assoc (completing-read "Pick a parser" parsers) parsers)))
           (t (if-let ((p (completing-read "There are no associated parsers. Pick a parser Combobulate knows about: "
                                           combobulate-rules-languages)))
                  (combobulate-parser-create (intern p) combobulate-query-builder-target-buffer-name)
                (error "No parser selected")))))
    (when combobulate-query-builder-parser
      (combobulate-message
       (format "Parser changed to `%s'" (combobulate-parser-language combobulate-query-builder-parser))))))


(defun combobulate-query-builder--highlight-capture (capture)
  "Highlight CAPTURE in the current buffer."
  (pcase-let* ((`(,capture-name . ,captured-node) capture)
               (`(,node-start . ,node-end) (combobulate-node-range captured-node))
               (overlay (make-overlay node-start node-end)))
    ;; Ensure the overlay is deleted when it becomes empty.
    (overlay-put overlay 'evaporate t)
    (let ((face (or (cdr (assoc-string capture-name combobulate-query-match-face-alist))
                    (cdr (assoc capture-name combobulate-query-builder-match-capture-faces-alist))
                    (and (facep capture-name) capture-name)
                    (let* ((default-face (nth (mod (length combobulate-query-builder-match-capture-faces-alist)
                                                   (length combobulate-query-node-match-faces))
                                              combobulate-query-node-match-faces))
                           (default-face-entry (cons capture-name default-face)))
                      (setq combobulate-query-builder-match-capture-faces-alist
                            (cons default-face-entry combobulate-query-builder-match-capture-faces-alist))
                      (cdr default-face-entry)))))
      (overlay-put overlay 'face face)
      (unless (string-empty-p capture-name)
        (overlay-put overlay 'help-echo (format "Capture group: @%s" (symbol-name capture-name))))
      (setq face (cdr (assoc capture-name combobulate-query-builder-match-capture-faces-alist))))))

(defun combobulate-query-builder-eval-query (query)
  "Evaluate query QUERY against the target buffer."
  (with-current-buffer combobulate-query-builder-target-buffer-name
    (remove-overlays)
    (let ((captures (combobulate-query-capture combobulate-query-builder-parser query)))
      (if (= (length captures) 0)
          (with-current-buffer combobulate-query-builder-buffer-name
            (combobulate-query-builder-update-header
             ;; rudimentary check for capture groups.
             (if (string-match-p "@" (combobulate-query-builder-get-query))
                 (propertize "No matches" 'face 'font-lock-warning-face)
               (propertize "No capture group(s) in query" 'face 'error))))
        (with-current-buffer combobulate-query-builder-buffer-name
          (combobulate-query-builder-update-header
           (propertize (format "%d match(es) found" (length captures)) 'face 'success)))
        (setq combobulate-query-builder-match-capture-faces-alist nil)
        (mapc #'combobulate-query-builder--highlight-capture captures)))))

(defun combobulate-query-builder--after-change (&rest _args)
  "Run query against the target buffer and highlight the matches."
  (ignore-errors
    (with-current-buffer (get-buffer combobulate-query-builder-buffer-name)
      (when combobulate-query-builder-parser
        (let ((query (combobulate-query-builder-get-query)))
          (if-let (err (with-current-buffer combobulate-query-builder-target-buffer-name
                         (combobulate-query-builder-validate-query query)))
              ;; invalid query
              (seq-let [start &rest message] err
                (combobulate-query-builder-update-header
                 (propertize (format "%s position `%s'" message start) 'face 'warning)))
            ;; query is valid
            (combobulate-query-builder-eval-query query)))))))

(cl-defun combobulate-query-builder-validate-query (query)
  "Validate QUERY and return a cons cell if the query is invalid."
  (ignore-errors
    (condition-case err
        (progn (combobulate-query-capture (combobulate-root-node) query) nil)
      ;; this can easily happen during startup.
      (treesit-query-error
       (pcase-let ((`(,_ ,message ,start . ,_) err))
         (cons start message))))))

(defun combobulate-query-builder--clean-target-buffer ()
  "Remove all overlays from the target buffer."
  (ignore-errors
    (with-current-buffer combobulate-query-builder-target-buffer-name
      (remove-overlays))
    (setq combobulate-query-builder-target-buffer-name nil)
    (delete-window (get-buffer-window combobulate-query-builder-buffer-name))))

(defun combobulate-query-builder-update-header (msg)
  "Update the header line with MSG."
  (setq header-line-format
        (concat "Parser: "
                (propertize (symbol-name (combobulate-parser-language
                                          combobulate-query-builder-parser))
                            'face 'bold)
                " | "
                "Cmds: " (propertize combobulate-key-prefix 'face 'help-key-binding)
                " | "
                "Target Buffer: " (propertize (buffer-name combobulate-query-builder-target-buffer-name)
                                              'face 'bold)
                " | "
                "Status: " msg)))


(defun combobulate-query-builder-get-node-text (node &optional max-length)
  "Return NODE's text, truncated to MAX-LENGTH characters."
  (let ((text (combobulate-node-text node)))
    (if (> (length text) (or max-length 50))
        (concat (substring text 0 (or max-length 50)))
      text)))

(defun combobulate-query-builder-to-string (form-query)
  "Convert FORM-QUERY to a string query.

Rewrites `:match' to `#match', etc. along the way."
  (let ((tgt-query (if (stringp form-query) form-query
                     (pp-to-string form-query))))
    (pcase-dolist (`(,before . ,after)
                   ;; `.' is the anchor in the real query language,
                   ;; but `:anchor' in elisp forms.
                   `((":anchor" . ".")
                     ;; Fix up `:match' to `#match', etc.
                     (,(rx ":" (group (| "match" "pred" "eq"))) . "#\\1")))
      (setq tgt-query (replace-regexp-in-string before after tgt-query)))
    (string-trim tgt-query)))

(defun combobulate-query-builder-root-to-point ()
  "Interactively build a query from the root node to point."
  (interactive)
  (let* ((node (combobulate-node-at-point nil t))
         (query (combobulate-build-nested-query
                 (cons node (combobulate-get-parents node))
                 (combobulate-query-builder-capture-group-fn
                  '@hl.default (combobulate-query-builder-get-node-text node) 1)))
         (combobulate-query-default-query (combobulate-query-builder-to-string query)))
    (combobulate-query-builder)))

(defun combobulate-query-pretty-print (query &optional parser)
  "Pretty print QUERY using the given PARSER, or the first parser if none is given."
  (combobulate-query-builder-fontify-query
   (combobulate-query-builder-to-string query)
   (or parser (car (combobulate-parser-list)))))

(defun combobulate-query-builder-fontify-query (query-string parser)
  "Fontify QUERY-STRING using Combobulate's query font locking rules.
Argument PARSER sets the buffer to the parser instance to use for fontifying."
  (with-temp-buffer
    (let ((combobulate-query-builder-parser parser)) (insert query-string)
         (combobulate-query-mode)
         (font-lock-default-function 1)
         (font-lock-default-fontify-region (point-min) (point-max) nil)
         (buffer-string))))

(defun combobulate-query-builder-copy-and-match-node-at-point ()
  "Copy the node at point, match its text, and insert it into the query builder."
  (interactive)
  (combobulate-query-builder-live-or-error)
  (let ((node (with-current-buffer combobulate-query-builder-target-buffer-name
                (combobulate-proffer-choices
                 (cons (combobulate-node-at-point nil t)
                       (combobulate-get-parents (combobulate-node-at-point nil t)))
                 #'combobulate-proffer-action-highlighter
                 :prompt-description "Pick a node to copy and match"))))
    (unless node
      (user-error "No node selected"))
    (combobulate-query-builder-insert
     (combobulate-query-builder-to-string
      `((,(combobulate-query--node-type node))
        ,@(combobulate-query-builder-match-form
           '@hl.default (combobulate-query-builder-get-node-text node)))))))

(defun combobulate-query-builder-copy-node-hierarchy-at-point ()
  "Select a node hierarchy starting at point and copy it into the query builder."
  (interactive)
  (combobulate-query-builder-live-or-error)
  (with-current-buffer combobulate-query-builder-target-buffer-name
    (let* ((point-node (combobulate-node-at-point nil t))
           (parent-node
            (combobulate-proffer-choices
             (cons point-node (combobulate-get-parents point-node))
             #'combobulate-proffer-action-highlighter
             :prompt-description "Pick a node hierarchy to copy")))
      (unless parent-node
        (user-error "No node selected"))
      ;; check if we have a `node' slot in the parent
      ;; node. `combobulate-proffer-choices' only returns proxy nodes.
      (setq parent-node (combobulate-proxy-node-node parent-node))
      (combobulate-query-builder-insert
       (combobulate-query-builder-to-string
        (combobulate-build-nested-query
         (cons point-node
               (combobulate-get-parents-until point-node parent-node))))))))

(defun combobulate-xref-backend ()
  "Combobulate xref backend.

This is let-bound by Combobulate for its own xref command
wrappers, and it is not intended to be added to
`xref-backend-functions' as a valid backend."
  'combobulate)

(defun combobulate-xref-summary (node)
  "Build a summary for NODE.

Combobulate will seek out parents of NODE until it finds a node
that exceeds a single line of text in the target buffer.

That node's text is then truncated to 100 characters."
  (combobulate-string-truncate
   (combobulate-node-text
    ;; Try to find the largest parent node of node that fills a single
    ;; line.
    (or (combobulate-parent-while node #'combobulate-node-occupies-single-line-p)
        node)
    t)
   100))

(defvar combobulate-xref-summary-function #'combobulate-xref-summary
  "Function to use for `xref-backend-references'.")

(defvar combobulate-xref-buffer nil
  "Buffer to use for `xref-backend-references'.")

(defvar combobulate-xref-query nil
  "Query to use for `xref-backend-references'.")

(defvar combobulate-query-builder-history nil
  "History for `combobulate-query-builder'.

This variable is also used for other prompts that take as input a
query.")

(cl-defmethod xref-backend-references ((_backend (eql combobulate)) _)
  "Using `combobulate-xref-query', find all nodes and return them.

The `combobulate-xref-buffer' is the buffer that is used to
associate the captured nodes."
  (mapcar
   (lambda (node)
     (xref-make-match
      (or (funcall combobulate-xref-summary-function node)
          (combobulate-pretty-print-node node))
      (xref-make-buffer-location combobulate-xref-buffer (combobulate-node-start node))
      (- (combobulate-node-end node) (combobulate-node-start node))))
   ;; BUG: Skip the capture group names. Calling the underlying
   ;; `treesit-query-capture' and telling it to skip the node labels
   ;; can cause spurious query errors?! Weirdly, this does not happen
   ;; if we *do* include the labels.
   (sort (mapcar #'cdr (combobulate-query-capture
                        (combobulate-root-node)
                        combobulate-xref-query))
         #'combobulate-node-before-node-p)))

(defun combobulate-xref-find-query-buffer-references (query &optional buffer)
  "Find all QUERY references in BUFFER.

The underlying xref backend will source the query from
`combobulate-xref-query'. If it is nil, instead use the current
query in the ring.

If BUFFER is nil, the current buffer is used."
  (interactive (list (combobulate-query-ring-current-query)
                     (current-buffer)))
  (pop-to-buffer buffer)
  (let ((xref-backend-functions '(combobulate-xref-backend))
        (xref-show-xrefs-function (or combobulate-xref-show-xrefs-function
                                      xref-show-xrefs-function))
        (combobulate-xref-buffer buffer)
        (combobulate-xref-query query))
    (xref-find-references query)))

(defun combobulate-query-builder-match-node-at-point ()
  "Match the node at point and insert it into the query builder."
  (interactive)
  (let ((combobulate-query-default-query ""))
    (combobulate-query-builder))
  (combobulate-query-builder-copy-and-match-node-at-point))

(defun combobulate-query-builder-insert (query-string &optional clear-buffer)
  "Insert QUERY-STRING into the query builder buffer and maybe CLEAR-BUFFER."
  (with-current-buffer combobulate-query-builder-buffer-name
    (when clear-buffer (erase-buffer))
    (insert (string-trim (or query-string "")))
    (combobulate-query-builder--after-change)
    (when combobulate-query-builder-fit-to-window
      (fit-window-to-buffer))))

(defun combobulate-query-ring--execute (ask-prompt message fn)
  "Execute FN on the active query ring entry with ASK-PROMPT and MESSAGE.

ASK-PROMPT is the prompt to ask the user before executing FN on
the query. MESSAGE is the message to display after executing FN
on the query.

FN is a function that takes two arguments: the matches and the
query."
  (when (ring-empty-p combobulate-query-ring)
    (error "The query ring is empty. Insert a query with \\[combobulate-query-builder]"))
  (let* ((query (combobulate-query-ring-current-query))
         (pretty-query (combobulate-query-pretty-print query))
         (matches))
    (when (yes-or-no-p (format (concat ask-prompt "\n%s") pretty-query))
      (setq matches (combobulate-query-capture (combobulate-root-node) query))
      (prog1 (funcall fn matches query)
        (combobulate-message
         (format (concat message ": %s")
                 (combobulate-tally-nodes matches t)
                 pretty-query))))))

(defun combobulate-query-ring-current-query ()
  "Return the current query from `combobulate-query-ring'."
  (ring-ref combobulate-query-ring combobulate-query-ring-index))

(defun combobulate-query-ring--message ()
  (combobulate-message
   (format "Query [%d/%d]: %s"
           (mod (1+ combobulate-query-ring-index) (ring-length combobulate-query-ring))
           (1- (ring-length combobulate-query-ring))
           (combobulate-query-pretty-print
            (combobulate-query-ring-current-query)
            combobulate-query-builder-parser))))

(defun combobulate-query-ring-next-query ()
  "Insert the next query from `combobulate-query-ring' into the query builder."
  (interactive)
  (cl-incf combobulate-query-ring-index)
  (when (combobulate-query-builder-live-p)
    (combobulate-query-builder-insert
     (ring-ref combobulate-query-ring combobulate-query-ring-index) t))
  (combobulate-query-ring--message))

(defun combobulate-query-ring-previous-query ()
  "Insert the previous query from `combobulate-query-ring' into the query builder."
  (interactive)
  (cl-decf combobulate-query-ring-index)
  (when (combobulate-query-builder-live-p)
    (combobulate-query-builder-insert
     (ring-ref combobulate-query-ring combobulate-query-ring-index)
     t))
  (combobulate-query-ring--message))

(defun combobulate-query-ring-save-query (&optional query)
  "Update `combobulate-query-ring', optionally with QUERY.

If QUERY is nil, then source the query from the query builder
buffer."
  (interactive)
  (ring-insert combobulate-query-ring (or query (combobulate-query-builder-get-query)))
  (setq combobulate-query-ring-index 0)
  (combobulate-message "Saved query to ring"))

(defun combobulate-query-builder-edit-nodes ()
  "Edit the nodes in the query builder buffer."
  (interactive)
  (combobulate-query-builder-live-or-error)
  (combobulate-query-ring-save-query)
  (pop-to-buffer combobulate-query-builder-target-buffer-name)
  (let* ((query (combobulate-query-builder-get-query))
         (nodes (combobulate-query-capture
                 (combobulate-root-node)
                 query nil nil nil)))
    ;; Search the query string for `@before', `@after', and `@mark' capture
    ;; groups. Raise an error if neither are present.
    (unless (or (string-match-p "@before" query)
                (string-match-p "@after" query)
                (string-match-p "@mark" query))
      (error "Please mark nodes with `@before', `@after', and/or `@mark' to determine cursor placement"))
    (if (>= (length nodes) combobulate-query-builder-edit-max-nodes)
        (error "Too many nodes to edit (%d)" (length nodes))
      (combobulate-edit-nodes nodes))))

(defun combobulate-query-builder-get-query ()
  "Get the current query from the query builder buffer."
  (with-current-buffer combobulate-query-builder-buffer-name
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun combobulate-query-builder-install-into-buffer (buffer query &optional language)
  "Install QUERY as a highlighter in the target BUFFER."
  (interactive (list
                (if (buffer-live-p (get-buffer combobulate-query-builder-target-buffer-name))
                    (get-buffer combobulate-query-builder-target-buffer-name)
                  (error "No target buffer found"))
                (combobulate-query-builder-get-query)))
  (with-current-buffer buffer
    (combobulate-query-ring-save-query query)
    (when (combobulate-highlight-install-query
           query
           (or language (combobulate-parser-language combobulate-query-builder-parser)))
      (combobulate-message "Installing query into buffer..."))))

(defun combobulate-query-builder-find-in-xref ()
  "Find all references to the current query in the target buffer."
  (interactive)
  (combobulate-query-ring-save-query)
  (combobulate-xref-find-query-buffer-references
   (combobulate-query-builder-to-string (combobulate-query-builder-get-query))
   combobulate-query-builder-target-buffer-name))

(defun combobulate-query-builder-install-into-file-local ()
  "Install the current query as a file local variable."
  (interactive)
  (combobulate-query-builder-live-or-error)
  (combobulate-query-ring-save-query)
  (with-current-buffer combobulate-query-builder-buffer-name
    (combobulate-query-builder-update-variable
     'file
     nil (combobulate-parser-language combobulate-query-builder-parser)
     (combobulate-query-builder-get-query)))
  (combobulate-message "Installing query into a file local variable..."))

(defun combobulate-query-builder-install-into-dir-local ()
  "Install the current query as a highlighter in a directory local variable."
  (interactive)
  (combobulate-query-builder-live-or-error)
  (combobulate-query-ring-save-query)
  (combobulate-query-builder-update-variable
   'directory
   nil (combobulate-parser-language combobulate-query-builder-parser)
   (combobulate-query-builder-get-query))
  (combobulate-message "Installing query into directory local variable..."))

(defun combobulate-query-builder-update-variable (target mode language query)
  "Update the file or directory local TARGET for MODE with LANGUAGE and QUERY."
  (with-current-buffer combobulate-query-builder-target-buffer-name
    (let ((queries (append combobulate-highlight-queries-alist
                           `((:language ,language :query ,query)))))
      (if (eq target 'file)
          (modify-file-local-variable 'combobulate-highlight-queries-alist queries 'add-or-replace)
        (modify-dir-local-variable mode 'combobulate-highlight-queries-alist queries 'add-or-replace)))))

(defvar combobulate-query-syntax-table
  (let ((tbl (make-syntax-table scheme-mode-syntax-table)))
    (modify-syntax-entry ?# "_" tbl)
    tbl)
  "Syntax table for `combobulate-query-mode'.")

(defun combobulate-query-builder-copy-query-as-form ()
  "Copy the current query as a form to the kill ring."
  (interactive)
  (combobulate-query-ring-save-query)
  (kill-new (treesit-pattern-expand (combobulate-query-builder-get-query)))
  (combobulate-message "Copied query as form to kill ring"))

(defun combobulate-query-builder-copy-query ()
  "Copy the current query to the kill ring."
  (interactive)
  (combobulate-query-ring-save-query)
  (kill-new (combobulate-query-builder-get-query))
  (combobulate-message "Copied query to kill ring"))

(transient-define-prefix combobulate-query-builder-popup ()
  "Query builder."
  ["Query Builder"
   ["Actions"
    ("n" "Copy and match node at point" combobulate-query-builder-copy-and-match-node-at-point)
    ("h" "Copy node hierarchy at point" combobulate-query-builder-copy-node-hierarchy-at-point)
    ("p" "Change parser" combobulate-query-builder-change-parser)
    ("M-w" "Copy query as form to kill ring" combobulate-query-builder-copy-query-as-form)
    ("w" "Copy query to kill ring" combobulate-query-builder-copy-query)
    ("+" "Fit window to buffer" fit-window-to-buffer)]
   ["History"
    ("C-s" "Save query to ring" combobulate-query-ring-save-query)
    ("M-n" "Next query in ring" combobulate-query-ring-next-query :transient t)
    ("M-p" "Previous query in ring" combobulate-query-ring-previous-query :transient t)]]
  ["Match Actions"
   ["Editing"
    ("e" "Edit Nodes" combobulate-query-builder-edit-nodes)
    ("x" "Find in Xref" combobulate-query-builder-find-in-xref)]
   ["Highlighting"
    ("d" "Add directory local variable" combobulate-query-builder-install-into-dir-local)
    ("f" "Add file local variable" combobulate-query-builder-install-into-file-local)
    ("b" "Install into buffer" combobulate-query-builder-install-into-buffer)]])

(defvar combobulate-query-mode-map
  (let ((map (copy-keymap scheme-mode-map)))
    (define-key map (kbd combobulate-key-prefix) #'combobulate-query-builder-popup)
    (define-key map (kbd "C-c p") #'combobulate-query-builder-change-parser)
    (define-key map (kbd "C-c n") #'combobulate-query-builder-copy-and-match-node-at-point)
    (define-key map (kbd "C-c f") #'combobulate-query-builder-install-into-file-local)
    (define-key map (kbd "C-c d") #'combobulate-query-builder-install-into-dir-local)
    (define-key map (kbd "C-c e") #'combobulate-query-builder-edit-nodes)
    (define-key map (kbd "C-c b") #'combobulate-query-builder-install-into-buffer)
    (define-key map (kbd "C-c x") #'combobulate-query-builder-find-in-xref)
    (define-key map (kbd "C-c h") #'combobulate-query-builder-copy-node-hierarchy-at-point)
    (define-key map (kbd "C-c +") #'fit-window-to-buffer)
    (define-key map (kbd "C-c q") #'combobulate-query-builder-save-and-quit)
    (define-key map (kbd "C-c M-p") #'combobulate-query-ring-previous-query)
    (define-key map (kbd "C-c M-n") #'combobulate-query-ring-next-query)
    (define-key map (kbd "C-c C-s") #'combobulate-query-ring-save-query)
    (define-key map (kbd "M-<up>") #'raise-sexp)
    map)
  "Keymap for `combobulate-query-mode'.")

(defun combobulate-query-builder-live-p ()
  "Return t if the query builder buffer is live."
  (buffer-live-p (get-buffer combobulate-query-builder-buffer-name)))

(defun combobulate-query-builder-live-or-error ()
  "Signal an error unless the query builder buffer is live."
  (unless (combobulate-query-builder-live-p)
    (user-error "No query builder buffer found")))

(defun combobulate-query-builder-save-and-quit ()
  "Save the query and quit the query builder buffer."
  (interactive)
  (combobulate-query-builder-live-or-error)
  (with-current-buffer combobulate-query-builder-target-buffer-name
    (when (yes-or-no-p "Save current query to ring ?")
      (combobulate-query-ring-save-query)))
  (kill-buffer combobulate-query-builder-buffer-name))

;;;###autoload
(defun combobulate-query-builder ()
  "Open an interactive query builder buffer.

Design and test tree-sitter queries interactively with syntax
highlighting and node completion."
  (interactive)
  (let* ((target-buffer (current-buffer))
         (builder-buffer (get-buffer-create combobulate-query-builder-buffer-name)))
    (when (eq target-buffer builder-buffer)
      (user-error "This buffer cannot be use as target buffer"))
    (with-current-buffer target-buffer
      (setq combobulate-query-builder-target-buffer-name target-buffer)
      (combobulate-query-builder-change-parser)
      (add-hook 'combobulate-query-builder-after-change-functions
                #'combobulate-query-builder--after-change nil :local))
    (with-current-buffer builder-buffer
      (erase-buffer)
      (combobulate-query-mode)
      (add-hook 'after-change-functions #'combobulate-query-builder--after-change nil t)
      (add-hook 'kill-buffer-hook #'combobulate-query-builder--clean-target-buffer nil t)
      (if combobulate-query-default-query
          (combobulate-query-builder-insert combobulate-query-default-query)
        (newline)
        (insert "()")
        (forward-char -1)))
    (display-buffer builder-buffer
                    `(display-buffer-below-selected
                      (window-height . fit-window-to-buffer)
                      (window-min-height . 3)
                      (inhibit-same-window . t)))
    ;; Switch focus to the query builder window.
    (select-window (get-buffer-window builder-buffer))))

(define-derived-mode combobulate-query-mode prog-mode "Combobulate Query Builder"
  "Major mode for interactively constructing tree-sitter queries.

To use, call \\[combobulate-query-builder]."
  :syntax-table combobulate-query-syntax-table
  :interactive nil
  :group 'combobulate
  :abbrev-table scheme-mode-abbrev-table
  (let ((parser-lang-name (combobulate-parser-language combobulate-query-builder-parser)))
    (setq combobulate-query-builder-rules (combobulate-production-rules-get-rules parser-lang-name))
    (setq combobulate-query-builder-rule-names (combobulate-production-rules-get-types parser-lang-name))
    (setq combobulate-query-builder-field-names
          (mapcar #'combobulate-query-builder-prop-name-to-field-name
                  (seq-remove (lambda (prop) (equal prop :*unnamed*))
                              (seq-uniq (mapcan #'map-keys (mapcar #'cadr (combobulate-production-rules-get-rules parser-lang-name)))))))
    (setq-local comment-start ";"
                comment-end "")
    (setq-local completion-at-point-functions '(combobulate-query-builder-completion-at-point-function))
    (combobulate-query-builder-update-header "Ready")
    (setq-local combobulate-query-builder-font-lock-keywords
                ;; all the various supported rule names
                `((,(regexp-opt combobulate-query-builder-rule-names)
                   . 'combobulate-query-query-constant-face)
                  ;; `field:' names
                  (,(regexp-opt
                     combobulate-query-builder-field-names)
                   . 'combobulate-query-query-keyword-face)
                  ,@combobulate-query-match-face-alist
                  ;; @match capturing groups
                  ("#\\([a-zA-Z0-9-_?]+\\)" . 'combobulate-query-query-builtin-face)
                  ;; #predicates
                  ("#\\([a-zA-Z0-9-_?]+\\)" . 'combobulate-query-query-predicate-builtin-face)
                  ;; misc. quantifiers and operators
                  ("\\([?!+*]\\)" . 'combobulate-query-query-doc-markup-face)
                  ;; match `.' anchors
                  ("\\Sw<\\([.]\\)\\Sw" . 'combobulate-query-query-constant-face)
                  ;; strings
                  ("\"\\([^\"]*\\)\"" . 'combobulate-query-query-string-face)
                  ;; anonymous named and unnamed nodes
                  ("\\((_)\\)\\|\\(\s-+_\s-+\\)" . 'combobulate-query-query-anonymous-face)))
    (setq-local font-lock-defaults '(combobulate-query-builder-font-lock-keywords t))))


(defvar combobulate-highlight-face-name-history nil
  "History of face names used in `combobulate-highlight-read-face-name'.")

(defun combobulate-highlight-read-face-name (prompt &optional default)
  "Like `read-face-name', but lists only Combobulate faces.

Sources face capture aliases from `combobulate-query-match-face-alist'.
Argument PROMPT controls the prompt to display.
Optional argument DEFAULT sets the default value in the prompt."
  (let ((prompt (if default (format-prompt prompt default)
                  (format "%s: " prompt)))
        (completion-extra-properties
         '(:affixation-function
           (lambda (faces)
             (mapcar
              (lambda (face)
                (list face
                      (concat (propertize "COMBOBULATE"
                                          'face (map-elt combobulate-query-match-face-alist face))
                              "\t")
                      ""))
              faces))))
        (faces (map-keys combobulate-query-match-face-alist)))
    (let ((face (completing-read
                 prompt
                 faces
                 nil t nil nil default)))
      (make-symbol face))))

(defun combobulate-query-builder-capture-group-fn (capture-group-name match-text match-depth)
  "Build a query for a capture group named CAPTURE-GROUP-NAME matching MATCH-TEXT.

Note that CAPTURE-GROUP-NAME must begin with a `@', as per the
rules of Tree-sitter's query language.

MATCH-DEPTH should be the depth at which the matcher is
installed, relative to the number of parent nodes, usually 0 or 1.

If STRING-STYLE is non-nil, use `@match' instead of `:match'."
  (lambda (_node _rest-nodes before ct)
    (when (and (not before) (= ct match-depth))
      ;; build the query here. we want the matcher
      ;; placed after the match-node (hence `(not
      ;; before)')
      (combobulate-query-builder-match-form capture-group-name match-text))))

(defun combobulate-query-builder-match-form (capture-group-name match-text &optional no-escape)
  "Build the query form with CAPTURE-GROUP-NAME matching MATCH-TEXT.
Optional argument NO-ESCAPE disables regexp quoting."
  `(,capture-group-name
    (:match ,(concat "^" (if no-escape
                             match-text
                           (regexp-quote match-text))
                     (if (string-match-p "\n" match-text) "" "$"))
            ,capture-group-name)))

(defun combobulate-query-builder-matcher-query (match-node match-text strict-match face
                                                           &rest parent-match-nodes)
  "Build a nested query for MATCH-NODE matching MATCH-TEXT and set with FACE.

STRICT-MATCH is a boolean indicating whether any parent match
nodes should be wildcard nodes or not."
  ;; Construct the (optionally nested) query for the match node.
  ;; The general format is:
  ;;
  ;;   (parent_n
  ;;     (parent_n-1
  ;;       ( ...
  ;;         (parent_1
  ;;           (match-node) @match-group-name
  ;;           (#match "some-str" @match-group-name))))
  (let* ((query (combobulate-build-nested-query
                 (cons match-node
                       (if strict-match parent-match-nodes
                         (make-list (length parent-match-nodes) '_)))
                 (combobulate-query-builder-capture-group-fn
                  '@combobulate-match match-text (length parent-match-nodes))))
         (fixed-query (combobulate-query-builder-expand-match-face
                       (combobulate-query-builder-to-string query)
                       `(("combobulate-match" . ',face)))))
    fixed-query))

(defun combobulate-highlight-dwim-at-point (face strict)
  "DWIM and build a query that highlight nodes with FACE.

If STRICT is non-nil, then the parent node of the node at point
is used to constrain the search.  Otherwise, the parent node is
marked `_' indicating a wildcard.

Uses `combobulate-navigation-context-nodes' to determine the right
node at point to highlight."
  (interactive (list (combobulate-highlight-read-face-name "Face: ")
                     (y-or-n-p "Match similar types only? ")))
  ;; determine the right node at point to collect.
  (let ((node (with-navigation-nodes (:nodes combobulate-navigation-context-nodes)
                (car-safe
                 (seq-sort #'combobulate-node-larger-than-node-p
                           (seq-filter #'combobulate-navigable-node-p
                                       (cons (combobulate-node-at-point nil t)
                                             (combobulate-get-parents (combobulate-node-at-point nil t))))))))
        (query))
    (unless node (error "Cannot find a valid context node at point"))
    (with-navigation-nodes (:nodes combobulate-navigation-parent-child-procedures)
      (if-let ((node-parent (combobulate-node-parent node)))
          (progn
            (setq query (combobulate-query-builder-matcher-query
                         node
                         (combobulate-query-builder-get-node-text node)
                         strict
                         face
                         node-parent))
            (combobulate-query-builder-install-into-buffer
             (current-buffer)
             query
             (combobulate-parser-language (combobulate-parser-node node)))
            (combobulate-message
             (format "Installing %s query:\n %s"
                     (if strict "strict" "lax")
                     (combobulate-query-builder-fontify-query query (combobulate-parser-node node)))))
        (error "Cannot derive a parent node from the current node")))))

(defvar combobulate-highlight-feature-symbol 'combobulate-highlight
  "The symbol used to identify the font lock feature for Combobulate highlighting.

This is used to identify the font lock feature that is used to
highlight Combobulate highlighters.")

(defun combobulate-highlight-setup ()
  "Setup the highlighting for the current buffer."
  ;; Add the `combobulate-highlight-feature-symbol' feature to the first level of the
  ;; font lock feature list. This is I think the only way to inject
  ;; custom font lock rules into the tree-sitter-powered font lock
  ;; engine.
  (when treesit-font-lock-feature-list
    (unless (member combobulate-highlight-feature-symbol (car treesit-font-lock-feature-list))
      (push combobulate-highlight-feature-symbol (car treesit-font-lock-feature-list)))))

(defun combobulate-highlight-query ()
  (interactive)
  (combobulate-query-ring--execute
   "Highlight nodes matching this query?"
   "Highlighted nodes"
   (lambda (_matches query)
     (combobulate-highlight-install-query
      (combobulate-query-builder-expand-match-face query)
      (combobulate-parser-language (car (combobulate-parser-list)))))))

(defun combobulate-highlight-install-query (query language &optional quiet)
  "Highlight the QUERY in LANGUAGE in the current buffer.

If QUIET is non-nil, then do not display any warning messages if
the query fails to compile."
  (if-let (err (combobulate-query-builder-validate-query query))
      (progn (unless quiet (warn "Query %s failed to compile: %s." query err)) nil)
    (setq treesit-font-lock-settings
          (append treesit-font-lock-settings
                  (treesit-font-lock-rules
                   :language language
                   :override t
                   :feature combobulate-highlight-feature-symbol
                   query)))
    ;; required for the font lock machinery to take effect.
    (treesit-font-lock-recompute-features)
    (font-lock-flush)
    query))

(defun combobulate-highlight-install (language)
  "Install the font lock rules for LANGUAGE in the current buffer."
  (when combobulate-mode
    (combobulate-highlight-setup)
    ;; do the user-defined rules...
    (dolist (rule combobulate-highlight-queries-alist)
      (when (eq language (plist-get rule :language))
        (combobulate-highlight-install-query
         (combobulate-query-builder-expand-match-face
          (combobulate-query-builder-to-string (plist-get rule :query)))
         language t)))
    ;; next, the system-supplied rules...
    (dolist (rule combobulate-highlight-queries-default)
      (combobulate-highlight-install-query
       (combobulate-query-builder-expand-match-face
        (combobulate-query-builder-to-string rule))
       language t))))

(defun combobulate-highlight-clear ()
  "Clear all Combobulate highlight in the current buffer."
  (interactive)
  (setq treesit-font-lock-settings
        (seq-remove (lambda (setting) (seq-let [_ _ feature _] setting
                                   (eq feature combobulate-highlight-feature-symbol)))
                    treesit-font-lock-settings))
  (treesit-font-lock-recompute-features)
  (font-lock-flush)
  (combobulate-message "Cleared all Combobulate highlights."))


(provide 'combobulate-query)
;;; combobulate-query.el ends here
