;;; combobulate-test-prelude.el --- prelude for combobulate tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

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

;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'bookmark)
(require 'ert)
;;; required to make major modes load
(require 'treesit)
(require 'typescript-ts-mode)
(require 'css-mode)
(require 'python)
(require 'js)
(require 'yaml-ts-mode)
(require 'json-ts-mode)


(defvar combobulate--test-point-categories '((outline . ("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩")))
  "Alist of categories and their overlay characters.

The overlay characters are used to indicate combobulate test
point markers in a buffer..")

(defvar-local combobulate-test-point-overlays nil
  "List of overlays used to display point helpers in test fixture buffers.

Format is an alist of (OVERLAY-STRING-CHAR . (OVERLAY NUMBER CATEGORY ORIG-PT)).")

(defun combobulate--with-test-overlays (&optional fn)
  "Call FN for each overlay related to Combobulate's test fixtures.

If FN is nil, just return the overlays. "
  (let ((ovs (seq-filter
              (lambda (ov)
                (overlay-get ov 'combobulate-test))
              (car (overlay-lists)))))
    (when fn
      (dolist (ov ovs)
        (funcall fn ov)))
    ovs))

(defun combobulate--test-get-overlay-by-number (number)
  "Get the overlay at point."
  (let ((match))
    (combobulate--with-test-overlays
     (lambda (ov)
       (when (eq (overlay-get ov 'combobulate-test-number) number)
         (cl-assert (not match) nil "Multiple overlays found for number %s" number)
         (setq match ov))))
    match))

(defun combobulate--test-get-overlay-at-point (number)
  "Get the overlay at point."
  (when-let (ov (combobulate--test-get-overlay-by-number number))
    (when (= (overlay-start ov) (point)) ov)))

(defun combobulate--test-delete-overlay (&optional number)
  "Delete overlays related to Combobulate's test fixtures with NUMBER.

If NUMBER is nil, delete all `combobulate-test' overlays."
  (combobulate--with-test-overlays
   (lambda (ov)
     (when (or (not number)
               (equal (overlay-get ov 'combobulate-test-number) number))
       (delete-overlay ov)))))

(defun combobulate-test-delete-all-overlays ()
  "Delete all overlays related to Combobulate's test fixtures."
  (interactive)
  (save-excursion
    (setq combobulate-test-point-overlays nil)
    (combobulate-test-update-file-local-variable)
    (combobulate--test-delete-overlay)))

(defun combobulate-test-go-to-marker (number)
  (if-let (ov (combobulate--test-get-overlay-by-number number))
      (progn
        (goto-char (overlay-start ov))
        (combobulate-test-assert-at-marker number))
    (error "No overlay found for number %s" number)))

(defun combobulate-test-assert-at-marker (number)
  (if-let (ov (combobulate--test-get-overlay-by-number number))
      (let ((ov-number (overlay-get ov 'combobulate-test-number)))
        (progn
          (unless (= ov-number number)
            (ert-fail (list "Expected marker with number `%d' but got `%d'" number ov-number)))
          (unless (eq (overlay-start ov) (point))
            (ert-fail (list "Overlay `%d' is not at point `%s'" number (point))))))
    (error "No overlay found for number %s" number)))

(defun combobulate--test-place-overlay (string-char number category &optional orig-pt)
  "Place a numbered overlay at point.

STRING-CHAR is the character to display.  NUMBER is the number to
display.  CATEGORY is the category of overlay to display.
ORIG-PT is the point to display the overlay at. If it is nil, the
current point is used."
  (let* ((pt (or orig-pt (point)))
         (ov (make-overlay pt (1+ pt) nil t nil)))
    (overlay-put ov 'display string-char)
    (overlay-put ov 'combobulate-test t)
    (overlay-put ov 'combobulate-test-number number)
    (overlay-put ov 'combobulate-test-category category)
    (overlay-put ov 'face 'combobulate-refactor-cursor-face)
    ov))

(defun combobulate--test-place-category-overlay (number category &optional orig-pt)
  "Place a numbered overlay at point.

NUMBER is the number to display.  CATEGORY is the category of
overlay to display.  ORIG-PT is the point to display the
overlay at. If it is nil, the current point is used."
  (combobulate--test-place-overlay
   (nth (1- number) (alist-get category combobulate--test-point-categories))
   number 'outline orig-pt))

(defun combobulate-test-go-to-overlay (arg)
  "Go to a point marker overlay, ARG, in the current buffer."
  (interactive "P")
  (let ((ov (combobulate--test-get-overlay-by-number (or arg 1))))
    (when ov
      (goto-char (overlay-start ov)))))

(defun combobulate-test-place-next-overlay (arg &optional category)
  "Place the next numbered overlay."
  (interactive "P")
  (save-excursion
    (let* ((category (or category 'outline))
           (ovs (combobulate--with-test-overlays))
           (next-number (or arg (1+ (seq-max (or (mapcar (lambda (ov)
                                                           (overlay-get ov 'combobulate-test-number))
                                                         ovs)
                                                 '(0)))))))
      (when (> next-number 10)
        (error "Too many overlays"))
      (combobulate--test-delete-overlay next-number)
      (combobulate--test-place-category-overlay next-number category (point))
      (combobulate-test-update-file-local-variable))))

(cl-defmacro combobulate-with-stubbed-prompt-expansion (&rest body)
  "Stub out the prompt expansion function for testing purposes.

This is a macro that stubs out the prompt expansion function
`combobulate-envelope-prompt-expansion' for testing purposes.  It
temporarily replaces the function with a lambda that returns the
first value in the list `combobulate-envelope-prompt-expansion-actions'.

This macro is intended to be used in conjunction with
`combobulate-with-stubbed-envelope-prompt'."
  (declare (indent 1) (debug (sexp body)))
  `(cl-letf (((symbol-function 'combobulate-envelope-prompt-expansion)
              (lambda (prompt)
                (pcase (pop combobulate-envelope-prompt-expansion-actions)
                  ('yes t)
                  ('no nil)
                  ((and (pred consp) fn) (funcall (car fn)))
                  ((and (pred functionp) fn) (funcall fn))
                  ((and (pred null)) (error "Expected prompt action but none available"))
                  (_ (error "Unknown prompt action"))))))
     ,@body))

(cl-defmacro combobulate-with-stubbed-envelope-prompt (&rest body)
  "Stub out the envelope prompt function for testing purposes.

This is a macro that stubs out the envelope prompt function
`combobulate-envelope-prompt' for testing purposes.  It
temporarily replaces the function with a lambda that returns the
first value in the list `combobulate-envelope-prompt-actions'
and then pops it off the list.  This allows us to simulate
multiple calls to the prompt function with different values.

The body of the macro is executed with the stubbed function in
place.

The `combobulate-envelope-prompt-actions' list may contain either
a string or a cons cell.  If it is a string, then the string is
returned as the prompt value.  If is a symbol, it is treated as
though it is a function and called."
  (declare (indent 1) (debug (sexp body)))
  `(cl-letf (((symbol-function 'combobulate-envelope-prompt)
              (lambda (prompt default-value &optional buffer update-fn)
                (pcase (pop combobulate-envelope-prompt-actions)
                  ((and (pred stringp) value) (funcall update-fn)
                   value)
                  ((and (pred consp) fn) (funcall (car fn)))
                  ((and (pred functionp) fn) (funcall fn))
                  ((and (pred null)) (error "Expected prompt action but none available"))
                  (_ (error "Unknown prompt action"))))))
     ,@body))

(cl-defmacro combobulate-with-stubbed-proffer-choices ((&key (choices nil) (error-if-missing t) (call-action-fn t)
                                                             (replacement-action-fn nil))
                                                       &rest body)
  "Stub out `combobulate-proffer-choices' to return one choice at a
time out of CHOICES.

CHOICES are 0-based and picked in order.

If ERROR-IF-MISSING is non-nil, signal an error if the choice in
CHOICES is greater than the number of nodes.

If CALL-ACTION-FN is non-nil, call the action function with the
node.

If REPLACEMENT-ACTION-FN is non-nil, use it instead of the action
function."
  (declare (indent 1) (debug (sexp body)))
  `(let ((choice-node) (current-choice) (remaining-choices ,choices))
     (cl-letf (((symbol-function 'combobulate-proffer-choices)
                (cl-defun stub-proffer-actions (nodes action-fn &key (first-choice nil)
                                                      (reset-point-on-abort t) (reset-point-on-accept nil)
                                                      (prompt-description nil)
                                                      (extra-map nil) (flash-node nil) (unique-only t)
                                                      (accept-action 'rollback)
                                                      (cancel-action 'commit)
                                                      (switch-action 'commit)
                                                      &allow-other-keys)
                  (cl-assert remaining-choices)
                  (setq current-choice (pop remaining-choices))
                  (when ,call-action-fn
                    (and ,error-if-missing (should (<= current-choice (length nodes))))
                    (let ((cg (prepare-change-group))
                          (picked-node (combobulate-proxy-node-make-from-nodes (nth current-choice nodes)))
                          (stubbed-refactor-id (combobulate-refactor-setup)))
                      (activate-change-group cg)
                      (setq choice-node picked-node)
                      (combobulate-refactor (:id stubbed-refactor-id)
                        (funcall (or ,replacement-action-fn action-fn)
                                 (combobulate-proffer-action-create
                                  :index current-choice
                                  :current-node (combobulate-proxy-node-make-from-nodes picked-node)
                                  ;; :proxy-nodes (combobulate-proxy-node-make-from-nodes nodes)
                                  :refactor-id stubbed-refactor-id))
                        (if (eq accept-action 'rollback)
                            (rollback)
                          (commit)))
                      (cancel-change-group cg)))
                  choice-node)))
       ,@body)))

(defun combobulate-test-update-file-local-variable ()
  "Update the file-local variable with the current overlays.

This function will repeatedly write out the variable to the
buffer until the overlay position markers stabilise as a result
of updating the prop line."
  (interactive)
  (save-excursion
    (let ((attempts-left 10))
      (delete-file-local-variable-prop-line 'eval)
      (add-file-local-variable-prop-line 'eval '(combobulate-test-fixture-mode t) nil)
      ;; repeatedly write out the variable until the overlay position
      ;; markers stabilise. I could just compare prev/next,
      ;; but... eh... this is fine... I guess..
      (while (> attempts-left 0)
        (setq attempts-left (1- attempts-left))
        (setq-local combobulate-test-point-overlays
                    (mapcar (lambda (ov)
                              (list (overlay-get ov 'combobulate-test-number)
                                    (overlay-get ov 'combobulate-test-category)
                                    (overlay-start ov)))
                            (combobulate--with-test-overlays)))
        (add-file-local-variable-prop-line 'combobulate-test-point-overlays combobulate-test-point-overlays nil)))))

(defun combobulate-test-apply-file-local-variable ()
  "Apply the file-local variable to the current buffer."
  (interactive)
  (when-let ((v (assoc 'combobulate-test-point-overlays file-local-variables-alist)))
    (setq-local combobulate-test-point-overlays (cdr v))
    (combobulate--test-delete-overlay)
    (pcase-dolist (`(,number ,category ,pt ) combobulate-test-point-overlays)
      (combobulate--test-place-category-overlay number category pt))))

(defun combobulate-test-visit-fixture-deltas ()
  (interactive)
  ;; search the ./test/fixture-deltas directory for files starting with our buffer-file-name
  (let* ((file-name (file-name-nondirectory buffer-file-name))
         (files (file-expand-wildcards (concat (expand-file-name (concat (file-name-directory buffer-file-name)
                                                                         "../fixture-deltas/"))
                                               "**/"
                                               file-name
                                               "*" ))))
    (if (not files)
        (message "No fixture deltas found for %s" file-name)
      ;; the files are all named fn[command@...] and we want to group by command
      (let ((grouped-cmd-files (seq-group-by (lambda (file)
                                               (let ((base (file-name-base file)))
                                                 (string-match (macroexpand `(rx ,file-name "[" (group (1+ (not "@"))) "@")) base)
                                                 (when-let (cmd (match-string 1 base))
                                                   cmd)))
                                             files)))
        (delete-other-windows)
        (mapc (lambda (buf) (display-buffer buf `(display-buffer-in-direction
                                             . ((direction . right)
                                                (window-width . 0.2)))))
              (mapcar #'find-file-noselect
                      (alist-get (completing-read "Pick command " (seq-uniq (mapcar #'car grouped-cmd-files) #'string=))
                                 grouped-cmd-files nil nil #'string=)))
        (balance-windows-area)))))

(defvar combobulate-test-fixture-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") #'combobulate-test-place-next-overlay)
    (define-key map (kbd "C-c C-a") #'combobulate-test-apply-file-local-variable)
    (define-key map (kbd "C-c C-u") #'combobulate-test-update-file-local-variable)
    (define-key map (kbd "C-c C-d") #'combobulate-test-delete-all-overlays)
    (define-key map (kbd "C-c v") #'combobulate-test-visit-fixture-deltas)
    map))

(define-minor-mode combobulate-test-fixture-mode "Load Combobulate's test fixture overlays"
  :init-value nil
  :lighter "Comb-Test"
  :keymap combobulate-test-fixture-mode-map
  (when combobulate-test-fixture-mode
    ;; hacky, but ensures that formatters aren't run which can muck up
    ;; the overlay positions in the file mode variable
    (setq-local before-save-hook nil)
    (setq-local after-save-hook nil)
    (setq-local after-change-functions nil)
    (combobulate-test-apply-file-local-variable)))

(put 'combobulate-test-point-overlays 'safe-local-variable #'listp)
(add-to-list 'safe-local-eval-forms '(combobulate-test-fixture-mode t))

(defun combobulate-test-skip-to-match (expr)
  "Skip to the next match of EXPR.

Returns nil if no match is found."
  (when-let (v (re-search-forward expr nil nil 1))
    (goto-char (match-beginning 0))
    nil))





(defvar ert--results-stats)

(defun combobulate-ert-diff-fixture ()
  "Diff the current test with the fixture."
  (interactive)
  (unless (eq major-mode 'ert-results-mode)
    (user-error "Not in ert-results-mode"))
  (let* ((test (ert--results-test-at-point-no-redefinition t))
         (stats ert--results-stats)
         (pos (ert--stats-test-pos stats test))
         (result (aref (ert--stats-test-results stats) pos)))
    (apply #'ediff-files
           (plist-get (cdadr (cl-etypecase result
                               (ert-test-failed-condition
                                (ert-test-result-with-condition-condition result))))
                      :files))))

(defun combobulate-test-compare-string-with-file (buf fn)
  "Compare the contents of of BUF with FN."
  ;; TODO: this is a bit of a hack, but sometimes the editing
  ;; procedures can leave the file without a trailing newline. This
  ;; fixes that
  (cl-flet ((ensure-final-newline ()
              (save-excursion (goto-char (1- (point-max)))
		              (unless (looking-at "\n")
                                (goto-char (point-max))
                                (insert ?\n)))))
    (should (file-exists-p fn))
    (let ((current-contents (with-current-buffer buf
                              (ensure-final-newline)
                              (buffer-substring-no-properties (point-min) (point-max))))
          (file-contents (with-temp-buffer
                           (insert-file-contents fn)
                           (ensure-final-newline)
                           (buffer-substring-no-properties (point-min) (point-max)))))
      (unless (string= current-contents file-contents)
        ;; save both file and buffer to two temporary files
        (let ((buf-fn (make-temp-file "combobulate-test-actual@"))
              (file-fn (make-temp-file "combobulate-test-expected@")))
          (with-temp-file buf-fn
            (insert current-contents))
          (with-temp-file file-fn
            (insert file-contents))
          (ert-fail
           (list
            nil
            :fail-reason "File contents differ"
            :point (point)
            :files (list buf-fn file-fn)
            :fixture-fn fn
            :statement `(string= current-contents file-contents))))))))

(defun combobulate-compare-action-with-fixture-delta (fixture-delta-fn)
  "Compare the output from the current buffer to a fixture-delta file."
  (combobulate-test-compare-string-with-file (current-buffer) fixture-delta-fn))

(defmacro combobulate-for-each-marker (action-fn &key (reverse nil))
  "Execute ACTION-FN for each marker in the current buffer.

The point is first moved to the start of the first marker. After
that, all remaining point markers are visited in order of their
given number. ACTION-FN is executed *before* each marker is
visited; then, the new position of point is checked to ensure it
matches the position of the next marker.

If REVERSE is non-nil, execute ACTION-FN in reverse order."
  (let ((ordered-ovs))
    (dotimes (number (length combobulate-test-point-overlays))
      (when-let (ov (combobulate--test-get-overlay-by-number (1+ number)))
        (push ov ordered-ovs)))
    (unless reverse
      (setq ordered-ovs (reverse ordered-ovs)))
    (let ((first-ov (pop ordered-ovs)))
      (goto-char (overlay-start first-ov))
      (should (= (point) (overlay-start first-ov)))
      (dolist (ov ordered-ovs)
        (funcall action-fn)
        (should (= (point) (overlay-start ov)))))))

(defun combobulate-test-1 (statements)
  "Execute a series of statements in the current buffer.

Each statement is either a string, or a symbol. If it is a
string, it is inserted into the buffer. If it is a symbol, it is
executed as a function. The following symbols are supported:

- `bob': go to the beginning of the buffer
- `goto-first-char': go to the first non-whitespace character
- `n>': insert a newline and indent
- `<-': dedent the current line
- `back': move the point back one character
- `forward': move the point forward one character
- `tab': call `insert-for-tab-command'
- `assert-at-marker': assert that the point is at the given marker
- `goto-marker': go to the given marker (without asserting)
- `debug-show': show the buffer in a new window and pause execution."
  (let ((modified) (stop-after-each-statement current-prefix-arg))
    (dolist (statement (if stop-after-each-statement
                           (seq-reduce (lambda (a b) (append (list 'debug-show) (list b) a)) statements nil)
                         statements))
      (pcase statement
        ((pred stringp) (push `(insert ,statement) modified))
        ('bob (push `(goto-char (point-min)) modified))
        ('goto-first-char (push `(progn (goto-char (point-min)) (skip-chars-forward " \n")) modified))
        ('n> (push '(newline-and-indent) modified))
        ('<- (push '(python-indent-dedent-line-backspace 1) modified))
        ('back (push '(forward-char -1) modified))
        ('forward (push '(forward-char 1) modified))
        ('tab (push '(indent-for-tab-command) modified))
        ((or `(assert-at-marker ,number) `(assert-at-marker ,number ,_))
         (push `(should (combobulate--test-get-overlay-at-point ,number)) modified))
        ((or `(goto-marker ,number) `(goto-marker ,number ,_))
         (push `(let ((ov (combobulate--test-get-overlay-by-number ,number)))
                  (should ov)
                  (goto-char (overlay-start ov)))
               modified))
        ('delete-markers (combobulate--test-delete-overlay))
        ('debug-show
         (push `(progn (pop-to-buffer (current-buffer))
                       (font-lock-mode 1)
                       (font-lock-fontify-buffer)
                       (font-lock-flush)
                       (message "Stopped. `C-M-c' to resume. Current point: %s" (point))
                       (pulse-momentary-highlight-one-line (point) 'highlight)
                       (recursive-edit))
               modified))
        (_ (push statement modified))))
    (reverse modified)))

(cl-defmacro combobulate-test ((&key (setup nil) (fixture nil) language mode) &rest body)
  "Run a combobulate test with a preconfigured buffer to execute BODY.

SETUP is a list of forms to run before the test body.

FIXTURE is the path (absolute or relative from the `tests/'
directory) to a file to load into the buffer before running the
test.

LANGUAGE is the language to use for the tree-sitter parser and
must match exactly.  MODE is the major mode to use for the
buffer.

The test BODY is executed in the context of the buffer.  The
buffer will have `combobulate-mode' enabled and
`combobulate-setup' will be called. Additionally, a number of
macros are available for use in the test body."
  (declare (indent 1) (debug (sexp body)))
  (let ((setup-stmts (symbol-value setup)))
    `(with-temp-buffer
       (delay-mode-hooks
         (let ((positions '())
               ;; disable noisy BS in python
               (python-indent-guess-indent-offset-verbose nil))
           (funcall #',mode)
           (treesit-parser-create ',language)
           (combobulate-mode)
           (combobulate-setup)
           (switch-to-buffer (current-buffer))
           (with-current-buffer (current-buffer)
             ;; the default directory must be set explicitly. Here
             ;; we'll use the root of the ./tests/ directory by
             ;; looking for the combobulate-test-prelude file
             ;; (setq default-directory (file-name-directory (locate-library "combobulate-test-prelude")))
             (should combobulate-mode)
             (should (treesit-parser-list))
             (erase-buffer)
             (when ,fixture
               (unless (file-exists-p ,fixture)
                 (error "Fixture file %s does not exist (default-directory: %s)" ,fixture ,default-directory))
               ;; load the fixture file
               (insert-file-contents ,fixture)
               ;; ensure everything's applied properly
               (combobulate-test-fixture-mode 1)
               (hack-local-variables)
               (hack-local-variables-apply)))
           (let ((offset 0)
                 ;; disables flashing nodes to echo area etc.
                 (combobulate-flash-node nil))
             ,@(combobulate-test-1 setup-stmts)
             ,@(combobulate-test-1 body)))))))

(defun combobulate-test-compare-structure (a b)
  (let ((ta (flatten-tree a))
        (tb (flatten-tree b)))
    (should (= (length ta) (length tb)))
    (cl-loop
     for elem-a in ta
     for elem-b in tb
     do
     (should (combobulate-node-eq elem-a elem-b)))))

(defconst combobulate-test-python
  '("try:
    a = 1
    b = 2
    # Comment here
    call_something()
    raise Foo()
except KeyError:
    sys.exit(1)
except ValueError:
    raise SomethingElseError('')
finally:
    do_something()
    "
    goto-first-char))

(defconst combobulate-test-python-large
  '("try:
    a = 1
    b = 2
    c = 3
    d = 4
    # Comment here
    call_something()
    if True:
        return 1
    if 1 + 1 == 3:
        return 2
except KeyError:
    sys.exit(1)
except ValueError:
    raise SomethingElseError('')
finally:
    do_something()
    "
    goto-first-char))


(defconst combobulate-test-python-nested
  '("
if True:
    if 1 + 1 == 2:
        print('Hello World!')
        try:
            raise KeyError('Some message')
        finally:
            return 1
        d = {
          1: 2,
          3: (4,
              5)
        }"
    goto-first-char))

(defconst combobulate-test-js-jsx
  '("
function foo() {
  const a = {
      bar: (a, b, c) => (a + b + c),
      fiz: 42,
      blah: \"some string!\",
      blah2: \"some other string!\",
      /* Comment */
      buzz: \"awsd\",
      /* Comment */
      moooo: \"how now\",
    }
}"
    goto-first-char))

(provide 'combobulate-test-prelude)
;;; combobulate-test-prelude.el ends here
