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

If FN is nil, just return the overlays."
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

(defun combobulate--test-place-overlay (string-char number category &optional orig-pt)
  "Place a numbered overlay at point.

STRING-CHAR is the character to display.  NUMBER is the number to
display.  CATEGORY is the category of overlay to display.
ORIG-PT is the point to display the overlay at. If it is nil, the
current point is used."
  (let* ((pt (or orig-pt (point)))
         (ov (make-overlay pt (1+ pt))))
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
      (when (>= next-number 10)
        (error "Too many overlays"))
      (combobulate--test-delete-overlay next-number)
      (combobulate--test-place-category-overlay next-number category (point))
      (combobulate-test-update-file-local-variable))))

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


(defun combobulate-test-generate-fixture-diff-filename (fn action-string number suffix)
  "Generate a filename for a fixture diff file."
  (concat (file-name-nondirectory fn)
          "[" (format "%s@%s~%s" action-string number suffix) "]"
          "."
          (file-name-extension fn)))

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
              (save-excursion
                (unless (/= (char-after (1- (point-max))) ?\n)
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
        (let ((buf-fn (make-temp-file "combobulate-test-buf"))
              (file-fn (make-temp-file "combobulate-test-fixture")))
          (with-current-buffer buf
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

(defun combobulate-test-get-fixture-directory ()
  "Get the directory for the fixture files."
  (expand-file-name "./tests/fixtures/"))

(defun combobulate-test-get-test-directory ()
  "Get the directory for the test files."
  (expand-file-name "./"))

(defun combobulate-test-get-fixture-deltas-directory (&optional action-subdir ensure-dir)
  "Get the directory for the fixture deltas.

If ACTION-SUBDIR is non-nil, then the directory will be
<fixture-directory>/<action-subdir>/, otherwise it will be
<fixture-directory>/.

If ENSURE-DIR is non-nil, then the directory will be created if it
doesn't exist."
  (let* ((dir (if action-subdir
                  (expand-file-name (format "./fixture-deltas/%s/" action-subdir))
                (expand-file-name "./fixture-deltas/"))))
    (when ensure-dir
      (make-directory dir t))
    dir))

(defun combobulate-test-fixture-action-function (number action-fn fixture-fn)
  "Run ACTION-FN on the fixture file, and compare the result with the fixture file."
  (combobulate-test-compare-string-with-file
   (current-buffer)
   (concat (combobulate-test-get-fixture-deltas-directory (symbol-name action-fn))
           (combobulate-test-generate-fixture-diff-filename
            fixture-fn
            (symbol-name action-fn)
            number
            "after"))))

(cl-defun combobulate-for-each-marker (action-fn &key (reverse nil))
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

(cl-defmacro combobulate-test ((&key (setup nil) (fixture nil) language mode) &rest body)
  "Run a combobulate test with a preconfigured buffer.

SETUP is a list of forms to run before the test body. FIXTURE is
the path to a file to load into the buffer before running the
test. LANGUAGE is the language to use for the treesitter parser.
MODE is the major mode to use for the buffer."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (let ((positions '()))
       (funcall #',mode)
       (treesit-parser-create ',language)
       (combobulate-mode)
       (combobulate-setup)
       (switch-to-buffer (current-buffer))
       (with-current-buffer (current-buffer)
         (erase-buffer)
         (when ,fixture
           (unless (file-exists-p ,fixture)
             (error "Fixture file %s does not exist" ,fixture))
           ;; load the fixture file
           (insert-file-contents ,fixture)
           ;; ensure everything's applied properly
           (combobulate-test-fixture-mode 1)
           (hack-local-variables)
           (hack-local-variables-apply)))
       (let ((offset 0)
             ;; disables flashing nodes to echo area etc.
             (combobulate-flash-node nil))
         (dolist (statement (append ,setup ',body))
           (pcase statement
             ((pred stringp) (insert statement))
             (`(save-position ,name) (push (cons name (point)) positions))
             ('bob (goto-char (point-min)))
             ('goto-first-char (goto-char (point-min)) (skip-chars-forward " \n"))
             ('set-offset (setq offset (python-test-get-column)))
             ('n> (newline-and-indent))
             ('<- (python-indent-dedent-line-backspace 1))
             ('back (forward-char -1))
             ('forward (forward-char 1))
             ('tab (indent-for-tab-command))
             ('navigate-up (combobulate-navigate-up))
             ('navigate-down (combobulate-navigate-down))
             ;; (`(cycle-through-markers ,category ,fn)
             ;;  (let ((ordered-ovs))
             ;;    (dotimes (number (length combobulate-test-point-overlays))
             ;;      (push (combobulate--test-get-overlay-by-number number) ordered-ovs))
             ;;    (dolist (ov ordered-ovs)
             ;;      (goto-char (overlay-start ov))
             ;;      (funcall #',fn)
             ;;      )))
             ((or `(assert-at-marker ,number) `(assert-at-marker ,number ,category))
              (should (combobulate--test-get-overlay-at-point number)))
             ((or `(goto-marker ,number) `(goto-marker ,number ,category))
              (let ((ov (combobulate--test-get-overlay-by-number number)))
                (should ov)
                (goto-char (overlay-start ov))))
             ('debug-show
              (pop-to-buffer (current-buffer))
              (font-lock-fontify-buffer)
              (message "Stopped. `C-M-c' to resume. Current point: %s" (point))
              (pulse-momentary-highlight-one-line (point) 'highlight)
              (recursive-edit))
             (`(position= ,name)
              (unless (= (alist-get name positions) (point))
                (ert-fail
                 (list
                  nil
                  :fail-reason "Current point does not match expected position"
                  :point (point)
                  :statement statement
                  :expected (alist-get name positions)
                  :condition (= (alist-get name positions) (point))))))
             (`(col= ,col)
              (unless (= (combobulate-test-get-column) col)
                (ert-fail
                 (list
                  nil
                  :fail-reason "Column incorrect"
                  :column (combobulate-test-get-column)
                  :statement statement
                  :expected col
                  :condition (= (combobulate-test-get-column) col)))))
             (`(line= ,line)
              (unless (= (combobulate-test-get-line) line)
                (ert-fail
                 (list
                  nil
                  :line (combobulate-test-get-line)
                  :statement statement
                  :expected line
                  :fail-reason "Line incorrect"
                  :condition (= (combobulate-test-get-line) line)))))
             (_ (eval statement t))))))))

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
