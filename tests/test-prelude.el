;;; test-prelude.el --- prelude for combobulate tests  -*- lexical-binding: t; -*-

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

(defun combobulate-test-skip-to-match (expr)
  (when-let (v (re-search-forward expr nil nil 1))
    (goto-char (match-beginning 0))
    nil))

(cl-defmacro combobulate-test ((&key (setup nil) language mode) &rest body)
  ""
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (let ((positions '()))
       (funcall #',mode)
       (treesit-parser-create ',language)
       (combobulate-mode)
       (combobulate-setup)
       (switch-to-buffer (current-buffer))
       (with-current-buffer (current-buffer)
         (font-lock-fontify-buffer))
       (let ((offset 0))
         (erase-buffer)
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
             ('debug-show
              (pop-to-buffer (current-buffer))
              (font-lock-fontify-buffer)
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
             (_ (eval statement))))))))

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
;;; test-prelude.el ends here
