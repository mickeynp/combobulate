
(defun combobulate-test-get-column ()
  (current-column))

(defun combobulate-test-get-line ()
  (line-number-at-pos))


(defmacro combobulate-test (&rest statements)
  "" nil
  `(with-temp-buffer
     (let ((typescript-mode-hook)
           (typescript-indent-level 4)
           (positions '()))
       (typescript-mode)
       (combobulate-mode)
       (switch-to-buffer (current-buffer))
       (let ((offset 0))
         (erase-buffer)
         (dolist (statement ',statements)
           (pcase statement
             ((pred stringp) (insert statement))
             (`(save-position ,name) (push (cons name (point)) positions))
             ((pred (eq 'bob)) (goto-char (point-min)))
             ((pred (eq 'set-offset)) (setq offset (python-test-get-column)))
             ((pred (eq 'return)) (newline-and-indent))
             ((pred (eq 'backspace)) (python-indent-dedent-line))
             ((pred (eq 'back)) (forward-char -1))
             ((pred (eq 'forward)) (forward-char 1))
             ((pred (eq 'tab)) (indent-for-tab-command))
             ((pred (eq 'navigate-up)) (combobulate-navigate-up))
             ((pred (eq 'navigate-down)) (combobulate-navigate-down))
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
             (_ (error "Unknown statement `%s'" statement))))))))


(ert-deftest combobulate-test-navigation-up ()
  "Test navigation up works"
  (combobulate-test
   "if (true) {" return
   (save-position 2) "function a (foo, bar, baz) {" return
   (save-position 1) "function blah ({a, b, c}) {" return
   "let a = 1;" return
   "}}}" back back back
   navigate-up
   (position= 1)
   navigate-up
   (position= 2)))


(ert-deftest combobulate-test-navigation-down ()
  "Test navigation up works"
  (combobulate-test
   "if (true) {" return
   (save-position 1) "function a (foo, bar, baz) {" return
   (save-position 2) "function blah ({a, b, c}) {" return
   "let a = 1;" return
   "}}}"
   bob
   navigate-down
   (position= 1)
   navigate-down
   (position= 2)))


(ert-deftest tsc-cursor-behaviour ()
  "Cursor movement does not behave as expected"
  )

(defmacro tsc-test-with (lang-symbol var &rest body)
  "Eval BODY with VAR bound to a new parser for LANG-SYMBOL."
  (declare (indent 2))
  `(let ((,var (tsc-test-make-parser ,lang-symbol)))
     ,@body))
(defun tsc-test-make-parser (lang-symbol)
  "Return a new parser for LANG-SYMBOL."
  (let ((parser (tsc-make-parser))
        (language (tree-sitter-require lang-symbol)))
    (tsc-set-language parser language)
    parser))

(ert-deftest cursor::walk ()
  (tsc-test-with 'rust parser
    (let* ((tree (tsc-parse-string parser "fn foo() {}"))
           (node (tsc-root-node tree)))
      (ert-info ("Should be able to get a cursor from either a tree or a node")
        (should (tsc-cursor-p (tsc-make-cursor tree)))
        (should (tsc-cursor-p (tsc-make-cursor node)))))))

(ert-deftest cursor::walk-from-root-node ()
  (let ((parser (tsc-make-parser))
        (language (tree-sitter-require 'rust)))
    (tsc-set-language parser language)
    (let* ((tree (tsc-parse-string parser "fn foo() {}"))
           (root-node (tsc-root-node tree))
           (tree-sitter-tree tree)
           (cursor (tsc-make-cursor root-node)))
      (ert-info ("Moving a cursor that was made against a non-root should work")
        ;; test that the root node can navigate independently
        ;; this works
        (should (eq (tsc-node-type (tsc-current-node cursor)) 'source_file))
        (should (tsc-goto-first-child cursor))
        (should (eq (tsc-node-type (tsc-current-node cursor)) 'function_item))
        ;; this too
        (let* ((sub-node (tsc-current-node cursor))
               (sub-cursor (tsc-make-cursor sub-node)))
          (should (tsc-node-p sub-node))
          (should (tsc-cursor-p sub-cursor))
          (should (eq (tsc-node-type (tsc-current-node cursor)) 'function_item))
          ;; this should move us back from whence we came but this fails with a nil response.
          (should (tsc-goto-parent sub-cursor)))))))
