
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
