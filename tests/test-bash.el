;;; test-bash.el --- Tests for Bash  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)

(defmacro combobulate-test-bash (source &rest body)
  "Run BODY in a `bash-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-language-available-p 'bash))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       ;; Without a file name, the shell is guessed from $SHELL, and
       ;; anything but bash or sh falls back to `sh-mode'.
       (let ((sh-shell-file "/bin/bash"))
         (bash-ts-mode))
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(defconst combobulate-test-bash-script
  "if [ -n \"$a\" ]; then\n  one\nelif [ -z \"$b\" ]; then\n  two\n  three\nfi\n")

(defun combobulate-test-bash-at (marker)
  "Return `combobulate-test-bash-script' with `‸' before MARKER."
  (let ((pos (string-search marker combobulate-test-bash-script)))
    (concat (substring combobulate-test-bash-script 0 pos) "‸"
            (substring combobulate-test-bash-script pos))))

(ert-deftest combobulate-test-bash-drag-refuses-to-move-the-elif-condition-into-the-body ()
  (combobulate-test-bash (combobulate-test-bash-at "[ -z")
    (should-error (combobulate-bash-drag-down) :type 'user-error)
    (should (equal (buffer-string) combobulate-test-bash-script))))

(ert-deftest combobulate-test-bash-drag-refuses-to-move-the-if-condition-into-the-body ()
  (combobulate-test-bash (combobulate-test-bash-at "[ -n")
    (should-error (combobulate-bash-drag-down))
    (should (equal (buffer-string) combobulate-test-bash-script))))

(ert-deftest combobulate-test-bash-next-stays-on-the-if-condition ()
  (combobulate-test-bash (combobulate-test-bash-at "[ -n")
    (let ((start (point)))
      (combobulate-navigate-next)
      (should (= (point) start)))))

(ert-deftest combobulate-test-bash-drag-is-remapped ()
  (combobulate-test-bash (combobulate-test-bash-at "two")
    (should (eq (key-binding [remap combobulate-drag-down]) #'combobulate-bash-drag-down))
    (call-interactively (key-binding [remap combobulate-drag-down]))
    (should (equal (buffer-string)
                   "if [ -n \"$a\" ]; then\n  one\nelif [ -z \"$b\" ]; then\n  three\n  two\nfi\n"))))

(ert-deftest combobulate-test-bash-next-and-previous-visit-the-stages-of-a-pipeline ()
  (combobulate-test-bash "cat log | ‸grep error | sort\n"
    (combobulate-navigate-next)
    (should (looking-at "sort"))
    (combobulate-navigate-previous)
    (combobulate-navigate-previous)
    (should (looking-at "cat log"))))

(ert-deftest combobulate-test-bash-drag-swaps-the-stages-of-a-pipeline ()
  (combobulate-test-bash "cat log | ‸grep error | sort\n"
    (combobulate-bash-drag-down)
    (should (equal (buffer-string) "cat log | sort | grep error\n"))))

(provide 'test-bash)
;;; test-bash.el ends here
