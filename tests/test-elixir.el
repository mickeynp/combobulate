;;; test-elixir.el --- Tests for Elixir  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)

(defmacro combobulate-test-elixir (source &rest body)
  "Run BODY in an `elixir-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'elixir-ts-mode) (treesit-language-available-p 'elixir)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (elixir-ts-mode)
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(defun combobulate-test-elixir--press (key)
  "Run the command that KEY is bound to, as a user pressing it would."
  (call-interactively (key-binding (kbd key))))

(ert-deftest combobulate-test-elixir-drag-down-keeps-end-below-the-last-case-clause ()
  (combobulate-test-elixir "case x do\n  1 -> :one\n  ‸2 -> :two\n  _ -> :other\nend\n"
    (combobulate-test-elixir--press "M-N")
    (should (equal (buffer-string) "case x do\n  1 -> :one\n  _ -> :other\n  2 -> :two\nend\n"))))

(ert-deftest combobulate-test-elixir-drag-up-keeps-end-below-the-last-case-clause ()
  (combobulate-test-elixir "case x do\n  1 -> :one\n  2 -> :two\n  ‸_ -> :other\nend\n"
    (combobulate-test-elixir--press "M-P")
    (should (equal (buffer-string) "case x do\n  1 -> :one\n  _ -> :other\n  2 -> :two\nend\n"))))

(ert-deftest combobulate-test-elixir-drag-down-keeps-end-below-the-last-fn-clause ()
  (combobulate-test-elixir "Enum.map(list, fn\n  ‸1 -> :a\n  2 -> :b\nend)\n"
    (combobulate-test-elixir--press "M-N")
    (should (equal (buffer-string) "Enum.map(list, fn\n  2 -> :b\n  1 -> :a\nend)\n"))))

(ert-deftest combobulate-test-elixir-drag-down-from-a-map-value-moves-the-pair ()
  (combobulate-test-elixir "def f do\n  %{a: ‸1, b: 2, c: 3}\nend\n"
    (combobulate-test-elixir--press "M-N")
    (should (equal (buffer-string) "def f do\n  %{b: 2, a: 1, c: 3}\nend\n"))))

(ert-deftest combobulate-test-elixir-drag-up-from-a-map-value-moves-the-pair ()
  (combobulate-test-elixir "def f do\n  %{a: 1, b: ‸2, c: 3}\nend\n"
    (combobulate-test-elixir--press "M-P")
    (should (equal (buffer-string) "def f do\n  %{b: 2, a: 1, c: 3}\nend\n"))))

(ert-deftest combobulate-test-elixir-drag-down-from-a-keyword-value-moves-the-pair ()
  (combobulate-test-elixir "def f do\n  [a: ‸1, b: 2, c: 3]\nend\n"
    (combobulate-test-elixir--press "M-N")
    (should (equal (buffer-string) "def f do\n  [b: 2, a: 1, c: 3]\nend\n"))))

(provide 'test-elixir)
;;; test-elixir.el ends here
