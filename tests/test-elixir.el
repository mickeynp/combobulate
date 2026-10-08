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
  (let* ((command (key-binding (kbd key)))
         (this-command command)
         (last-command nil))
    (call-interactively command)))

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

(ert-deftest combobulate-test-elixir-sequence-visits-do-else-and-end ()
  (combobulate-test-elixir "‸if x do\n  1\nelse\n  2\nend\n"
    (dolist (keyword '("do" "else" "end"))
      (combobulate-test-elixir--press "M-n")
      (should (looking-at-p keyword)))))

(ert-deftest combobulate-test-elixir-sequence-visits-the-clauses-of-try ()
  (combobulate-test-elixir "‸try do\n  a()\nrescue\n  e -> e\ncatch\n  :x -> 1\nafter\n  b()\nend\n"
    (dolist (keyword '("do" "rescue" "catch" "after" "end"))
      (combobulate-test-elixir--press "M-n")
      (should (looking-at-p keyword)))))

(ert-deftest combobulate-test-elixir-sequence-visits-fn-and-end ()
  (combobulate-test-elixir "Enum.map(l, ‸fn x ->\n  x\nend)\n"
    (combobulate-test-elixir--press "M-n")
    (should (looking-at-p "end"))))

(ert-deftest combobulate-test-elixir-sequence-walks-out-of-a-nested-construct ()
  (combobulate-test-elixir "def f(x) do\n  ‸case x do\n    1 -> :a\n  end\nend\n"
    (combobulate-test-elixir--press "M-n")
    (should (looking-at-p "do"))
    (combobulate-test-elixir--press "M-n")
    (should (equal (list (line-number-at-pos) (current-column)) '(4 2)))
    (combobulate-test-elixir--press "M-n")
    (should (equal (list (line-number-at-pos) (current-column)) '(5 0)))))

(ert-deftest combobulate-test-elixir-sequence-previous-goes-back-to-the-start ()
  (combobulate-test-elixir "def f(x) do\n  :ok\n‸end\n"
    (combobulate-test-elixir--press "M-p")
    (should (looking-at-p "do"))
    (combobulate-test-elixir--press "M-p")
    (should (bobp))))

(provide 'test-elixir)
;;; test-elixir.el ends here
