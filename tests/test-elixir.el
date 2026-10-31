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

(ert-deftest combobulate-test-elixir-sequence-visits-fn-and-end ()
  (combobulate-test-elixir "Enum.map(l, ‸fn x ->\n  x\nend)\n"
    (combobulate-test-elixir--press "M-n")
    (should (looking-at-p "end"))))

(ert-deftest combobulate-test-elixir-next-visits-the-stages-of-a-pipeline ()
  (combobulate-test-elixir "def f(l) do\n  l\n  |> ‸Enum.filter(& &1)\n  |> Enum.map(& &1)\n  |> Enum.sum()\nend\n"
    (combobulate-test-elixir--press "C-M-n")
    (should (looking-at-p "Enum.map"))
    (combobulate-test-elixir--press "C-M-n")
    (should (looking-at-p "Enum.sum"))))

(ert-deftest combobulate-test-elixir-drag-down-swaps-the-stages-of-a-pipeline ()
  (combobulate-test-elixir "def f(l) do\n  l\n  |> ‸Enum.filter(& &1)\n  |> Enum.map(& &1)\n  |> Enum.sum()\nend\n"
    (combobulate-test-elixir--press "M-N")
    (should (equal (buffer-string) "def f(l) do\n  l\n  |> Enum.map(& &1)\n  |> Enum.filter(& &1)\n  |> Enum.sum()\nend\n"))))

(provide 'test-elixir)
;;; test-elixir.el ends here
