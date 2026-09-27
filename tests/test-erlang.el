;;; test-erlang.el --- Tests for the Erlang editing commands  -*- lexical-binding: t; -*-

(require 'combobulate)
(require 'combobulate-test-prelude)

(defmacro combobulate-test-erlang (source &rest body)
  "Run BODY in an `erlang-ts-mode' buffer holding SOURCE, with point at `‸'."
  (declare (indent 1))
  `(progn
     (skip-unless (and (fboundp 'erlang-ts-mode) (treesit-language-available-p 'erlang)))
     (with-temp-buffer
       (insert ,source)
       (goto-char (point-min))
       (search-forward "‸")
       (delete-char -1)
       (erlang-ts-mode)
       (combobulate-mode)
       (let ((combobulate-flash-node nil))
         ,@body))))

(ert-deftest combobulate-test-erlang-kill-last-clause-takes-previous-separator ()
  (combobulate-test-erlang "f(X) ->\n    case X of\n        a -> 1;\n        ‸b -> 2\n    end.\n"
    (combobulate-erlang-kill-node-dwim)
    (should (equal (buffer-string) "f(X) ->\n    case X of\n        a -> 1\n    end.\n"))))

(ert-deftest combobulate-test-erlang-kill-first-clause-takes-its-line ()
  (combobulate-test-erlang "f(X) ->\n    case X of\n        ‸a -> 1;\n        b -> 2\n    end.\n"
    (combobulate-erlang-kill-node-dwim)
    (should (equal (buffer-string) "f(X) ->\n    case X of\n        b -> 2\n    end.\n"))))

(ert-deftest combobulate-test-erlang-kill-last-statement-takes-previous-comma ()
  (combobulate-test-erlang "f() ->\n    A = 1,\n    ‸{ok, A}.\n"
    (combobulate-erlang-kill-node-dwim)
    (should (equal (buffer-string) "f() ->\n    A = 1.\n"))))

(ert-deftest combobulate-test-erlang-kill-last-function-clause-ends-the-function ()
  (combobulate-test-erlang "f(0) -> zero;\n‸f(N) -> N.\n\ng() -> ok.\n"
    (combobulate-erlang-kill-node-dwim)
    (should (equal (buffer-string) "f(0) -> zero.\n\ng() -> ok.\n"))))

(ert-deftest combobulate-test-erlang-drag-last-function-clause-up ()
  (combobulate-test-erlang "f(0) -> zero;\n‸f(N) -> N.\n"
    (combobulate-erlang-drag-up)
    (should (equal (buffer-string) "f(N) -> N;\nf(0) -> zero.\n"))))

(ert-deftest combobulate-test-erlang-drag-refuses-to-cross-after ()
  (combobulate-test-erlang "f() ->\n    receive\n        stop -> ok\n    ‸after 10 -> timeout\n    end.\n"
    (should-error (combobulate-erlang-drag-up) :type 'user-error)
    (should (equal (buffer-string) "f() ->\n    receive\n        stop -> ok\n    after 10 -> timeout\n    end.\n"))))

(ert-deftest combobulate-test-erlang-splice-up-replaces-the-case ()
  (combobulate-test-erlang "f(X) ->\n    case X of\n        a -> ‸one;\n        b -> two\n    end.\n"
    (combobulate-erlang-splice-up)
    (should (equal (buffer-string) "f(X) ->\n    one.\n"))))

(ert-deftest combobulate-test-erlang-splice-up-reindents-the-statements ()
  (combobulate-test-erlang "f() ->\n    begin\n        ‸A = 1,\n        A\n    end.\n"
    (combobulate-erlang-splice-up)
    (should (equal (buffer-string) "f() ->\n    A = 1,\n    A.\n"))))

(ert-deftest combobulate-test-erlang-splice-refuses-a-function-body ()
  (combobulate-test-erlang "f() ->\n    ‸ok.\n"
    (should-error (combobulate-erlang-splice-up) :type 'user-error)))

(ert-deftest combobulate-test-erlang-sequence-visits-case-keywords ()
  (combobulate-test-erlang "f(X) ->\n    ‸case X of\n        a -> 1\n    end.\n"
    (combobulate-erlang-navigate-sequence-next)
    (should (looking-at-p "of"))
    (combobulate-erlang-navigate-sequence-next)
    (should (looking-at-p "end"))
    (combobulate-erlang-navigate-sequence-previous)
    (should (looking-at-p "of"))))

(ert-deftest combobulate-test-erlang-sequence-skips-missing-keywords ()
  (combobulate-test-erlang "f() ->\n    ‸try g()\n    catch\n        _:_ -> ok\n    end.\n"
    (combobulate-erlang-navigate-sequence-next)
    (should (looking-at-p "catch"))
    (combobulate-erlang-navigate-sequence-next)
    (should (looking-at-p "end"))))

(provide 'test-erlang)
;;; test-erlang.el ends here
