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

(defun combobulate-test-erlang--press (key)
  "Run the command that KEY is bound to, as a user pressing it would."
  (let* ((command (key-binding (kbd key)))
         (this-command command)
         (last-command nil))
    (call-interactively command)))

(ert-deftest combobulate-test-erlang-sequence-visits-case-keywords ()
  (combobulate-test-erlang "f(X) ->\n    ‸case X of\n        a -> 1\n    end.\n"
    (combobulate-test-erlang--press "M-n")
    (should (looking-at-p "of"))
    (combobulate-test-erlang--press "M-n")
    (should (looking-at-p "end"))
    (combobulate-test-erlang--press "M-p")
    (should (looking-at-p "of"))))

(ert-deftest combobulate-test-erlang-sequence-visits-the-end-of-receive ()
  (combobulate-test-erlang "f() ->\n    ‸receive\n        m -> 1\n    after 10 -> 2\n    end.\n"
    (combobulate-test-erlang--press "M-n")
    (should (looking-at-p "end"))))

(ert-deftest combobulate-test-erlang-drag-up-does-not-move-after ()
  (combobulate-test-erlang "f() ->\n    receive\n        stop -> ok\n    ‸after 10 -> timeout\n    end.\n"
    (should-error (combobulate-test-erlang--press "M-P"))
    (should (equal (buffer-string) "f() ->\n    receive\n        stop -> ok\n    after 10 -> timeout\n    end.\n"))))

(provide 'test-erlang)
;;; test-erlang.el ends here
