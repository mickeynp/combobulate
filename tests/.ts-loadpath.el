;;-*- mode: emacs-lisp; -*-
(use-package combobulate
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode))
  :load-path "../")

(load ".ts-install-tests.el")
