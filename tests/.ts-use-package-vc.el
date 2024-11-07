;;-*- mode: emacs-lisp; -*-

(if (< emacs-major-version 30)
    (message "Skipping use-package vc - this configuration requires at least GNU Emacs 30")
  (use-package combobulate
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    :vc (:url "https://github.com/mickeynp/combobulate"
              :branch "development"
              :rev :newest))

  (load ".ts-install-tests.el"))
