;;; test that when we open python, tsx, and js files, combobulate is enabled
(require 'typescript-ts-mode)
(require 'ert)
(dolist (mode '(python-mode typescript-ts-mode))
  (with-temp-buffer
    (should (not (eq (key-binding (kbd "C-c o c")) 'combobulate-clone-node-dwim)))
    ;; activate the major mode; now combobulate must load via
    ;; `prog-mode-hook'.
    (funcall mode)
    (should (member 'combobulate-mode prog-mode-hook))
    (should (featurep 'combobulate))

    ;; `combobulate-mode' is called in prog-mode-hook. It must therefore be set.
    (should (boundp 'combobulate-mode))
    (should (eq combobulate-mode t))

    (should (eq (key-binding (kbd "C-c o c")) 'combobulate-clone-node-dwim))
    ;; turn off combobulate.
    (funcall #'combobulate-mode)
    (should (eq combobulate-mode nil))))
