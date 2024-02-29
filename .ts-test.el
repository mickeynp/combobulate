;;; Helper for the batch emacs command
(setq load-prefer-newer t)
(princ (format "Default directory is `%s'\n" default-directory))
(load-library "tests/html-ts-mode/html-ts-mode.el")
