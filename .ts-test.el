;;; Helper for the batch emacs command
(setq load-prefer-newer t)
(princ (format "Default directory is `%s'\n" default-directory))
(load-library "tests/html-ts-mode/html-ts-mode.el")
(load-library "tests/tuareg/tuareg-opam.el")
(load-library "tests/tuareg/tuareg-compat.el")
(load-library "tests/tuareg/tuareg.el")
;; (load-library "tests/ocaml-ts-mode/ocaml-ts-mode.el")
(load-library "tests/tuareg-treesit.el")


(setq auto-mode-alist
      (append '(("\\.ml[ily]?\\'" . tuareg-mode)
                ("\\.topml\\'" . tuareg-mode))
              auto-mode-alist))
