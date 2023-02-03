;;; combobulate.el --- edit and navigate text by syntactic constructs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.1
;; Homepage: https://www.github.com/mickeynp/combobulate
;; Keywords: convenience, tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Navigate and transform source code using the concrete syntax tree
;; provided by the Emacs 29's builtin support for tree-sitter.
;;

;;; Code:

;; Requirements:
(require 'seq)
(require 'subr-x)
(require 'tempo)
(require 'map)
(eval-when-compile
  (require 'cl-lib))


(require 'combobulate-settings)

(defvar-local combobulate-options-envelope-key-map
    (make-sparse-keymap "Combobulate Envelopes")
  "Dynamically set key map of Combobulate envelopes.")

(defvar combobulate-options-key-map
  (let ((map (make-sparse-keymap "Combobulate Options")))
    (define-key map (kbd "j") #'combobulate-avy-jump)
    (define-key map (kbd "t") #'combobulate-edit-cluster-dwim)
    (define-key map (kbd "o") #'combobulate)
    (define-key map (kbd "c") #'combobulate-clone-node-dwim)
    (define-key map (kbd "v") #'combobulate-vanish-node)
    map))

(defvar combobulate-key-map
  (let ((map (make-sparse-keymap "Combobulate")))
    (define-key map (kbd "C-M-a") #'combobulate-navigate-beginning-of-defun)
    (define-key map (kbd "C-M-d") #'combobulate-navigate-down-list-maybe)
    (define-key map (kbd "C-M-e") #'combobulate-navigate-end-of-defun)
    (define-key map (kbd "C-M-h") #'combobulate-mark-defun)
    (define-key map (kbd "C-M-n") #'combobulate-navigate-next)
    (define-key map (kbd "C-M-p") #'combobulate-navigate-previous)
    (define-key map (kbd "C-M-t") #'combobulate-transpose-sexps)
    (define-key map (kbd "C-M-u") #'combobulate-navigate-up-list-maybe)
    (define-key map (kbd "M-<up>") #'combobulate-splice-up)
    (define-key map (kbd "M-<down>") #'combobulate-splice-down)
    (define-key map (kbd "M-N") #'combobulate-drag-down)
    (define-key map (kbd "M-P") #'combobulate-drag-up)
    (define-key map (kbd "M-a") #'combobulate-navigate-logical-previous)
    (define-key map (kbd "M-e") #'combobulate-navigate-logical-next)
    (define-key map (kbd "M-h") #'combobulate-mark-node-dwim)
    (define-key map (kbd "M-k") #'combobulate-kill-node-dwim)
    (define-key map (kbd "C-c o") combobulate-options-key-map)
    map))

(make-variable-buffer-local 'forward-sexp-function)

(defun combobulate--setup-envelopes (envelopes)
  "Prepare ENVELOPES for interactive use.

Each envelope is read and an interactive function for it
created. The envelope is then modified in-situ with a
`:template-symbol' containing the symbol name of the `:template'
-- a weird requirement of tempo -- and a function bound to the
same name, which is stored in `:function'"
  (mapcar
   (lambda (envelope)
     (map-let (:description :name :template :point-placement) envelope
       (let ((fn-name (intern (string-replace
                               " " "-"
                               (concat
                                combobulate-envelope-symbol-prefix
                                (symbol-name major-mode)
                                "-"
                                name)))))
         (set fn-name template)
         ;; Store the function symbol for later recall in things like
         ;; transient.
         (setf envelopes
               (plist-put envelope
                          :function
                          (defalias fn-name
                            `(lambda () ,description
                               (interactive)
                               (combobulate-execute-envelope ,name)))))
         ;; Tempo has an insane requirement that it _must_ be fed the
         ;; *symbol* that holds the template and never just the
         ;; template.
         (setf envelopes (plist-put envelope :template-symbol fn-name))
         (setf envelopes (plist-put envelope :point-placement (or point-placement 'start))))))
   envelopes))

(defun combobulate-setup ()
  "Setup combobulate in the current buffer.

This can be used to reinitialize mode-specific setups if they
have changed."
  (interactive)
  (if-let* ((lang (car-safe (treesit-parser-list)))
            (parser-lang (treesit-parser-language lang))
            (setup-fn (alist-get parser-lang combobulate-setup-functions-alist)))
      (progn
        ;; load the production rules
        (setq combobulate-navigation-rules-overrides nil)
        (setq combobulate-navigation-rules-overrides-inverted nil)
        (setq combobulate-navigation-rules
              (symbol-value (intern (format "combobulate-rules-%s" parser-lang))))
        (setq combobulate-navigation-rules-inverted
              (symbol-value (intern (format "combobulate-rules-%s-inverted" parser-lang))))
        ;; prepare the sexp functions so they use our version
        (setq-local forward-sexp-function #'combobulate-forward-sexp-function)
        (setq-local transpose-sexps-function #'combobulate-transpose-sexp-function)
        (funcall setup-fn parser-lang)
        ;; this should come after the funcall to `setup-fn' as we need
        ;; the procedures setup and ready before we continue.
        (setq-local combobulate-navigation-editable-nodes
                    (combobulate-procedure-get-activation-nodes combobulate-manipulation-edit-procedures))
        (local-set-key
         (kbd "C-c o e")
         ;; todo: this should be a single-shot setup per mode.
         (let ((map (make-sparse-keymap)))
           (dolist (envelope (combobulate--setup-envelopes
                              combobulate-manipulation-envelopes))
             (map-let (:function :key :extra-key) envelope
               (define-key map (kbd key) function)
               (when extra-key
                 (define-key combobulate-key-map (kbd extra-key) function))))
           map))
        (run-hooks 'combobulate-after-setup-hook))
    (user-error "Combobulate cannot find a setup function for this tree sitter language.

Customize `combobulate-setup-functions-alist' to change the language setup alist.")))

(define-minor-mode combobulate-mode "Navigate and edit text by syntactic constructs

\\{combobulate-key-map}"
  :init-value nil :lighter "©" :keymap combobulate-key-map
  (condition-case nil
      (when combobulate-mode
        (combobulate-setup))
    (user-error
     (combobulate-message "There is either no tree sitter language in this buffer, or Combobulate does not support it.")
     (combobulate-mode -1))))

;;; internal
(require 'combobulate-rules)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-contrib)
(require 'combobulate-display)
(require 'combobulate-ui)
(require 'combobulate-misc)
;;; end internal

;;; language support
(require 'combobulate-html)
(require 'combobulate-python)
(require 'combobulate-js-ts)
(require 'combobulate-css)
(require 'combobulate-yaml)
;;; end language support

(provide 'combobulate)
;;; combobulate.el ends here

