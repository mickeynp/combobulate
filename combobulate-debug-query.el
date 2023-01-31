;;; combobulate-debug-query.el --- debug tree sitter queries  -*- lexical-binding: t; -*-
;; Copyright (C) 2020
;;
;; Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;;         Tuấn-Anh Nguyễn
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Taken from `https://github.com/emacs-tree-sitter' and repurposed to
;; work with Emacs 29's `treesit' module, plus a handful of minor
;; tweaks.

;;; Code:

(require 'combobulate-interface)
(require 'scheme)

(declare-function combobulate-node-range "combobulate-navigation")
(declare-function combobulate-query-capture "combobulate-navigation")


(defgroup combobulate--debug-ts-query nil
  "combobulate--debug-ts playground."
  :group 'combobulate)

(define-derived-mode combobulate--debug-ts-query-mode prog-mode "treesit-query-builder"
  "Major mode for building combobulate--debug-ts queries and testing them live."
  :syntax-table scheme-mode-syntax-table
  :abbrev-table scheme-mode-abbrev-table)

(defvar combobulate--debug-ts-query-parser nil
  "Parser index to use")
(defconst combobulate--debug-ts-query-builder-buffer-name "*combobulate--debug-ts-query-builder*"
  "Name of the builder buffer.")

(defvar combobulate--debug-ts-query--target-buffer nil
  "Target buffer to run the queries against.")

(defun combobulate--debug-ts--echo (&rest args)
  "Display a transient message, without logging it in the `*Messages*' buffer."
  (let (message-log-max)
    (apply #'message args)))

(defvar tree-sitter-query-matches
  '((:background "chartreuse4" :foreground "white")
    (:background "chartreuse3" :foreground "black")
    (:background "chartreuse2" :foreground "black")
    (:background "chartreuse1" :foreground "black")
    (:background "cyan4" :foreground "white")
    (:background "cyan3" :foreground "black")
    (:background "cyan2" :foreground "black")
    (:background "cyan1" :foreground "black"))
  "Face for highlight captures in matches.")

(defun combobulate--debug-ts-change-parser ()
  (interactive)
  (let ((parsers (mapcar (lambda (p) (cons (format "%s" p) p)) (combobulate-parser-list))))
    (setq combobulate--debug-ts-query-parser
          (cdr (assoc (completing-read "Pick a parser" parsers) parsers)))))

(defun combobulate--debug-ts-query--highlight-capture (capture ct)
  "Highlight CAPTURE in the current buffer."
  (pcase-let* ((`(,capture-name . ,captured-node) capture)
               (`(,node-start . ,node-end) (combobulate-node-range captured-node))
               (overlay (make-overlay node-start node-end)))
    ;; Ensure the overlay is deleted when it becomes empty.
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face (nth (mod ct (length tree-sitter-query-matches))
                                    tree-sitter-query-matches))
    ;; Use the capture's name as the mouseover tooltip.
    (unless (string= capture-name "")
      (overlay-put overlay 'help-echo capture-name))))



(defun combobulate--debug-ts-query--eval-query (patterns)
  "Evaluate query PATTERNS against the target buffer."
  (with-current-buffer combobulate--debug-ts-query--target-buffer
    (remove-overlays)
    (when-let*
        ((captures (combobulate-query-capture combobulate--debug-ts-query-parser
                                              patterns)))
      (if (= (length captures) 0)
          (combobulate--debug-ts--echo "No captures found")
        (let ((ct 0))
          (seq-doseq (capture captures)
            (combobulate--debug-ts-query--highlight-capture capture ct)
            (cl-incf ct)))))))

(defun combobulate--debug-ts-query--after-change (&rest _args)
  "Run query patterns against the target buffer and update highlighted texts."
  (with-current-buffer (get-buffer combobulate--debug-ts-query-builder-buffer-name)
    (let ((patterns (buffer-string)))
      (with-demoted-errors "Error: %S"
        (combobulate--debug-ts-query--eval-query patterns)))))

(defun combobulate--debug-ts-query--clean-target-buffer ()
  "Remove all overlays from the target buffer."
  (with-current-buffer combobulate--debug-ts-query--target-buffer
    (remove-overlays))
  (setq combobulate--debug-ts-query--target-buffer nil))

;;;###autoload
(defun combobulate--debug-ts-query-builder ()
  "Provide means for developers to write and test combobulate--debug-ts queries.

The buffer on focus when the command is called is set as the target buffer."
  (interactive)
  (let* ((target-buffer (current-buffer))
         (builder-buffer (get-buffer-create combobulate--debug-ts-query-builder-buffer-name))
         (builder-window-is-visible (get-buffer-window builder-buffer)))
    (when (eq target-buffer builder-buffer)
      (user-error "This buffer cannot be use as target buffer"))
    (with-current-buffer target-buffer
      (combobulate--debug-ts-change-parser)
      ;; TODO: The query should be run against the changed range only.
      (add-hook 'combobulate--debug-ts-after-change-functions #'combobulate--debug-ts-query--after-change nil :local)
      (setq combobulate--debug-ts-query--target-buffer target-buffer))
    (unless builder-window-is-visible
      (display-buffer builder-buffer '(display-buffer-in-side-window
                                       ((side . bottom)
                                        (window-height . 10)))))
    (with-current-buffer builder-buffer
      (erase-buffer)
      (combobulate--debug-ts-query-mode)
      (add-hook 'after-change-functions #'combobulate--debug-ts-query--after-change nil :local)
      (add-hook 'kill-buffer-hook #'combobulate--debug-ts-query--clean-target-buffer nil :local))
    (setf combobulate--debug-ts-query--target-buffer target-buffer)
    ;; Switch focus to the query builder window.
    (select-window (get-buffer-window builder-buffer))))


(provide 'combobulate-debug-query)
;;; combobulate-debug-query.el ends here
