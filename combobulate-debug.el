;;; combobulate-debug.el --- debug routines for combobulate  -*- lexical-binding: t; -*-

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

;; Debug helpers for combobulate.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'edebug)

(defun combobulate--show-navigable-and-parent ()
  (interactive)
  (let* ((node(combobulate--get-nearest-navigable-node))
         (parent (combobulate-nav-get-parent node)))
    (save-excursion
      (combobulate--debug-highlight-queries (list node parent)))))

(defun combobulate--debug-highlight-queries (nodes &optional wait)
  (setq wait (or wait .7))
  (unless (listp nodes)
    (setq nodes (list nodes)))
  (save-excursion
    (prog1 nodes
      (dolist (node nodes)
        (when node
          (with-current-buffer (treesit-node-buffer node)
            (pcase node
              ((or `(,_ . ,match-node) `,match-node)
               (when match-node
                 (combobulate--mark-extent (combobulate-node-start match-node)
                                           (combobulate-node-end match-node))
                 (message "Marked %s" (combobulate-node-type match-node))
                 (sit-for wait)
                 (deactivate-mark))))))))))


;; stolen from `edebug.el'
(defvar edebug-outside-buffer) ; the current-buffer outside of edebug
(defvar edebug-outside-point) ; the point outside of edebug
(defvar edebug-buffer) ; which buffer the function is in.
(defvar edebug-window-data)  ; window and window-start for current function

(defun combobulate--debug-flash-variable (expr &optional _)
  (interactive (list (read--expression "Combobulate Flash variable: ")
                     current-prefix-arg))
  (save-excursion
    (save-window-excursion
      (edebug-pop-to-buffer edebug-outside-buffer)
      (goto-char edebug-outside-point)
      (edebug-eval-expression `(combobulate--debug-highlight-queries ,expr 1.0) nil)
      (sit-for 1.5)
      (edebug-pop-to-buffer edebug-buffer (car edebug-window-data)))))

(define-key edebug-mode-map (kbd "C-c f") #'combobulate--debug-flash-variable)

;; (advice-remove #'combobulate--query-from-node #'combobulate--debug-highlight-queries)

;; (advice-add #'combobulate--query-from-node :filter-return
;;             #'combobulate--debug-highlight-queries)

(provide 'combobulate-debug)
;;; combobulate-debug.el ends here

