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




;;; internal

;;; NOTE: The autoload cookies are required so third-party package
;;; managers like straight.el work correctly. I cannot verify that
;;; this assertion is entirely correct, but I think straight.el does
;;; something with autoloading that is substantially different from
;;; what `use-package' does when you set a `:load-path'.


(require 'combobulate-rules)
(require 'combobulate-procedure)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-envelope)
(require 'combobulate-display)
(require 'combobulate-ui)
(require 'combobulate-misc)
(require 'combobulate-query)
(require 'combobulate-cursor)
;;; end internal



;;; language support
(require 'combobulate-toml)
(require 'combobulate-html)
(require 'combobulate-python)
(require 'combobulate-dockerfile)
(require 'combobulate-js-ts)
(require 'combobulate-css)
(require 'combobulate-yaml)
(require 'combobulate-json)
(require 'combobulate-go)
;;; end language support


;;;###autoload
(defun combobulate-mode (&optional arg &rest _)
  "Navigate and edit by syntactic constructs.

This is a helper command that tries to activate the right
Combobulate minor mode suitable for the current buffer."
  (interactive "p")
  ;; This is no longer an actual minor mode, but instead a function.
  (combobulate-maybe-activate nil (not (null arg))))

(provide 'combobulate)
;;; combobulate.el ends here


