;;; combobulate.el --- edit and navigate text by syntactic constructs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.2
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


;;;###autoload
(require 'combobulate-rules)
;;;###autoload
(require 'combobulate-procedure)
;;;###autoload
(require 'combobulate-navigation)
;;;###autoload
(require 'combobulate-manipulation)
;;;###autoload
(require 'combobulate-envelope)
;;;###autoload
(require 'combobulate-display)
;;;###autoload
(require 'combobulate-ui)
;;;###autoload
(require 'combobulate-misc)
;;;###autoload
(require 'combobulate-query)
;;;###autoload
(require 'combobulate-cursor)
;;; end internal



;;; language support
;;;###autoload
(require 'combobulate-toml)
;;;###autoload
(require 'combobulate-html)
;;;###autoload
(require 'combobulate-python)
;;;###autoload
(require 'combobulate-js-ts)
;;;###autoload
(require 'combobulate-css)
;;;###autoload
(require 'combobulate-yaml)
;;;###autoload
(require 'combobulate-json)
;;;###autoload
(require 'combobulate-go)
;;;###autoload
(require 'combobulate-ocaml)
;;; end language support

(provide 'combobulate)
;;; combobulate.el ends here


