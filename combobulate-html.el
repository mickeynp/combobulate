;;; combobulate-html.el --- HTML and SGML-alike structured editing for combobulate  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-rules)

(defun combobulate-html-pretty-print (node default-name)
  (if (and node (equal (combobulate-node-type node) "element"))
      (format "<%s>" (thread-first node
                                   (combobulate-node-child 0)
                                   (combobulate-node-child 0)
                                   (combobulate-node-text)))
    default-name))

(defun combobulate-html-setup (_)
  (setq combobulate-navigation-default-nodes '("element"))
  (setq combobulate-navigation-sexp-nodes '("element" "attribute" "text"))
  (setq combobulate-pretty-print-node-name-function #'combobulate-html-pretty-print))

(provide 'combobulate-html)
;;; combobulate-html.el ends here
