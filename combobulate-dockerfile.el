;;; combobulate-dockerfile.el --- Dockerfile support for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords: convenience

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

;; Dockerfile support for Combobulate.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-interface)
(require 'combobulate-rules)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(declare-function combobulate--mark-node "combobulate-manipulation")
(declare-function combobulate-indent-region "combobulate-manipulation")

(defun combobulate-dockerfile-pretty-print-node-name (node default-name)
  default-name)

(eval-and-compile
  (defconst combobulate-dockerfile-definitions
    '((context-nodes '("unquoed_string" "path" "doubled_quoted_string" "variable" "json_string"))
      ;; do not indent envelopes.
      (envelope-indent-region-function nil)
      (highlight-queries-default nil)
      (indent-after-edit nil)
      (pretty-print-node-name-function #'combobulate-dockerfile-pretty-print-node-name)
      (envelope-procedure-shorthand-alist nil)
      (envelope-list nil)
      (indent-calculate-function nil)
      (envelope-deindent-function nil)
      (procedures-default
       '((:activation-nodes ((:nodes (exclude (all) ("source_file")))))))
      (procedures-defun
       '((:activation-nodes ((:nodes (rx "instruction" eol))))))
      (procedures-sexp
       '((:activation-nodes ((:nodes (rx "instruction" eol))))))
      (procedures-sibling
       '(;; To cut down on layers in Docker images, it is common to
         ;; use shell-style continuation `\' to concatenate multi-line
         ;; commands. Moving between these as siblings is therefore
         ;; useful.
         (:activation-nodes
          ((:nodes ("shell_fragment") :has-ancestor (irule "shell_command")))
          :selector (:choose parent :match-query (:query ((shell_fragment) @match) :engine treesitter)))
         (:activation-nodes
          (;; This is for line-continued environment variables and labels
           (:nodes (rx "pair" eol) :has-ancestor ("label_instruction" "env_instruction")))
          :selector (:choose parent :match-query (:query ([(shell_fragment) (env_pair) (label_pair)] @match) :engine treesitter)))
         ;; JSON navigation between array elements
         (:activation-nodes
          ((:nodes "json_string" :has-parent ("json_string_array")))
          :selector (:match-children t))
         ;; Generalised navigation between siblings
         (:activation-nodes
          (;; Navigation between source-file-level instructions (like
           ;; RUN, ENV, etc.)
           (:nodes (rx "instruction" eol) :has-parent ("source_file")))
          :selector (:match-children t))))
      (plausible-separators
       '("," "\\"))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ((all)) :has-parent ((all))))
          :selector (:choose node
                             :match-children t)))))))

(define-combobulate-language
 :name dockerfile
 :language dockerfile
 ;; `dockerfile-mode' refers to Spotify's Dockerfile
 ;; mode. `dockerfile-ts-mode' refers to the builtin Emacs 29+ mode.
 :major-modes (dockerfile-mode dockerfile-ts-mode)
 :custom combobulate-dockerfile-definitions
 :setup-fn combobulate-dockerfile-setup)



(defun combobulate-dockerfile-setup (_))



(provide 'combobulate-dockerfile)
;;; combobulate-dockerfile.el ends here
