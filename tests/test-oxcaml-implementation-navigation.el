;;; test-ocaml-implementation-navigation.el --- Tests for OCaml implementation (.ml) navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pixie Dust

;; Author: Pixie Dust <playersrebirth@gmail.com>
;; Keywords:

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

;; Tests for navigation in OxCaml implementation (.ml) files

;;; Code:

(require 'combobulate)

(require 'combobulate-test-prelude)
(require 'ert)

;;; Helpers

(defmacro combobulate-step (description &rest body)
  "Execute BODY, logging DESCRIPTION."
  (declare (indent 1) (debug t))
  `(progn
     ,@body))

(defun with-tuareg-buffer (callback &optional file)
  "Perform CALLBACK in a temp-buffer (with FILE as a content)."
  (let* ((file (or file "fixtures/oxcaml/oxcaml.ml"))
         (fixture (expand-file-name file default-directory)))
    (with-temp-buffer (progn
                        (insert-file-contents fixture)
                        (setq buffer-file-name fixture)
                        (tuareg-mode)
                        (combobulate-mode)
                        (sit-for 0.1)
                        (funcall callback)))))

(defun expected-node-type (expected &optional msg node)
  "Expect that NODE has EXPECTED type (and display MSG if given)."
  (let* ((node (or node (combobulate-node-at-point)))
         (actual (combobulate-node-type node))
         (msg (if msg (format "%s - " msg) "")))
    (when (not (equal expected actual))
      (message "%sExpected node: %s, Got: %s" msg expected actual))
    (should (equal expected actual))))

(defun expected-thing-at-point (expected &optional msg kind)
  "Expect that things at point is EXPECTED using MSG for a given KIND."
  (let* ((kind (or kind 'word))
         (actual (thing-at-point kind 'no-properties))
         (msg (if msg (format "%s - " msg) "")))
    (when (not (string-equal expected actual))
      (message "%s - Expected things: %s, Got: %s" msg expected actual))
    (should (string-equal expected actual))))

(defun expected-sexp-at-point (expected &optional msg)
  "Expect that sexp at point is EXPECTED using MSG."
  (let ((actual (sexp-at-point))
        (msg (if msg (format "%s - " msg) "")))
    (when (not (equal expected actual))
      (message "%s - Expected things: %s, Got: %s" msg expected actual))
    (should (equal expected actual))))

(defun expected-symbol-at-point (expected &optional msg)
  "Expect that symbol at point is EXPECTED using MSG."
  (let ((actual (symbol-name (symbol-at-point)))
        (msg (if msg (format "%s - " msg) "")))
    (when (not (equal expected actual))
      (message "%s - Expected things: %s, Got: %s" msg expected actual))
    (should (equal expected actual))))

;;; Tests

(ert-deftest oxcaml-1 ()
  "Test sibling navigation involving unboxed constants."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let fast_square"
      (goto-char (point-min))
      (re-search-forward "let fast_square")
      (beginning-of-line))

     (combobulate-step
      "C-M-p: goto type t"
      (combobulate-navigate-previous)
      (expected-node-type "type" "1"))

     (combobulate-step
      "C-M-n: goto let pi"
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (expected-node-type "let" "2")
      (forward-word 2)
      (expected-thing-at-point "pi" "2.1" 'symbol
      )))))


(ert-deftest oxcaml-2 ()
  "Test hierarchy navigation in an unboxed float."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to type t"
      (goto-char (point-min))
      (re-search-forward "type t")
      (beginning-of-line))

     (combobulate-step
      "C-M-d: goto t"
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "1"))

     (combobulate-step
      "C-M-n: goto float32"
      (combobulate-navigate-next)
      (expected-node-type "jkind_abbreviation" "2")
      )
      
      (combobulate-step
      "C-M-n: goto float32#"
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "3")
      ))))

(ert-deftest oxcaml-3 ()
  "Test sibling navigation in an unboxed float."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to type t"
      (goto-char (point-min))
      (re-search-forward "type t")
      (beginning-of-line)
       (combobulate-navigate-down)
       (combobulate-navigate-next))

      (combobulate-step
      "C-M-n: goto float32#"
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "1")
      )
      ;; Bug: navigating back from float32# should go to float32, but cursor doesn't move
      (combobulate-step
      "C-M-p: goto float32"
      (combobulate-navigate-previous)
      (expected-node-type "jkind_abbreviation" "2")
      ))))

  (ert-deftest oxcaml-4 ()
  "Test sibling navigation in a mixed record with unboxed float."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to type point"
      (goto-char (point-min))
      (re-search-forward "type point")
      (beginning-of-line)
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-down))

      (combobulate-step
      "C-M-n: goto count"
      (combobulate-navigate-next)
      (expected-node-type "field_name" "1")
      (expected-thing-at-point "count" "1.1" 'symbol)
      )
      
      (combobulate-step
      "C-M-n: goto x"
      (combobulate-navigate-next)
      (expected-node-type "field_name" "2")
      (expected-thing-at-point "x" "2.1" 'symbol)
      )
      
      (combobulate-step
      "C-M-n: goto y"
      (combobulate-navigate-next)
      (expected-node-type "field_name" "3")
      (expected-thing-at-point "y" "3.1" 'symbol)
      )
      
      (combobulate-step
      "C-M-p: go back to x"
      (combobulate-navigate-previous)
      (expected-node-type "field_name" "4")
      (expected-thing-at-point "x" "4.1" 'symbol)
      )
      
      (combobulate-step
      "C-M-p: go back to count"
      (combobulate-navigate-previous)
      (expected-node-type "field_name" "5")
      (expected-thing-at-point "count" "5.1" 'symbol)
      ))))

(ert-deftest oxcaml-5 ()
  "Test navigation in a local returning function."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let make_pair"
      (goto-char (point-min))
      (re-search-forward "let make_pair")
      (beginning-of-line))

      (combobulate-step
      "goto enclave_"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (expected-node-type "exclave_legacy" "1")
      )

      (combobulate-step
      "C-M-d: goto stack_"
      (combobulate-navigate-down)
      (expected-node-type "stack_" "2")
      )
      
      (combobulate-step
      "C-M-d: goto ("
      (combobulate-navigate-down)
      (expected-node-type "(" "3")
      )
      
      (combobulate-step
      "C-M-d: goto a"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "4")
      (expected-thing-at-point "a" "4.1" 'symbol)
      )
      
      (combobulate-step
      "C-M-d: goto b"
      (combobulate-navigate-next)
      (expected-node-type "value_name" "5")
      (expected-thing-at-point "b" "5.1" 'symbol)
      ))))

(ert-deftest oxcaml-6 ()
  "Test navigation with global_ field."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let type 'a wrapper"
      (goto-char (point-min))
      (re-search-forward "type 'a wrapper")
      (beginning-of-line))

      (combobulate-step
      "goto global_"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "modality_legacy" "1")
      (expected-thing-at-point "global_" "1.1" 'symbol)
      )

      (combobulate-step
      "C-M-n: goto tag"
      (combobulate-navigate-next)
      (expected-node-type "field_name" "2")
      (expected-thing-at-point "tag" "2.1" 'symbol)
      )

      (combobulate-step
      "C-M-p: go back to global"
      (combobulate-navigate-previous)
      (expected-node-type "modality_legacy" "3")
      (expected-thing-at-point "global_" "3.1" 'symbol)
      )

      (combobulate-step
      "C-M-d: go to the child of global_ which is value"
      (combobulate-navigate-down)
      (expected-node-type "field_name" "4")
      (expected-thing-at-point "value" "4.1" 'symbol)
      ))))


(ert-deftest oxcaml-7 ()
  "Test navigation in an array of unboxed elements"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let zeros"
      (goto-char (point-min))
      (re-search-forward "let zeros")
      (beginning-of-line))

     (combobulate-step
      "C-M-d: goto float#"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "1")
      (expected-thing-at-point "float" "1.1" 'symbol)
      )

     (combobulate-step
      "C-M-n: goto array"
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "2")
      (expected-thing-at-point "array" "2.1" 'symbol)
      ))))

(ert-deftest oxcaml-8 ()
  "Test sibling navigation in an array of unboxed elements"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let zeros"
      (goto-char (point-min))
      (re-search-forward "let zeros")
      (beginning-of-line))

     (combobulate-step
      "C-M-d: goto float#"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "1")
      (expected-thing-at-point "float" "1.1" 'symbol)
      )

     (combobulate-step
      "Move to ["
      (re-search-forward "\\[")
      (expected-node-type "[|" "2")
      )

      (combobulate-step
      "Move to the first element of the array"
      (combobulate-navigate-down)
      (expected-node-type "unboxed_constant" "3")
      )

      (combobulate-step
      "Move to the second element of the array"
      (combobulate-navigate-next)
      (expected-node-type "unboxed_constant" "4")
      )

      (combobulate-step
      "Move to the third element of the array"
      (combobulate-navigate-next)
      (expected-node-type "unboxed_constant" "5")
      )

      (combobulate-step
      "Move to the second element of the array"
      (combobulate-navigate-previous)
      (expected-node-type "unboxed_constant" "6")
      )
      
    )))

(ert-deftest oxcaml-9 ()
  "Test navigation in immutable arrays"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let primes"
      (goto-char (point-min))
      (re-search-forward "let primes")
      (beginning-of-line))

      (combobulate-step
      "Move to iarray"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "1")
      )

      (combobulate-step
      "Move to the immutable array [|..|]"
      (combobulate-navigate-logical-next)
      (expected-node-type "module_name" "2")
      )

      (combobulate-step
      "Move to the array"
      (combobulate-navigate-next)
      (expected-node-type "[" "3")
      )
      
    )))

(ert-deftest oxcaml-10 ()
  "Test navigation in stack allocation"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let use_temp"
      (goto-char (point-min))
      (re-search-forward "let use_temp")
      (beginning-of-line))

      (combobulate-step
      "Move to parameter"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "value_pattern" "1")
      )

      (combobulate-step
      "Move to the function type"
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "2")
      )

      (combobulate-step
      "Move to int ref"
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "3")
      (expected-thing-at-point "int" "3.1" 'symbol)
      )
      
    )))

(ert-deftest oxcaml-11 ()
  "Test navigation in unboxed floats"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let fast_square"
      (goto-char (point-min))
      (re-search-forward "let fast_square")
      (beginning-of-line))

      (combobulate-step
      "Move to parameter"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (expected-node-type "module_name" "1")
      )

      (combobulate-step
      "Move to mul"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      )

      (combobulate-step
      "Move to x"
      (combobulate-navigate-next)
      (expected-node-type "value_name" "3")
      (expected-thing-at-point "x" "3.1" 'symbol)
      )
      
    )))

(ert-deftest oxcaml-12 ()
  "Test navigation in unboxed constants"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let pi"
      (goto-char (point-min))
      (re-search-forward "let pi")
      (beginning-of-line))

      (combobulate-step
      "Move to float#"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "1")
      )
;; Bug: add a rule to navigate unboxed constants
      (combobulate-step
      "Move to #3.14.."
      (combobulate-navigate-next)
      (expected-node-type "unboxed_constant" "2")
      )
      
    )))

(ert-deftest oxcaml-13 ()
  "Test navigation for unboxed float#"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let distance"
      (goto-char (point-min))
      (re-search-forward "let distance")
      (beginning-of-line))

      (combobulate-step
      "Move to float#"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "1")
      )
      ;; Bug: add rule for navigating unboxed_type_constructors
      (combobulate-step
      "Move to let dx"
      (combobulate-navigate-next)
      (expected-node-type "let" "2")
      )
      
    )))

(ert-deftest oxcaml-14 ()
  "Test navigation for unboxed float#"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let distance"
      (goto-char (point-min))
      (re-search-forward "let distance")
      (beginning-of-line))

      (combobulate-step
      "Move to float#"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "1")
      )
      ;; Bug: add rule for navigating unboxed_type_constructors
      (combobulate-step
      "Move to let dx"
      (combobulate-navigate-next)
      (expected-node-type "let" "2")
      )
      
    )))

(ert-deftest oxcaml-15 ()
  "Test navigation for unboxed float#-b"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

     (combobulate-step
      "Move to let dx"
      (goto-char (point-min))
      (re-search-forward "let dx")
      (beginning-of-line))

      (combobulate-step
      "Move to Float_u sub"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "value_name" "1")
      )

      (combobulate-step
      "Move to a.x"
      (combobulate-navigate-next)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "a" "2.1" 'symbol)
      )
      
    )))

(ert-deftest oxcaml-16 ()
  "Test navigation for unboxed tuple"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let sincos"
      (goto-char (point-min))
      (re-search-forward "let sincos")
      (beginning-of-line))

    (combobulate-step
      "Move to (theta: float#)"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "(" "1")
      )

    (combobulate-step
      "Move to the unboxed tuple (float#, float#)"
      (combobulate-navigate-next)
      (expected-node-type "#(" "2")
      )

    (combobulate-step
      "Navigate the first sibling float#in the unboxed tuple"
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "3")
      )

     (combobulate-step
      "Navigate to the second sibling float# in the unboxed tuple"
      (combobulate-navigate-next)
      (expected-node-type "type_constructor" "4")
      )
      
    )))

(ert-deftest oxcaml-17 ()
  "Test navigation for modes"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to module Arena"
      (goto-char (point-min))
      (re-search-forward "module Arena")
      (beginning-of-line))

    (combobulate-step
      "Move to val create"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (expected-node-type "val" "1")
      )

    (combobulate-step
      "Move to unit"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "2")
     )

    (combobulate-step
      "Move to unique"
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (combobulate-navigate-down)
      (expected-node-type "mode" "3")
      )
    
    )))


(ert-deftest oxcaml-18 ()
  "Test navigation for comprehensions"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let triples"
      (goto-char (point-min))
      (re-search-forward "let triples")
      (beginning-of-line))

    (combobulate-step
      "Move to a"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "value_name" "1")
      (expected-thing-at-point "a" "1.1" 'symbol)
      )

    (combobulate-step
      "Move to b"
      (combobulate-navigate-next)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "b" "2.1" 'symbol)
     )

    (combobulate-step
      "Move to c"
      (combobulate-navigate-next)
      (expected-node-type "value_name" "3")
      (expected-thing-at-point "c" "3.1" 'symbol)
     )
    
    )))

(ert-deftest oxcaml-19 ()
  "Test navigation for comprehensions b"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let triples"
      (goto-char (point-min))
      (re-search-forward "let triples")
      (beginning-of-line))

    (combobulate-step
      "Move to for"
      (goto-char (point-min))
      (re-search-forward "for a")
      (back-to-indentation)
      (expected-node-type "for" "1"))

    (combobulate-step
      "Move to a"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "a" "2.1" 'symbol)
     )

    (combobulate-step
      "Move to 1"
      (combobulate-navigate-down)
      (expected-node-type "number" "3")
      (expected-thing-at-point "1" "3.1" 'symbol)
     )

     (combobulate-step
      "Move to n"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "4")
      (expected-thing-at-point "n" "4.1" 'symbol)
     )

    )))

(ert-deftest oxcaml-20 ()
  "Test navigation for comprehensions c"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let triples"
      (goto-char (point-min))
      (re-search-forward "let triples")
      (beginning-of-line))

    (combobulate-step
      "Move to for"
      (goto-char (point-min))
      (re-search-forward "for a")
      (back-to-indentation)
      (expected-node-type "for" "1"))

    (combobulate-step
      "Move to the next for"
      (combobulate-navigate-next)
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "b" "2.1" 'symbol)
     )

    )))

(ert-deftest oxcaml-21 ()
  "Test navigation for immutable array comprehensions"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let even_irray"
      (goto-char (point-min))
      (re-search-forward "let even_iarray")
      (beginning-of-line))

    (combobulate-step
      "Move to the comprehension"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (expected-node-type "[:" "1")
     )

    )))

(ert-deftest oxcaml-22 ()
  "Test navigation for immutable array comprehensions b"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let even_irray"
      (goto-char (point-min))
      (re-search-forward "let even_iarray")
      (beginning-of-line))

    (combobulate-step
      "Move to the comprehension"
      (re-search-forward "\\[:")
      (back-to-indentation)
      (expected-node-type "[:" "1")
     )

     (combobulate-step
      "Move to the x"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "x" "2.1" 'symbol)
     )

     (combobulate-step
      "Move to the first comprehension clause: for"
      (combobulate-navigate-next)
      (expected-node-type "for" "3")
      (expected-thing-at-point "for" "3.1" 'symbol)
     )


     (combobulate-step
      "Move to the second comprehension clause: when"
      (combobulate-navigate-next)
      (expected-node-type "when" "4")
      (expected-thing-at-point "when" "4.1" 'symbol)
     )

    )))


(ert-deftest oxcaml-23 ()
  "Test navigation for immutable array comprehensions c"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let even_irray"
      (goto-char (point-min))
      (re-search-forward "let even_iarray")
      (beginning-of-line))

    (combobulate-step
      "Move to the comprehension"
      (re-search-forward "\\[:")
      (back-to-indentation)
      (expected-node-type "[:" "1")
     )

     (combobulate-step
      "Move to the x"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "x" "2.1" 'symbol)
     )

     (combobulate-step
      "Move to the first comprehension clause: for"
      (combobulate-navigate-next)
      (expected-node-type "for" "3")
      (expected-thing-at-point "for" "3.1" 'symbol)
     )


     (combobulate-step
      "Move to the child of the comprehension clause for"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "4")
      (expected-thing-at-point "x" "4.1" 'symbol)
     )

    )))


(ert-deftest oxcaml-24 ()
  "Test navigation for comprehensions d"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let combos"
      (goto-char (point-min))
      (re-search-forward "let combos")
      (beginning-of-line))

    (combobulate-step
      "Move to Printf inside the comprehension"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "module_name" "1")
     )
;; BUG: navigating next enters a cyclic loop between comprehension clauses and the Printf string
     (combobulate-step
      "Move to the first comprehension clause"
      (combobulate-navigate-next)
      (expected-node-type "for" "2")
      (expected-thing-at-point "for" "2.1" 'symbol)
     )

    )))

(ert-deftest oxcaml-25 ()
  "Test navigation for identity polymorphic over locality mode"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let%template"
      (goto-char (point-min))
      (re-search-forward "let%template")
      (beginning-of-line))

    (combobulate-step
      "Move to the attribute"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "[@" "2")
     )
 ;; BUG: mode is not recongnized
    (combobulate-step
      "Move to the attribute payload"
      (combobulate-navigate-down)
      (expected-node-type "attribute_id" "2")
      (expected-thing-at-point "mode" "2.1" 'symbol)
     )

    )))

(ert-deftest oxcaml-26 ()
  "Test navigation for identity polymorphic over locality kind"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let%template"
      (goto-char (point-min))
      (re-search-forward "let%template")
      (beginning-of-line)
      (combobulate-navigate-next)
      )

    (combobulate-step
      "Move to the attribute"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "[@" "2")
     )
 ;; BUG: mode is not recongnized
    (combobulate-step
      "Move to the attribute payload"
      (combobulate-navigate-down)
      (expected-node-type "attribute_id" "2")
      (expected-thing-at-point "kind" "2.1" 'symbol)
     )

    )))

(ert-deftest oxcaml-27 ()
  "Test navigation for portable functors"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to module%template"
      (goto-char (point-min))
      (re-search-forward "module%template")
      (beginning-of-line)
      )
    
    ;; The entire attribute_id is template.portable which is not ideal as we don't have any node seperating template from portable and hence we can't navigate to portable.
    (combobulate-step
      "Move to template"
      (combobulate-navigate-down)
      (expected-node-type "attribute_id" "2")
     )

    (combobulate-step
      "Move to the body"
      (combobulate-navigate-down)
      (combobulate-navigate-next)
      (combobulate-navigate-next)
      (combobulate-navigate-down)
      (expected-node-type "let" "3")
      (expected-thing-at-point "let" "3.1" 'symbol)
     )

    )))

(ert-deftest oxcaml-28 ()
  "Test navigation in item extensions"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to module type List_ops"
      (goto-char (point-min))
      (re-search-forward "module type List")
      (beginning-of-line)
      )

    (combobulate-step
      "Move to the body"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "[%%" "2")
     )

    
    (combobulate-step
      "Move to the payload which is a floating attribute"
      (combobulate-navigate-down)
      (expected-node-type "[@@@" "3")
     )

    )))

(ert-deftest oxcaml-29 ()
  "Test sibling navigation for small number extensions"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let a"
      (goto-char (point-min))
      (re-search-forward "let a : float32 = 1.0")
      (beginning-of-line)
      (expected-node-type "let" "1")
      )

    (combobulate-step
      "Move to let a_unboxed"
       (combobulate-navigate-next)
       (expected-node-type "let" "2")
       (forward-word 2)
       (expected-thing-at-point "a_unboxed" "2.1" 'symbol
       ))

    (combobulate-step
      "Move to let b"
       (combobulate-navigate-next)
       (expected-node-type "let" "3")
       (forward-word 2)
       (expected-thing-at-point "b" "3.1" 'symbol
       ))

    (combobulate-step
      "Move to let b_unboxed"
       (combobulate-navigate-next)
       (expected-node-type "let" "4")
       (forward-word 2)
       (expected-thing-at-point "b_unboxed" "4.1" 'symbol
       ))

    (combobulate-step
      "Move to let c"
       (combobulate-navigate-next)
       (expected-node-type "let" "5")
       (forward-word 2)
       (expected-thing-at-point "c" "5.1" 'symbol
       ))

    (combobulate-step
      "Move to let c_unboxed"
       (combobulate-navigate-next)
       (expected-node-type "let" "6")
       (forward-word 2)
       (expected-thing-at-point "c_unboxed" "6.1" 'symbol
       ))

     (combobulate-step
      "Move to let d_unboxed"
       (combobulate-navigate-next)
       (expected-node-type "let" "7")
       (forward-word 2)
       (expected-thing-at-point "d_unboxed" "7.1" 'symbol
       ))

     (combobulate-step
      "Move to let e_unboxed"
       (combobulate-navigate-next)
       (expected-node-type "let" "8")
       (forward-word 2)
       (expected-thing-at-point "e_unboxed" "8.1" 'symbol
       ))

     (combobulate-step
      "Move to let f_unboxed"
       (combobulate-navigate-next)
       (expected-node-type "let" "9")
       (forward-word 2)
       (expected-thing-at-point "f_unboxed" "9.1" 'symbol
       ))

     (combobulate-step
      "Move back to let e_unboxed"
       (combobulate-navigate-previous)
       (expected-node-type "let" "10")
       (forward-word 2)
       (expected-thing-at-point "e_unboxed" "10.1" 'symbol
       ))

    )))

(ert-deftest oxcaml-30 ()
  "Test hierarchical navigation for small number extensions"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let a_unboxed"
      (goto-char (point-min))
      (re-search-forward "let a_unboxed")
      (beginning-of-line)
      (expected-node-type "let" "1")
      )

    (combobulate-step
      "Move to a_unboxed"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      )
      
    (combobulate-step
      "Move to float32#"
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "3")
      )

    (combobulate-step
      "Move to #1.0"
      (combobulate-navigate-next)
      (expected-node-type "unboxed_constant" "4")
      )

    )))

(ert-deftest oxcaml-31 ()
  "Test hierarchical navigation for small number extensions b"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to let e_unboxed"
      (goto-char (point-min))
      (re-search-forward "let e_unboxed")
      (beginning-of-line)
      (expected-node-type "let" "1")
      )

    (combobulate-step
      "Move to e_unboxed"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      )
      
    (combobulate-step
      "Move to char#"
      (combobulate-navigate-down)
      (expected-node-type "type_constructor" "3")
      )
    
    ;; BUG: node types are not recognized and treesitter shows errors
    (combobulate-step
      "Move to #'\123'"
      (combobulate-navigate-next)
      (expected-node-type "unboxed_constant" "4")
      )

    )))

(ert-deftest oxcaml-32 ()
  "Test hierarchical navigation for module type strenghtening extensions"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()

    (combobulate-step
      "Move to module type SS"
      (goto-char (point-min))
      (re-search-forward "module type SS")
      (back-to-indentation)
      (expected-node-type "module" "1")
      )

    (combobulate-step
      "Move to SS"
      (combobulate-navigate-down)
      (expected-node-type "module_type_name" "2")
      )

    (combobulate-step
      "Move to S"
      (combobulate-navigate-down)
      (expected-node-type "module_type_name" "2")
      )

    (combobulate-step
      "Move to M"
      (combobulate-navigate-down)
      (expected-node-type "module_name" "2")
      )

    )))

(ert-deftest oxcaml-33 ()
  "Test hierarchical navigation for stack allocation (tmp @ local and stack_)."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to tmp @ local"
       (goto-char (point-min))
       (re-search-forward "let tmp @ local")
       (goto-char (match-beginning 0))
       (expected-node-type "value_name" "1"))

      (combobulate-step
      "Move to tmp"
      (combobulate-navigate-down)
      (expected-node-type "value_name" "2")
      (expected-thing-at-point "tmp" "2.1" 'symbol))

      (combobulate-step
      "Move to local"
      (combobulate-navigate-down)
      (combobulate-navigate-down)
      (expected-node-type "mode" "3")
      (expected-thing-at-point "mode" "3.1" 'symbol))

      (combobulate-step
      "Move to stack_"
      (combobulate-navigate-next)
      (expected-node-type "stack_" "4")
      (expected-thing-at-point "stack_" "4.1" 'symbol))
      
    )))

(ert-deftest oxcaml-34 ()
  "Test sibling navigation for Unboxed Types (let big : int64#)."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to let big"
       (goto-char (point-min))
       (re-search-forward "let big")
       (beginning-of-line)
       )
     (combobulate-step "Navigate to big"
       (combobulate-navigate-down)
       (expected-node-type "value_name" "1"))
     (combobulate-step "Navigate to type int64#"
       (combobulate-navigate-down)
       (expected-node-type "type_constructor" "2"))
    (combobulate-step "Navigate to #9_000_000_000L"
       (combobulate-navigate-next)
       (expected-node-type "unboxed_constant" "3"))
  )))

(ert-deftest oxcaml-35 ()
  "Test hierarchical navigation for Modes (let arena @ unique)."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to let arena @ unique"
       (goto-char (point-min))
       (re-search-forward "let arena @ unique")
       (goto-char (match-beginning 0))
       )

    (combobulate-step "Navigate to unique"
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (expected-node-type "mode" "1")
       (expected-thing-at-point "unique" "1.1" 'symbol))

    (combobulate-step "Navigate to Arena.create ()"
       (combobulate-navigate-next)
       (expected-node-type "module_name" "2")
       (expected-thing-at-point "Arena" "2.1" 'symbol))
       
  )))

(ert-deftest oxcaml-36 ()
  "Test Kinds (value mod contended portable, as _ immediate)."
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to module T"
       (goto-char (point-min))
       (re-search-forward "module T")
       (goto-char (match-beginning 0))
       (expected-node-type "module_name" "1"))

    (combobulate-step "Navigate to the first type"
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-next)
       (expected-node-type "type" "2")
       (expected-thing-at-point "type" "2.1" 'symbol))

    (combobulate-step "Navigate to value"
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-next)
       (expected-node-type "jkind_abbreviation" "3")
       (expected-thing-at-point "value" "3.1" 'symbol))

    (combobulate-step "Navigate to the second value"
       (combobulate-navigate-next)
       (combobulate-navigate-next)
       (expected-node-type "jkind_abbreviation" "3")
       (expected-thing-at-point "value" "3.1" 'symbol))

    )))


(ert-deftest oxcaml-37 ()
  "Test Kinds (value mod contended portable, as _ immediate). b"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to module T"
       (goto-char (point-min))
       (re-search-forward "module T")
       (goto-char (match-beginning 0))
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-next)
       (expected-node-type "type" "1")
       (expected-thing-at-point "type" "1.1" 'symbol))

    (combobulate-step "Navigate to value"
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-next)
       (expected-node-type "jkind_abbreviation" "2")
       (expected-thing-at-point "value" "2.1" 'symbol))

    (combobulate-step "Navigate to contended"
       (combobulate-navigate-down)
       (expected-node-type "mode" "3")
       (expected-thing-at-point "contended" "3.1" 'symbol))
   
     (combobulate-step "Navigate to portable"
       (combobulate-navigate-down)
       (expected-node-type "mode" "4")
       (expected-thing-at-point "portable" "4.1" 'symbol))

    )))

(ert-deftest oxcaml-38 ()
  "Test for flattened nested arrays"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to let flatten"
       (goto-char (point-min))
       (re-search-forward "let flatten")
       (beginning-of-line)
       (expected-node-type "let" "1"))
     (combobulate-step "Navigate to xss"
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (expected-node-type "value_name" "2")
       (expected-thing-at-point "xss" "2.1" 'symbol))
      
      (combobulate-step "Navigate to 'a array"
       (combobulate-navigate-next)
       (expected-node-type "type_variable" "3")
       (expected-sexp-at-point "'a" "3.1" 'symbol)
       )

     )))

(ert-deftest oxcaml-38 ()
  "Test for flattened nested arrays"
  :tags '(oxcaml implementation navigation combobulate)
  (skip-unless (treesit-language-available-p 'ocaml))
  (with-tuareg-buffer
   (lambda ()
     (combobulate-step "Move to let flatten"
       (goto-char (point-min))
       (re-search-forward "let flatten")
       (beginning-of-line)
       (expected-node-type "let" "1"))
     (combobulate-step "Navigate to xss"
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (combobulate-navigate-down)
       (expected-node-type "value_name" "2")
       (expected-thing-at-point "xss" "2.1" 'symbol))
      
      (combobulate-step "Navigate to 'a array"
       (combobulate-navigate-next)
       (expected-node-type "type_variable" "3")
       (expected-sexp-at-point "'a" "3.1" 'symbol)
       )

     )))

(provide 'test-oxcaml-implementation-navigation)
;;; test-oxcaml-implementation-navigation.el ends here