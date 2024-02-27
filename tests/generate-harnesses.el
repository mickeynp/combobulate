;;; generate-harnesses.el --- generates test harnesses  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
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

;;

;;; Code:

(require 'combobulate-test-prelude)
(require 'combobulate-test-suite)

(defconst combobulate-test-suites
  (list
   ;; General-purpose envelope tests that expand in a blank file.
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-envelope
    :fixture-files '("fixtures/envelope/blank.py")
    :collection-name "combobulate-envelope-expand-instructions"
    :action-body '((combobulate-envelope-expand-instructions instructions))
    :per-marker nil
    :reverse nil
    :harness-factory-matrix
    '((:test-name "string-basic"
                  :instructions ("test string")
                  :mock-registers nil)
      (:test-name "string-multiple"
                  :mock-registers nil
                  :instructions ("a" "b" "c"))
      (:test-name "newline"
                  :instructions  ("a = 1" n "b = 1"))
      (:test-name "newline-and-indent-simple"
                  :instructions  ("a = 1" n> "b = 1"))
      (:test-name "newline-and-indent-inside-block-then-outside"
                  :instructions  ("if 1:" n> "b = 1" n "c = 1"))
      (:test-name "newline-and-indent-inside-block-both"
                  :instructions ("if 1:" n>
                                 "b = 1" n>
                                 "c = 1" n>
                                 "while True:" n>
                                 "d = 3"))
      (:test-name "save-column"
                  :instructions  ("def Foo():" n>
                                  (save-column "try:" n>
                                               "do_something()" n)
                                  "except:" n> "pass"))
      (:test-name "save-column-nested"
                  :instructions  ("def Foo():" n>
                                  (save-column "try:" n>
                                               (save-column "with some_stuff() as foo:" n>
                                                            "pass")
                                               n)
                                  "except:" n> "pass"))
      ;; register stuff
      (:test-name "insert-region-register"
                  :instructions  (r))
      (:test-name "insert-region-register-then-indent"
                  :instructions ("if True:" n> r)
                  :mock-registers ((region . "random = 1")))
      (:test-name "insert-region-register-2-then-indent"
                  :instructions ("if True:" n> (r some-register))
                  :mock-registers ((some-register . "my_register = 1")))
      (:test-name "insert-missing-register-with-default"
                  :instructions ("if True:" n> (r some-register "foo"))
                  :mock-registers nil)
      ;; prompting with preset prompt register values
      (:test-name "prompt-register-once"
                  :instructions ("a = " (p some-prompt "Pick a value"))
                  :mock-registers ((some-prompt . "foo")))
      (:test-name "prompt-register-reused"
                  :instructions ("a = " (p some-prompt "Pick a value")
                                 n>
                                 "b = " (f some-prompt))
                  :mock-registers ((some-prompt . "this is a prompt value")))
      (:test-name "prompt-manual-input-once"
                  :instructions ("a = " (p some-prompt "Pick a value"))
                  :mock-prompt-actions ("simulated prompt value"))
      (:test-name "prompt-manual-input-twice"
                  :instructions ("a = " (p some-prompt "Pick a value") n>
                                 "b = " (p another-prompt "Pick a second value"))
                  :mock-prompt-actions ("simulated prompt value"
                                        "second value"))
      (:test-name "prompt-manual-keyboard-quit"
                  :instructions ("a = " (p some-prompt "Pick a value") n>
                                 "b = " (p another-prompt "Pick a value"))
                  :mock-prompt-actions ("foo" keyboard-quit))
      (:test-name "field-before-prompt"
                  :instructions ("a = " (f some-prompt) n>
                                 "b = " (p some-prompt "Pick a value"))
                  :mock-prompt-actions ("blah"))
      ;; repeat
      ;; (:test-name "repeat-simple"
      ;;             :instructions (repeat "if 1:" n>
      ;;                                   "a = " (p repeating-prompt "Pick a value") n>
      ;;                                   "b = 1" n>)
      ;;             :mock-prompt-actions ("blah")
      ;;             :mock-expansion-actions (yes yes no))
      ;; choice and choice*
      (:test-name "choice*-with-complex-missing-field"
                  :instructions
                  ("{" @ "null" >
                   n > " ? " @ (choice* :name "consequence" :missing ("null") :rest (r>))
                   n > " : " (choice* :name "alternative" :missing ("<" (p other "SOME TAG") "/>") :rest (r>))
                   n > "}" >)
                  :mock-proffer-choices (0 0)
                  :mock-registers ((region . "<div>Some jsx element</div>"))
                  :mock-prompt-actions ("mytag"))
      ;; simple choice
      (:test-name "choice-simple-0"
                  :instructions ("{a + " (choice "1") (choice "2") "}")
                  :mock-proffer-choices (0))
      (:test-name "choice-simple-1"
                  :instructions ("{a + " (choice "1") (choice "2") "}")
                  :mock-proffer-choices (1))))
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-envelope
    :fixture-files '("fixtures/envelope/component.tsx")
    :collection-name "combobulate-envelope-expand-instructions-tsx"
    :action-body '((combobulate-envelope-expand-instructions instructions))
    :per-marker nil
    :reverse nil
    :harness-factory-matrix
    '(;; choice and choice*
      (:test-name "choice*-with-complex-missing-field-consequence"
                  :instructions
                  ("{" @ "null" >
                   n > " ? " @ (choice* :name "consequence" :missing ("null") :rest (r>))
                   n > " : " (choice* :name "alternative" :missing ("<" (p other "SOME TAG") "/>") :rest (r>))
                   n > "}" >)
                  :mock-proffer-choices (1 0)
                  :mock-registers ((region . "<div>Some jsx element</div>"))
                  :mock-prompt-actions nil)
      (:test-name "choice*-with-complex-missing-field-alt"
                  :instructions
                  ("{" @ "null" >
                   n > " ? " @ (choice* :name "consequence" :missing ("null") :rest (r>))
                   n > " : " (choice* :name "alternative" :missing ("<" (p other "SOME TAG") "/>") :rest (r>))
                   n > "}" >)
                  :mock-proffer-choices (0 0)
                  :mock-registers ((region . "<div>Some jsx element</div>"))
                  :mock-prompt-actions ("mytag"))
      ;; simple choice
      (:test-name "choice-simple-0"
                  :instructions ("{a + " (choice "1") (choice "2") "}")
                  :mock-proffer-choices (0))
      (:test-name "choice-simple-1"
                  :instructions ("{a + " (choice "1") (choice "2") "}")
                  :mock-proffer-choices (1))
      ;; choice has choice
      (:test-name "choice-has-choice-0"
                  :instructions ("{a + " (choice "1" (choice "1.1") (choice "1.2")) (choice "2") "}")
                  :mock-proffer-choices (0 0))
      (:test-name "choice-has-choice-1"
                  :instructions ("{a + " (choice "1" (choice "1.1") (choice "1.2")) (choice "2") "}")
                  :mock-proffer-choices (0 1))
      (:test-name "choice-has-choice-2"
                  :instructions ("{a + " (choice "1" (choice "1.1") (choice "1.2")) (choice "2") "}")
                  :mock-proffer-choices (1 0))
      (:test-name "choice-has-choice-3"
                  :instructions ("{a + " (choice "1" (choice "1.1") (choice "1.2")) (choice "2") "}")
                  :mock-proffer-choices (1 1))))
   ;; Dragging
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-with-fixture-delta
    :fixture-files "fixtures/sibling/*"
    :collection-name "combobulate-drag-down"
    :action-body '((combobulate-drag-down))
    :per-marker t
    :reverse nil)
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-with-fixture-delta
    :fixture-files "fixtures/sibling/*"
    :collection-name "combobulate-drag-up"
    :action-body '((combobulate-drag-up))
    :per-marker t
    :reverse t)
   ;; Splicing
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-with-fixture-delta
    :fixture-files "fixtures/splice/*"
    :collection-name "combobulate-splice-up"
    :action-body '((combobulate-with-stubbed-proffer-choices (:choices '(0 0 0 0 0 0))
                     (combobulate-splice-up)))
    :per-marker t
    :reverse nil)
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-with-fixture-delta
    :fixture-files "fixtures/splice/*"
    :collection-name "combobulate-splice-self"
    :action-body '((combobulate-with-stubbed-proffer-choices (:choices '(0 0 0 0 0 0))
                     (combobulate-splice-self)))
    :per-marker t
    :reverse nil)
   ;; Cloning
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-with-fixture-delta
    :fixture-files "fixtures/clone/*"
    :collection-name "combobulate-clone-dwim"
    :action-body '((combobulate-with-stubbed-proffer-choices (:choices '(0 0 0 0))
                     (combobulate-clone-node-dwim)))
    :per-marker t
    :reverse nil)
   ;; Sibling navigation
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-marker-loop
    :fixture-files "fixtures/sibling/*"
    :collection-name "combobulate-navigate-previous"
    :action-body '((combobulate-navigate-previous))
    :per-marker nil
    :reverse t)
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-marker-loop
    :fixture-files "fixtures/sibling/*"
    :collection-name "combobulate-navigate-next"
    :action-body '((combobulate-navigate-next))
    :per-marker nil
    :reverse nil)
   ;; Parent navigation
   ;; child navigation
   (combobulate-test-suite
    :harness-factory #'combobulate-test-harness-marker-loop
    :fixture-files "fixtures/down/*"
    :collection-name "combobulate-navigate-down"
    :action-body '((combobulate-navigate-down))
    :per-marker nil
    :harness-factory-args)))

(mapc #'combobulate-test-suite-generate-test-suite
      combobulate-test-suites)

(provide 'generate-harnesses)
;;; generate-harnesses.el ends here
