
(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'combobulate-generate-tests)
(eval-when-compile
  (require 'cl-lib))


(combobulate-test
    (:language python :mode python-ts-mode :fixture
               "./fixtures/envelope/class.py")
  (goto-marker 1) delete-markers
  (let
      ((combobulate-envelope-prompt-actions)
       (combobulate-envelope-prompt-expansion-actions))
    (font-lock-update)
    (font-lock-fontify-buffer)
    (combobulate--mark-node (combobulate--get-nearest-navigable-node) t)
    (combobulate-envelope-expand-instructions
     '("if " @ "Condition" ":" n>
       (choice* :missing
                nil
                :rest
                (@@ r>)
                :name "if-block")
       (choice* :missing
                nil
                :rest
                (@@ "pass" n> "else:" n> r>)
                :name "else-block"))
     ))
  debug-show)

(combobulate-test
    (:language python :mode python-ts-mode :fixture
               "./fixtures/envelope/class.py")
  (goto-marker 1) delete-markers
  (let
      ((combobulate-envelope-prompt-actions)
       (combobulate-envelope-prompt-expansion-actions))
    (font-lock-update)
    (font-lock-fontify-buffer)
    (combobulate--mark-node (combobulate--get-nearest-navigable-node) t)
    (combobulate-envelope-expand-instructions
     '((save-column
        "try:" n>
        (choice* :missing
                 (@@ "pass")
                 :rest
                 (@@ r>))
        n)
       "except "
       "Exception"
       ":" n>
       (choice* :missing
                (@@ "pass" n>)
                :rest
                (@@ r> n)))))
  debug-show)

(combobulate-test
    (:language tsx :mode tsx-ts-mode :fixture
               "./fixtures/envelope/blank.tsx")
  (goto-marker 1) delete-markers
  (let
      ((combobulate-envelope-prompt-actions '("blah"))
       (combobulate-envelope-prompt-expansion-actions
        '(yes yes no)))
    (font-lock-update)
    (combobulate-envelope-expand-instructions
     '("function MyComponent({ a, b}: {a: number, b: string}) {" n>
       "return <div>" n>
       @ "BEFORE"
       (choice "one") (choice "two")
       @ "AFTER"
       (choice "")
       "</div>" n>
       "}")))
  debug-show)


(combobulate-test
    (:language tsx :mode tsx-ts-mode :fixture
               "./fixtures/envelope/blank.tsx")
  (goto-marker 1) delete-markers
  (let
      ((combobulate-envelope-prompt-actions '("blah"))
       (combobulate-envelope-prompt-expansion-actions
        '(yes yes no)))
    (combobulate-envelope-expand-instructions
     '("function MyComponent({ a, b}: {a: number, b: string}) {" n>
       "return <div>" n>
       "<" ">" (choice "<" (p subtag "sub-tag") ">" "SUB SOMETHING ELSE" "</" (f subtag) ">")
       "Middle text"
       (choice "Some random text")
       "</"  ">"  n>
       "</div>" n>
       "}")))
  debug-show)


(combobulate-test
    (:language tsx :mode tsx-ts-mode :fixture
               "./fixtures/envelope/blank.tsx")
  (goto-marker 1) delete-markers
  (let
      ((combobulate-envelope-prompt-actions '("blah"))
       (combobulate-envelope-prompt-expansion-actions
        '(yes yes no)))
    (combobulate-envelope-expand-instructions
     '("function MyComponent({ a, b}: {a: number, b: string}) {" n>
       "return <div>" @ n>
       (choice "Hello World" @)
       (choice "<"(p tag "tag") ">" (choice "<" (p subtag "sub-tag") ">" "SUB SOMETHING ELSE" "</" @ (f subtag) ">")
               "Middle text"
               (choice "Some random text --- oh, and here's a tag:" @ (f tag))
               "</" (f tag) ">")
       "</div>" n>
       "}")))
  debug-show)



(combobulate-test
    (:language tsx :mode tsx-ts-mode :fixture
               "./fixtures/envelope/blank.tsx")
  (goto-marker 1) delete-markers
  (let
      ((combobulate-envelope-prompt-actions '("blah"))
       (combobulate-envelope-prompt-expansion-actions
        '(yes yes no)))
    (combobulate-envelope-expand-instructions
     '("function MyComponent({ a, b}: {a: number, b: string}) {" n>
       "return <div>" @ n>
       (choice "Hello World" @)
       (choice "Goodbye " (choice "World") (choice "Earth") @)
       "</div>" n>
       "}")))
  debug-show)
