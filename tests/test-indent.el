(require 'combobulate-test-prelude)

(ert-deftest test-strip-whitespace-from-blank-input ()
  (should (string= "" (combobulate-indent-string--strip-whitespace "   "))))

(ert-deftest test-strip-whitespace-from-nonleading-whitespace-input ()
  (should (string= "abc" (combobulate-indent-string--strip-whitespace "abc"))))

(ert-deftest test-strip-whitespace-from-leading-whitespace-input ()
  (should (string= "abc" (combobulate-indent-string--strip-whitespace "   abc"))))

(ert-deftest test-strip-whitespace-from-leading-and-trailing-whitespace_input ()
  (should (string= "abc " (combobulate-indent-string--strip-whitespace "   abc "))))

(ert-deftest test-strip-whitespace-with-count-arg ()
  (should (string= "   abc" (combobulate-indent-string--strip-whitespace "     abc" 2))))

(ert-deftest test-strip-whitespace-with-zero-count-arg ()
  (should (string= " abc" (combobulate-indent-string--strip-whitespace " abc" 0))))

(ert-deftest test-strip-non-space-whitespace ()
  (should (string= "\tabc" (combobulate-indent-string--strip-whitespace " \tabc" 1))))

(ert-deftest test-count-whitespace-from-blank-input ()
  (should (= 3 (combobulate-indent-string--count-whitespace "   "))))

(ert-deftest test-count-whitespace-from-nonleading-whitespace-input ()
  (should (= 0 (combobulate-indent-string--count-whitespace "abc"))))

(ert-deftest test-count-whitespace-from-leading-whitespace-input ()
  (should (= 3 (combobulate-indent-string--count-whitespace "   abc"))))

(ert-deftest test-count-whitespace-from-leading-and-trailing-whitespace_input ()
  (should (= 3 (combobulate-indent-string--count-whitespace "   abc "))))

(ert-deftest test-count-whitespace-from-non-space-whitespace-characters ()
  (should (= 3 (combobulate-indent-string--count-whitespace " \t abc"))))


(ert-deftest test-combobulate-indent-string-first-line-indent-first-line-only ()
  "Test that only the first line is indented to the target column."
  (should (string= "    abc\ndef"
                   (combobulate-indent-string-first-line "abc\ndef" 4))))

(ert-deftest test-combobulate-indent-string-first-line-indent-first-line-with-existing-indentation ()
  "Test that current indentation is removed before the new one is applied."
  (should (string= "        def\nghi"
                   (combobulate-indent-string-first-line "    def\nghi" 8))))

(ert-deftest test-combobulate-indent-string-first-line-no-leading-whitespace ()
  "Test for `combobulate-indent-string-first-line` with no leading whitespace."
  (let* ((text "no whitespace")
         (target-col 2)
         (expected "  no whitespace"))
    (should (equal (combobulate-indent-string-first-line text target-col) expected))))

(ert-deftest test-combobulate-indent-string-first-line-existing-leading-whitespace ()
  "Test for `combobulate-indent-string-first-line` with existing leading whitespace."
  (let* ((text "  existing whitespace")
         (target-col 4)
         (expected "    existing whitespace"))
    (should (equal (combobulate-indent-string-first-line text target-col) expected))))

(ert-deftest test-combobulate-indent-string-first-line-tab-characters ()
  "Test for `combobulate-indent-string-first-line` with tab characters."
  (let* ((text "\ttab character")
         (target-col 4)
         (expected "    tab character"))
    (should (equal (combobulate-indent-string-first-line text target-col) expected))))

(ert-deftest test-combobulate-indent-string-first-line-multiple-lines ()
  "Test for `combobulate-indent-string-first-line` with multiple lines."
  (let* ((text "first line\nsecond line")
         (target-col 4)
         (expected "    first line\nsecond line"))
    (should (equal (combobulate-indent-string-first-line text target-col) expected))))

(ert-deftest test-combobulate-indent-subtract-2 ()
  (should (equal (combobulate-indent-string-1 " foo" 'subtract 2)
                 "foo"))
  (should (equal (combobulate-indent-string-1 "  foo" 'subtract 2)
                 "foo"))
  (should (equal (combobulate-indent-string-1 "   foo" 'subtract 2)
                 " foo")))

(ert-deftest test-combobulate-indent-add-0-to-2 ()
  (should (equal (combobulate-indent-string-1 " foo" 'add 0)
                 " foo"))
  (should (equal (combobulate-indent-string-1 "foo" 'add 2)
                 "  foo"))
  (should (equal (combobulate-indent-string-1 "foo" 'add 1)
                 " foo"))
  (should (equal (combobulate-indent-string-1 "foo" 'add 0)
                 "foo")))

(ert-deftest test-combobulate-indent-absolute-0-to-2 ()
  (should (equal (combobulate-indent-string-1 " foo" 'absolute 0)
                 "foo"))
  (should (equal (combobulate-indent-string-1 " foo" 'absolute 1)
                 " foo"))
  (should (equal (combobulate-indent-string-1 " foo" 'absolute 2)
                 "  foo"))
  (should (equal (combobulate-indent-string-1 "foo" 'absolute 2)
                 "  foo"))
  (should (equal (combobulate-indent-string-1 "foo" 'absolute 1)
                 " foo"))
  (should (equal (combobulate-indent-string-1 "foo" 'absolute 0)
                 "foo")))

(ert-deftest test-combobulate-indent-tests-subtract-more-than-available ()
  (should (equal (combobulate-indent-string-1 "  foo" 'subtract 3)
                 "foo")))

(ert-deftest test-combobulate-indent-no-starting-space ()
  (should (equal (combobulate-indent-string-1 "foo" 'subtract 2)
                 "foo")))

(ert-deftest test-combobulate-indent-only-spaces ()
  (should (equal (combobulate-indent-string-1 "   " 'subtract 2)
                 " ")))

(ert-deftest test-combobulate-indent-add-negative ()
  (should (equal (combobulate-indent-string-1 " foo" 'add -1)
                 "foo")))

(ert-deftest test-combobulate-indent-subtract-negative ()
  (should (equal (combobulate-indent-string-1 " foo" 'subtract -1)
                 "  foo")))

(ert-deftest test-combobulate-indent-subtract-more-than-indent ()
  (should (equal (combobulate-indent-string-1 " foo" 'subtract 3)
                 "foo")))

(ert-deftest test-combobulate-indent-string-first-line ()
  (should (equal (combobulate-indent-string "foo\nbar\nbaz"
                                            :first-line-operation 'add
                                            :first-line-amount 4)
                 "    foo\nbar\nbaz")))

(ert-deftest test-combobulate-indent-string-rest-lines ()
  (should (equal (combobulate-indent-string "foo\nbar\nbaz"
                                            :rest-lines-operation 'add
                                            :rest-lines-amount 4)
                 "foo\n    bar\n    baz")))

(ert-deftest test-combobulate-indent-string-both ()
  (should (equal (combobulate-indent-string "foo\nbar\nbaz"
                                            :first-line-operation 'add
                                            :first-line-amount 5
                                            :rest-lines-operation 'subtract
                                            :rest-lines-amount 2)
                 "     foo\nbar\nbaz")))

(ert-deftest test-combobulate-indent-string-empty-string ()
  (should (equal (combobulate-indent-string ""
                                            :first-line-operation 'add
                                            :first-line-amount 4)
                 "    "))
  (should (equal (combobulate-indent-string ""
                                            :rest-lines-operation 'add
                                            :rest-lines-amount 4)
                 "")))

(ert-deftest test-combobulate-indent-string-single-line ()
  (should (equal (combobulate-indent-string "foo"
                                            :first-line-operation 'add
                                            :first-line-amount 4)
                 "    foo"))
  (should (equal (combobulate-indent-string "foo"
                                            :rest-lines-operation 'add
                                            :rest-lines-amount 4)
                 "foo")))



(ert-deftest test-combobulate-indent-string-relative ()
  ;; adding with relative shifts everything by the
  ;; `:first-line-amount'.
  (should (equal (combobulate-indent-string "    if True:\n        print('foo')"
                                            :first-line-operation 'add
                                            :first-line-amount 4
                                            :rest-lines-operation 'relative)
                 "        if True:\n            print('foo')"))
  ;; subtracting is the same as adding a negative number
  (should (equal (combobulate-indent-string "    if True:\n        print('foo')"
                                            :first-line-operation 'subtract
                                            :first-line-amount 4
                                            :rest-lines-operation 'relative)
                 "if True:\n    print('foo')"))
  (should (equal (combobulate-indent-string "    if True:\n        print('foo')"
                                            :first-line-operation 'absolute
                                            :first-line-amount 0
                                            :rest-lines-operation 'relative)
                 "if True:\n    print('foo')")))
