;;; test-edit-mc-nodes.el --- test for editing nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

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
(require 'combobulate-manipulation)
(require 'combobulate-contrib)
(when (fboundp 'multiple-cursors-mode)
  (require 'multiple-cursors))


(defmacro with-stubbed-mc (edit-fn language mode fixture)
  `(let ((stub/combobulate--mc-clear-cursors)
         (stub/combobulate--mc-enable)
         (stub/combobulate--mc-place-cursor))
     (cl-letf (((symbol-function 'combobulate--mc-active)
                (lambda () nil))
               ((symbol-function 'combobulate--mc-assert-is-supported)
                (lambda () t))
               ((symbol-function 'combobulate--mc-clear-cursors)
                (lambda () (setq stub/combobulate--mc-clear-cursors t)))
               ((symbol-function 'combobulate--mc-enable)
                (lambda () (setq stub/combobulate--mc-enable t)))
               ((symbol-function 'combobulate--mc-place-cursor)
                (lambda () (push (point) stub/combobulate--mc-place-cursor))))
       (combobulate-test (:language ,language :mode ,mode :fixture ,fixture)
         (goto-marker 1)
         ;; to account for point being at the start of marker 1, which
         ;; would otherwise occupy a cursor.
         (push (point) stub/combobulate--mc-place-cursor)
         (call-interactively ,edit-fn)
         (should (equal stub/combobulate--mc-clear-cursors t))
         (should (equal stub/combobulate--mc-enable t))
         (should (equal (sort stub/combobulate--mc-place-cursor #'>)
                        (sort (mapcar #'overlay-start (combobulate--with-test-overlays)) #'>)))))))

(ert-deftest combobulate-test-mc-combobulate-edit-cluster-dwim ()
  :tags '(multiple-cursors manipulation)
  (with-stubbed-mc
   #'combobulate-edit-cluster-dwim
   python
   python-ts-mode
   "./fixtures/mc-edit/python-dict-keys.py"))

(ert-deftest combobulate-test-mc-combobulate-edit-node-by-text-dwim ()
  :tags '(multiple-cursors manipulation)
  (combobulate-with-stubbed-proffer-choices (:choices '(0))
    (with-stubbed-mc #'combobulate-edit-node-by-text-dwim
                     tsx
                     tsx-ts-mode
                     "./fixtures/mc-edit/identifiers-named-c.tsx")))

(ert-deftest combobulate-test-mc-combobulate-edit-node-siblings-dwim ()
  :tags '(multiple-cursors manipulation)
  (combobulate-with-stubbed-proffer-choices (:choices '(0))
    (with-stubbed-mc #'combobulate-edit-node-siblings-dwim
                     css
                     css-ts-mode
                     "./fixtures/mc-edit/property.css")))

(ert-deftest combobulate-test-mc-combobulate-edit-node-type-dwim ()
  :tags '(multiple-cursors manipulation)
  (combobulate-with-stubbed-proffer-choices (:choices '(1))
    (with-stubbed-mc #'combobulate-edit-node-type-dwim
                     python
                     python-ts-mode
                     "./fixtures/mc-edit/python-dict-values.py")))

(provide 'test-edit-mc-nodes)
;;; test-edit-mc-nodes.el ends here

