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



(defun combobulate--mc-assert-is-supported ()
  (unless (fboundp 'multiple-cursors-mode)
    (error "Multiple cursors is not installed or activated.")))

(defun combobulate--mc-active ()
  "Return non-nil if multiple cursors mode is active."
  (and (fboundp 'multiple-cursors-mode)
       multiple-cursors-mode))

(defun combobulate--mc-clear-cursors ()
  "Clear multiple cursors."
  (when (combobulate--mc-active)
    (mc/remove-fake-cursors)))

(defun combobulate--mc-enable ()
  "Enable multiple cursors."
  (when (combobulate--mc-active)
    ;; abysmal MC hack to prevent MC from triggering on the damned
    ;; command that started the whole thing.
    (dolist (ignored-command '(combobulate-edit-cluster-dwim
                               combobulate-edit-node-type-dwim
                               combobulate-edit-siblings-dwim
                               combobulate-edit-node-by-text-dwim
                               combobulate-query-builder-edit-nodes
                               combobulate-edit-query))
      (add-to-list 'mc--default-cmds-to-run-once ignored-command))
    (mc/maybe-multiple-cursors-mode)))

(defun combobulate--mc-place-cursor ()
  "Place a cursor at the current node."
  (when (combobulate--mc-active)
    (mc/create-fake-cursor-at-point)))
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
   "./fixtures/python-dict.py"))

(ert-deftest combobulate-test-mc-combobulate-edit-cluster-dwim ()
  :tags '(multiple-cursors manipulation)
  (with-stubbed-mc
   #'combobulate-edit-cluster-dwim
   python
   python-ts-mode
   "./fixtures/python-dict.py"))

(provide 'test-edit-mc-nodes)
;;; test-edit-mc-nodes.el ends here

