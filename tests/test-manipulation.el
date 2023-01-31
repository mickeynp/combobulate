;;; test-manipulation.el --- tests for manipulation routines  -*- lexical-binding: t; -*-

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

(load-library "./test-prelude")

(ert-deftest combobulate-test-edit-procedures ()
  (combobulate-test (:language python :mode python-ts-mode :setup combobulate-test-python)
    (let* ((node (combobulate-node-at-point '("try_statement")))
           (block-node (combobulate-node-child node 0))
           (result))
      ;; no match. point outside of block
      (should (equal (car-safe (combobulate-procedure-find-applicable-procedures
                                block-node '((:activation-nodes
                                              ((:node "block" :find-parent "try_statement" :position at-or-in))))))
                     nil))
      (combobulate--goto-node block-node)
      ;; match. point is at the edge.
      (setq result (car-safe (combobulate-procedure-find-applicable-procedures
                              block-node '((:activation-nodes
                                            ((:node "block" :find-parent "try_statement" :position at-or-in)))))))
      (should (combobulate-node-eq (car result) node))
      (should (equal (cdr result)
                     '(:activation-nodes
                       ((:node "block" :find-parent "try_statement" :position at-or-in)))))

      ;; match. point is inside.
      (forward-line 1)
      (setq result (car-safe (combobulate-procedure-find-applicable-procedures
                              block-node '((:activation-nodes
                                            ((:node "block" :find-parent "try_statement" :position at-or-in)))))))
      (should (combobulate-node-eq (car result) node))
      (should (equal (cdr result)
                     '(:activation-nodes
                       ((:node "block" :find-parent "try_statement" :position at-or-in)))))
      ;;  no match. position is `at'
      (setq result (car-safe (combobulate-procedure-find-applicable-procedures
                              block-node '((:activation-nodes
                                            ((:node "block" :find-parent "try_statement" :position at)))))))
      (should (equal result nil))
      ;;  match. position is `at'
      (goto-char (point-min))
      (setq result (car-safe (combobulate-procedure-find-applicable-procedures
                              node '((:activation-nodes ((:node "try_statement" :position at)))))))
      (should (combobulate-node-eq (car result) node))
      (should (equal (cdr result)
                     '(:activation-nodes
                       ((:node "try_statement" :position at))))))))


(provide 'test-manipulation)
;;; test-manipulation.el ends here
