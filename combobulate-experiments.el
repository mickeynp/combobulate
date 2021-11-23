;;; combobulate-experiments.el --- experimental features for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>

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

;;; Code:

(defun combobulate--swap-node-regions (node-a node-b)
  "Swaps the region substring in NODE-A with NODE-B"
  (save-excursion
    (transpose-subr-1 (tsc-node-position-range node-a) (tsc-node-position-range node-b))))

(defun combobulate--drag (direction)
  (let* ((up (eq direction 'up))
         (node-a (or (combobulate--get-nearest-navigable-node)
                     (error "No navigable node")))
         (node-b (or (combobulate--navigate (if up 'previous 'next))
                     (error "No node"))))
    (if up
        (combobulate--swap-node-regions node-a node-b)
      (combobulate--swap-node-regions node-b node-a))
    (combobulate--move-point-to-node (combobulate--navigate (if up 'previous 'next)))))

(defun combobulate-drag-up ()
  (interactive)
  (combobulate--drag 'up)
  (recenter))

(defun combobulate-drag-down ()
  (interactive)
  (combobulate--drag 'down))





(provide 'combobulate-experiments)
;;; combobulate-experiments.el ends here
