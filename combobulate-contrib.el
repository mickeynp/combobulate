;;; combobulate-contrib.el --- third-party integrations for combobulate  -*- lexical-binding: t; -*-

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

(declare-function avy-process "avy.el")
(declare-function mc/maybe-multiple-cursors-mode "multiple-cursors-core.el")
(declare-function mc/create-fake-cursor-at-point "multiple-cursors-core.el")

(defun combobulate-avy-jump ()
  "Use avy to jump to a navigable node"
  (interactive)
  ;; Is this right?
  (avy-process (mapcar (lambda (node) (cons (cons (combobulate-node-start node)
                                             (combobulate-node-end node))
                                       (selected-window)))
                       (combobulate--query-tree (combobulate--make-navigation-query)
                                                #'combobulate-node-visible-window-p))))

(defun combobulate-avy-jump-defun ()
  "Use avy to jump to a defun"
  (interactive)
  (with-navigation-nodes (:nodes combobulate-navigation-defun-nodes)
    (combobulate-avy-jump)))

(when (fboundp 'multiple-cursors-mode)
  (require 'multiple-cursors))

(defun combobulate--mc-edit-nodes (nodes &optional point-at-end)
  "Edit NODES with Multiple Cursors

Places point at the beginning of the first node unless
POINT-AT-END is non-nil."
  (if (fboundp 'multiple-cursors-mode)
      (let ((counter 0))
        (dolist (node-point (mapcar (if point-at-end
                                        #'combobulate-node-end
                                      #'combobulate-node-start)
                                    nodes))
          (cl-incf counter)
          (if (= counter 1)
              (goto-char node-point)
            (save-excursion
              (goto-char node-point)
              (mc/create-fake-cursor-at-point))))
        (dolist (ignored-command '(combobulate-edit-cluster-dwim
                                   combobulate-edit-node-type-dwim
                                   combobulate-edit-node-by-text-dwim))
          (add-to-list 'mc--default-cmds-to-run-once ignored-command))
        (mc/maybe-multiple-cursors-mode))
    (error "Multiple cursors is not installed")))

(provide 'combobulate-contrib)
;;; combobulate-contrib.el ends here
