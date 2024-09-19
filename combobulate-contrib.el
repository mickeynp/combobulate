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
(require 'combobulate-setup)
(defvar mc--default-cmds-to-run-once)
(declare-function avy-process "avy.el")
(declare-function mc/remove-fake-cursors "multiple-cursors-core.el")
(declare-function mc/maybe-multiple-cursors-mode "multiple-cursors-core.el")
(declare-function mc/create-fake-cursor-at-point "multiple-cursors-core.el")
(declare-function combobulate-tally-nodes "combobulate-manipulation.el")
(declare-function combobulate-procedure-collect-activation-nodes "combobulate-navigation")

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
  (with-navigation-nodes (:procedures (combobulate-read procedures-defun))
    (combobulate-avy-jump)))



(provide 'combobulate-contrib)
;;; combobulate-contrib.el ends here


