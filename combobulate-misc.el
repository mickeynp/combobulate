;;; combobulate-misc.el --- misc stuff for combobulate  -*- lexical-binding: t; -*-

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

(require 'subr-x)
(require 'combobulate-settings)
(require 'combobulate-interface)
(declare-function combobulate-node-start "combobulate-navigation")
(declare-function combobulate-node-end "combobulate-navigation")
(declare-function combobulate-pretty-print-node "combobulate-navigation")
(declare-function combobulate-draw-node-tree "combobulate-display")
(declare-function combobulate-node-visible-window-p "combobulate-navigation")
(declare-function combobulate-node-p "combobulate-navigation")
(declare-function combobulate-display-draw-node-tree "combobulate-display")


(defconst combobulate-sigil (propertize "©" 'face 'font-lock-keyword-face))

(defmacro combobulate-alist-append (key value alist)
  "Append to KEY (or create) KEY with VALUE in ALIST."
  `(map-put ,alist ,key (cons ,value (map-elt ,alist ,key))))

(defmacro combobulate-alist-set (key value alist)
  "Append to KEY (or create) KEY with VALUE in ALIST."
  `(map-put ,alist ,key (cons ,value nil)))

(defun combobulate-message (message &rest args)
  "Display MESSAGE and pretty print NODE"
  (message (concat
            combobulate-sigil " " message " "
            (mapconcat
             (lambda (a)
               (cond ((or (combobulate-proxy-node-p a) (combobulate-node-p a))
                      (propertize (format "%s" (combobulate-pretty-print-node
                                                (if (consp a)
                                                    (car a)
                                                  a)))
                                  'face 'combobulate-tree-highlighted-node-face))
                     ((stringp a) a)
                     (t "")))
             args " "))))

(defun combobulate-string-truncate (s max-length)
  "Truncate S to MAX-LENGTH and append ellipses."
  (let* ((len (length s)))
    (if (> len max-length)
        (concat (string-limit s max-length) " … ")
      s)))


(defun combobulate-count-lines-ahead (&optional pt)
  "Count the number of lines ahead of point."
  (save-excursion
    (goto-char (or pt (point)))
    (combobulate-string-count
     "\n"
     (buffer-substring-no-properties
      (or pt (point))
      (save-excursion
        (skip-chars-forward "[:space:]\n")
        (point))))))

(defun combobulate-string-count (regexp string)
  "Count the number of REGEXP in STRING.

No effort is made to account for, or exclude, overlaps."
  (let ((ct 0) (offset 0))
    (while (setq offset (string-match regexp string offset))
      (cl-incf ct)
      (cl-incf offset))
    ct))

(defun combobulate--flash-node (node)
  "Flashes NODE on the screen."
  (when (and node combobulate-flash-node)
    (message "%s" (combobulate-display-draw-node-tree node))))

(defsubst combobulate-debug (s &rest args)
  (princ (apply #'format (concat s "\n") args)))

(defun combobulate-display-indicator (current-level total)
  "Indicate CURRENT-LEVEL within a TOTAL number of pips."
  (concat
   "("
   (mapconcat
    (lambda (level)
      (let ((current (= level current-level)))
        (propertize (if current
                        (substring combobulate-indentation-marker 1 2)
                      (substring combobulate-indentation-marker 0 1))
                    'face (if current
                              'combobulate-active-indicator-face
                            'combobulate-dimmed-indicator-face))))
    (number-sequence 0 (1- total)) " ")
   ")"))

(defun combobulate-pulse-node (node &optional wait-time)
  "Pulses NODE for WAIT-TIME."
  (when combobulate-pulse-node
    (save-excursion
      (goto-char (combobulate-node-start node))
      (unless (combobulate-node-visible-window-p node nil t)
        (recenter))
      (unwind-protect
          (let ((ov (make-overlay (combobulate-node-start node)
                                  (combobulate-node-end node))))
            (overlay-put ov 'face 'combobulate-tree-pulse-node-face)
            (sit-for (or wait-time combobulate-pulse-node-wait-time))
            (delete-overlay ov))))))


(provide 'combobulate-misc)
;;; combobulate-misc.el ends here
