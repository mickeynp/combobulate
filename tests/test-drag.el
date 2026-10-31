;;; test-drag.el --- drag tests for Combobulate     -*- lexical-binding: t; -*-

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

(ert-deftest combobulate-test-drag-down-from-inside-a-sibling-drags-the-sibling ()
  :tags '(combobulate)
  (with-temp-buffer
    (insert "x = {\"a\": 1, \"b\": 2}\n")
    (python-ts-mode)
    (combobulate-mode)
    (goto-char (point-min))
    (search-forward "1,")
    (goto-char (match-beginning 0))
    (combobulate-drag-down)
    (should (equal (buffer-string) "x = {\"b\": 2, \"a\": 1}\n"))))

(provide 'test-drag)
;;; test-drag.el ends here
