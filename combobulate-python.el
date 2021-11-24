;;; combobulate-python.el --- python-specific features for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
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

(defun combobulate-setup-python ()
  (setq combobulate-manipulation-node-cluster-queries
        '((dictionary . (pair \. (_) @match))
          (tuple . (tuple ((_) @match)))
          (set . (set ((_) @match)))
          (list . (list ((_) @match)))
          (function_definition . (function_definition (identifier) (parameters ((_) @match))))))
  (setq combobulate-navigation-node-types '(module
                                            class_definition
                                            decorated_definition
                                            function_definition
                                            while_statement
                                            with_statement
                                            for_statement
                                            dictionary
                                            except_clause
                                            finally_clause
                                            for_in_clause
                                            finally_clause
                                            elif_clause
                                            if_statement
                                            try_statement
                                            list
                                            tuple
                                            dictionary
                                            set)))

(provide 'combobulate-python)
;;; combobulate-python.el ends here
