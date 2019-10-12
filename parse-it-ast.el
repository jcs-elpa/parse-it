;;; parse-it-ast.el --- Core to build Abstract Syntax Tree (AST).  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh <jcs090218@gmail.com>

;; This file is NOT part of GNU Emacs.

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
;; Core to build Abstract Syntax Tree (AST).
;;

;;; Code:

(require 'parse-it-lex)


(defun parse-it-ast-build (token-list in-ss bk-ss)
  "Build an AST by using TOKEN-LIST.
IN-SS are list of symbols that recognize"
  (message "%s" token-list)

  )


(provide 'parse-it-ast)
;;; parse-it-ast.el ends here
