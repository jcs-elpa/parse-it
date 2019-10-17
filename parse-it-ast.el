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

(require 'parse-it-util)
(require 'parse-it-lex)


(defconst parse-it-ast-magic-root "ROOT"
  "Magic string represent the root of the tree.")


(defun parse-it-ast--form-node (type val pos &optional child)
  "Form a node for AST."
  (unless child (setq child '()))
  (list (cons :node-type type) (cons :value val) (cons :position pos) (cons :children child)))

(defun parse-it-ast--form-root-ast ()
  "Create the root of AST, basically the container of the source file."
  (parse-it-ast--form-node parse-it-ast-magic-root "" 1 '()))

(defun parse-it-ast--add-node (parent-node new-node)
  "Add NEW-NODE to PARENT-NODE."
  (let* ((plist-children (nth 3 parent-node))
         (children-list (if (cdr plist-children) (cdr plist-children) '())))
    (setq children-list (parse-it-util--add-to-list children-list new-node))
    (setcdr plist-children children-list)
    plist-children))

(defun parse-it-ast-build (token-list in-ss bk-ss)
  "Build an AST by using TOKEN-LIST.
IN-SS are list of symbols that recognized as into level.
BK-SS are list of symbols that recognized as back level."
  (let* ((ast-tree (parse-it-ast--form-root-ast))
         (parent-node ast-tree)
         (parent-node-stack '())
         (token-type nil) (token-val nil) (token-pos nil))
    (dolist (token token-list)
      (setq token-type (plist-get token :type))
      (setq token-val (plist-get token :value))
      (setq token-pos (plist-get token :pos))
      (let* ((into-lvl (parse-it-util--is-contain-list-string in-ss token-type))
             (back-lvl (parse-it-util--is-contain-list-string bk-ss token-type))
             (new-node (parse-it-ast--form-node token-type token-val token-pos)))
        (cond (into-lvl  ; Push stack.
               (parse-it-ast--add-node parent-node new-node)
               (push parent-node parent-node-stack)
               (setq parent-node new-node)
               )
              (back-lvl  ; Pop stack.
               (setq parent-node (nth 0 parent-node-stack))
               (setq parent-node-stack (parse-it-util--remove-nth-element 0 parent-node-stack))
               (parse-it-ast--add-node parent-node new-node)
               )
              (t
               (parse-it-ast--add-node parent-node new-node)))))
    ast-tree))


(provide 'parse-it-ast)
;;; parse-it-ast.el ends here
