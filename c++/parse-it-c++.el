;;; parse-it-c++.el --- Core parser for C++.  -*- lexical-binding: t; -*-

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
;; Core parser for C++.
;;

;;; Code:

(require 'parse-it-c)


(defconst parse-it-c++--token-type
  '(("COLON" . "[:]")
    ("SEMICOLON" . "[;]")
    ("COMMA" . "[,]")
    ("DOT" . "[.]")
    ("QT_S" . "[']")
    ("QT_D" . "[\"]")
    ("ARROW" . "[=][>]")
    ("DECL" . "\\<\\(class\\|enum\\|interface\\namespace|\\|new\\|struct\\)")
    ("KEYWORD" . "\\<\\(public\\|partial\\|private\\|const\\|abstract\\|sealed\\|protected\\|ref\\|out\\|static\\|virtual\\|implicit\\|explicit\\|fixed\\|override\\|params\\|internal\\|async\\|extern\\|unsafe\\|is\\|as\\|operator\\|delegate\\|event\\|set\\|get\\|add\\|remove\\|var\\|do\\|else\\|try\\|finally\\|for\\|if\\|switch\\|while\\|catch\\|foreach\\|using\\|checked\\|unchecked\\|lock\\|return\\|continue\\|break\\|throw\\|goto\\|true\\|false\\|null\\|value\\|this\\|base\\|sizeof\\|typeof\\|yield\\|where\\|select\\|from\\|let\\|orderby\\|ascending\\|descending\\|await\\)"))
  "C++ token type.")


(defun parse-it-c++--make-token-type ()
  "Make up the token type."
  (append parse-it-c++--token-type
          parse-it-c--token-type
          parse-it-c--c-type-commenting-token-type
          parse-it-c--bracket-token-type
          parse-it-c--macro-token-type
          parse-it-c--c-type-arithmetic-operators-token-type
          parse-it-c--c-type-inc/dec-operators-token-type
          parse-it-c--c-type-assignment-operators-token-type
          parse-it-c--c-type-relational-operators-token-type
          parse-it-c--c-type-logical-operators-token-type
          parse-it-c--c-type-bitwise-operators-token-type
          parse-it-lex--token-type))

(defun parse-it-c++ (path)
  "Parse the PATH C++."
  (let* ((parse-it-lex--token-type (parse-it-c++--make-token-type))
         (token-list (parse-it-lex-tokenize-it path)))
    (parse-it-ast-build token-list
                        parse-it-c--into-level-symbols
                        parse-it-c--back-level-symbols)))


(provide 'parse-it-c++)
;;; parse-it-c++.el ends here
