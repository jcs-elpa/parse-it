;;; parse-it-typescript.el --- Core parser for TypeScript.  -*- lexical-binding: t; -*-

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
;; Core parser for TypeScript.
;;

;;; Code:

(require 'parse-it-c)


(defconst parse-it-typescript--token-type
  '(("COLON" . "[:]"))
  "TypeScript token type.")


(defun parse-it-typescript--make-token-type ()
  "Make up the token type."
  (append parse-it-c--c-type-commenting-token-type
          parse-it-typescript--token-type
          parse-it-lex--token-type))

(defun parse-it-typescript (path)
  "Parse the PATH TypeScript."
  (message "Parsing TypeScript..")
  (let ((parse-it-lex--token-type (parse-it-typescript--make-token-type)))
    (message "%s" (parse-it-lex-tokenize-it path))
    ))


(provide 'parse-it-typescript)
;;; parse-it-typescript.el ends here
