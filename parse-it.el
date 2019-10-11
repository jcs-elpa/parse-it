;;; parse-it.el --- Basic Parser in Emacs Lisp.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-10-10 11:50:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Basic Parser in Emacs Lisp.
;; Keyword: parse parser lex lexer ast
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs090218/parse-it

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
;; Basic Parser in Emacs Lisp.
;;

;;; Code:


(defgroup parse-it nil
  "Basic Parser in Emacs Lisp."
  :prefix "parse-it-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/parse-it"))


(defun parse-it (lan &optional buf-name)
  "Parse the BUF-NAME with symbol language LAN support."
  (cl-case lan
    ('csharp (require 'parse-it-csharp) (parse-it-csharp buf-name))
    ('typescript (require 'parse-it-typescript) (parse-it-typescript buf-name))
    (t (user-error "Language '%s' is not supported" lan))))


(provide 'parse-it)
;;; parse-it.el ends here
