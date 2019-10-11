;;; parse-it-lex.el --- Basic lexical analysis.  -*- lexical-binding: t; -*-

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
;; Basic lexical analysis
;;

;;; Code:

(require 'parse-it)


(defvar parse-it-lex--token-type
  '(("NEWLN" . "[\n]+")
    ("TAB" . "[\t]+")
    ("SPC" . "[ ]+"))
  "List of token identifier.")

(defcustom parse-it-lex-ignore-comment t
  "Ignore comment when tokenizing."
  :type 'boolean
  :group 'parse-it)

(defcustom parse-it-lex-ignore-spc-tab t
  "Ignore whitespaces/tabs when tokenizing."
  :type 'boolean
  :group 'parse-it)

(defvar parse-it-lex--buffer-string ""
  "Current tokenizing buffer string.")


(defun parse-it-lex--get-string-from-file (path)
  "Return PATH file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun parse-it-lex--get-string-from-buffer (buf-name)
  "Return BUF-NAME file content."
  (with-current-buffer buf-name
    (buffer-string)))

(defun parse-it-lex-tokenize-it (path)
  "Tokenize the PATH and return list of tokens."
  (setq parse-it-lex--buffer-string
        (if path (parse-it-lex--get-string-from-file path)
          (parse-it-lex--get-string-from-buffer (current-buffer))))
  (message "%s" parse-it-lex--buffer-string)
  )


(provide 'parse-it-lex)
;;; parse-it-lex.el ends here
