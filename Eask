;; -*- mode: eask; lexical-binding: t -*-

(package "parse-it"
         "0.2.1"
         "Basic Parser in Emacs Lisp")

(website-url "https://github.com/jcs-elpa/parse-it")
(keywords "convenience" "parse" "parser" "lex" "lexer" "ast")

(package-file "parse-it.el")
(files "*.el" "langs/*.el")

(load-paths "./" "./langs/")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "melpa")

(depends-on "emacs" "25.1")
(depends-on "s")

(development
 (depends-on "f"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
