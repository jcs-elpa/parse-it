(source "melpa")

(package-file "parse-it.el")

(files "*.el" "langs/*.el")

(load-paths "./" "./langs/")

(depends-on "emacs" "25.1")
(depends-on "s")

(development
 (depends-on "f"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-command-compile-hook
           (lambda () (setq byte-compile-error-on-warn t)))
