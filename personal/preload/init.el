(setq prelude-theme 'monokai)
(setq helm-dash-browser-func 'eww)


(setq helm-dash-common-docsets '("Go" "Clojure" "Emacs_Lisp") )

(when (require 'yasnippet nil 'noerror)
  (progn
    (yas/load-directory "~/.emacs.d/snippets/clojure-mode")))

(server-start)
