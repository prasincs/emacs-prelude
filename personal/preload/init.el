(setq prelude-theme 'monokai)
(setq helm-dash-browser-func 'eww)

(defun go-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Go")))

(add-hook 'go-mode-hook 'go-doc)
(server-start)
