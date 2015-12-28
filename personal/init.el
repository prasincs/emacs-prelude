;;;  personal/init.el --- this is my package

;;; Commentary:

;; Something something.

;;; Code:
(add-to-list 'auto-mode-alist '("\\.clj\.[a-zA-Z0-9.]+\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\.[a-zA-Z0-9.]+\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))


(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Kill all other buffers except the current one
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-cider-repls ()
  "Kill all cider repls"
  (interactive)
  (kill-matching-buffers ".*cider-repl.*"))

(defun hide-cider-error-window ()
  "Hides the *cider-error* window"
  (interactive)
  (let ((target-window (get-buffer-window "*cider-error*")))
    (when target-window
      (delete-window target-window))))

(global-set-key (kbd "C-c e") 'hide-cider-error-window)

(defun open-notes ()
  "Opens the note file"
  (find-file (abbreviate-file-name
                       (expand-file-name "~/dotfiles/Emacs.org"))))

(global-set-key (kbd "C-n") 'open-notes)

;; Taken from technomancy's emacs.d
(global-set-key (kbd "C-c n")
                (defun pnh-cleanup-buffer () (interactive)
                       (delete-trailing-whitespace)
                       (untabify (point-min) (point-max))
                       (indent-region (point-min) (point-max))))

;; cider REPL
;; wrap around
(setq cider-repl-wrap-history t)

(setq cider-repl-history-size 1000) ; the default is 500

(setq cider-repl-history-file "~/.emacs.d/cider-history")`

;; magit
(global-set-key (kbd "s-i") 'magit-status)

(global-set-key (kbd "s-u") 'revert-buffer)

;; Use tab to complete without losing ability to manually indent
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)


(setq prelude-guru nil)

;;; Fix scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(add-hook 'python-mode 'run-python)

(global-wakatime-mode)

;; Home and End jumping around buffer is fucking annoying
(global-set-key (kbd "<home>")
                'beginning-of-line)

(global-set-key (kbd "<end>")
                'end-of-line)

(global-set-key (kbd "C-c p w")
                'helm-swoop)

;; Pressing C-z seems to crash Emacs in OSX every once in a while.
;; I dont like the behavior anyway.
(global-unset-key (kbd "C-z"))


;; Paredit hooks
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'json-mode 'flymake-json-load)

(require 'json-snatcher)
(defun js-mode-bindings ()
  "Sets a hotkey for using the json-snatcher plugin."
  (when (string-match  "\\.json$" (buffer-name))
    (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)
(add-hook 'json-mode 'js-mode-bindings)

;; Fix conflicts (http://orgmode.org/org.html#Conflicts)

;; windmove compatibility
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Requires coreutils to be installed in mac
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))


;; Have to install https://github.com/golang/lint for this to work
(add-to-list 'load-path (concat (exec-path-from-shell-getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;; Go lang defaults
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-w") 'go-goto-imports)))



(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(require 'org-crypt)
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)


;; To enable org-table and markdown mode to play nicely
(require 'org-table)

(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))
    ))

(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          (lambda()
            (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))

;; end org-table

; (setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;(persp-mode)

(require 'dot-mode)

(server-start)
;;; init.el ends here
