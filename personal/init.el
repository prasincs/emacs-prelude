;;;  personal/init.el --- this is my package

;;; Commentary:

;; Something something.

;;; Code:
(add-to-list 'auto-mode-alist '("\\.clj\.[a-zA-Z0-9.]+\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\.[a-zA-Z0-9.]+\\'" . clojure-mode))

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



;; magit
(global-set-key (kbd "s-i") 'magit-status)

(global-set-key (kbd "s-u") 'revert-buffer)

(setq prelude-guru nil)

;;; Fix scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(add-hook 'python-mode 'run-python)

(server-start)
;;; init.el ends here
