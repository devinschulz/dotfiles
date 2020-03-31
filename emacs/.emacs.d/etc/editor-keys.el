;; Don't suggest shorter ways to type commands in M-x, since they
;; don't apply when using Selectrum.
(setq suggest-key-bindings nil)

;; Join the next line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Jump to function at point
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)

;; Allow mouse interactions
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))

;; Remove all trailing whitespaces before saving the buffer.
(global-set-key (kbd "C-x s") '(lambda ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer)))
