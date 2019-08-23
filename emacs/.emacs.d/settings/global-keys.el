
;; Join the next line
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; Always go to the right indentation on the next line
(global-set-key (kbd "RET") 'newline-and-indent)

;; Jump to function at point
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)

(provide 'global-keys)

