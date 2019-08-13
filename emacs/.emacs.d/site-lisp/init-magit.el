(use-package magit
  :config (define-key magit-file-mode-map (kbd "C-x g") nil)
  :bind
  ("C-x g g" . magit-status)
  ("C-x g f" . magit-find-file)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))
(provide 'init-magit)
