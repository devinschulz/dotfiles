(use-package go-mode
  :mode "\\.go\\'")

(use-package company-go
  :after (company go-mode))

(provide 'init-go-mode)
