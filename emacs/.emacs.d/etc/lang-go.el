(use-package go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
         ("C-c C-n" . go-run)
         ("C-c ."   . go-test-current-test)
         ("C-c f"   . go-test-current-file)
         ("C-c a"   . go-test-current-project))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest)
  (use-package go-tag
    :config (setq go-tag-args (list "-transform" "camelcase"))))
