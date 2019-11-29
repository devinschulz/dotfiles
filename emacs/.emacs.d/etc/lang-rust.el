(use-package rust-mode)

;; Provide a language server for Rust, and a company backend
(use-package racer
  :init
  (add-hook 'rust-mode #'racer-mode))
