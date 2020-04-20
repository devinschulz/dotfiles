(use-package lsp-mode
  :commands lsp
  :hook ((prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map ("C-c r" . lsp-rename))
  :config
  ;; Disable eldoc hover
  (setq lsp-eldoc-enable-hover nil)

  (require 'lsp-clients))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
