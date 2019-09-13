(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  (lsp-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-reponse-timeout 10)

  ;; Go client
  (lsp-clients-go-server-args '("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports"))
  :bind
  (:map lsp-mode-map
        ("C-c r" . lsp-rename))
  :config
  (require 'lsp-clients)
  )

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t))

(use-package lsp-treemacs :defer t)
