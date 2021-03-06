(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package tagedit)

;; Major mode for editing web templates
(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'" "\\.tmp\\'" "\\.tmpl\\'")
  :defines company-backends
  :config
  ;; Autocomplete </ instantly
  (setq web-mode-enable-auto-closing t)

  ;; Indent by 2 spaces by default
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
