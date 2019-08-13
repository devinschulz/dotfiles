;; Show line numbers all the time
(global-linum-mode 1)

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(provide 'appearance)
