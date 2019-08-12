;; Show line numbers all the time
(global-linum-mode 1)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(provide 'appearance)
