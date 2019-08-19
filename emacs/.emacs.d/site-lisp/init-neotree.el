(use-package all-the-icons :defer t)

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  :bind ("<f8>" . neotree-toggle))

(provide 'init-neotree)
