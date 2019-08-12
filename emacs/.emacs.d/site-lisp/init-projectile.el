;; projectile
(use-package projectile
  :init
  (setq projectile-dynamic-mode-line nil
        projectile-enable-caching t
        projectile-mode-line-prefix "")
  (projectile-mode)
  :config
  (setq projectile-project-compilation-cmd "make ")
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

;; counsel-projectile
(use-package counsel-projectile
  :after (projectile)
  :init (counsel-projectile-mode))

(provide 'init-projectile)
