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

(use-package go-projectile
  :defer t
  :init
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")
    (go-guru-hl-identifier-mode)                    ; highlight identifiers
    (go-eldoc-setup)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (add-to-list 'company-backends 'company-go))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(provide 'init-projectile)
