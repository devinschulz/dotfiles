(use-package diff-hl
  :ensure t
  :diminish diff-hl-mode
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode))

(use-package saveplace
  :ensure nil
  :defer 5
  :config
  (setq save-place t))

(use-package magit
  :config (define-key magit-file-mode-map (kbd "C-x g") nil)
  :bind
  ("C-x g g" . magit-status)
  ("C-x g f" . magit-find-file)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))

(use-package gist :defer t)
(use-package undo-tree :defer t)

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
