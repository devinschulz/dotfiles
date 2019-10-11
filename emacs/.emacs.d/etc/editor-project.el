;; Globally enable save place mode
(save-place-mode 1)

(use-package diff-hl
  :defer t
  :init
  (global-diff-hl-mode)

  ;; Highlight changed files in the fringe of Dired
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (diff-hl-margin-mode))

  ;; Refresh diff-hl after Magit operations
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package magit
  :config (define-key magit-file-mode-map (kbd "C-x g") nil)
  :bind
  ("C-x g g" . magit-status)
  ("C-x g f" . magit-find-file)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))

(use-package gist :defer t)

(use-package github-pullrequest :defer t)

(use-package undo-tree
  :defer t
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package projectile
  :init
  (setq projectile-dynamic-mode-line nil
        projectile-enable-caching t
        projectile-mode-line-prefix "")
  (projectile-mode)
  :config
  (setq projectile-sort-order 'recentf)
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
