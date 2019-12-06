;; Globally enable save place mode
(save-place-mode 1)

;; Write backup ffiles to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :init
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode...")))

(use-package magit-todos
  :straight t)

(use-package git-commit
  :config
  (setq git-commit-summary-max-length 50))

;; Package `git-link` provides a simple function M-x git-link which
;; copies to the kill ring a link to the current line of code or
;; selection on Github
(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package diff-hl
  :after magit

  ;; Highlight changed files in the fringe of Dired
  :hook (dired-mode . diff-hl-dired-mode)

  ;; Refresh diff-hl after Magit operations
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (diff-hl-margin-mode))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("M-/" . undo-tree-redo))
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-enable-undo-in-region nil))

(use-package projectile
  :config
  (setq projectile-dynamic-mode-line nil
        projectile-enable-caching t
        projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-project-compilation-cmd "make "
        projectile-completion-system 'ivy)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package counsel-projectile
  :config
  ;; Sort files using `prescient', instead of showing them in a
  ;; lexicographic order.
  (setq counsel-projectile-sort-files t)

  :init
  (counsel-projectile-mode +1))

(use-package go-projectile
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")
    (go-guru-hl-identifier-mode)                    ; highlight identifiers
    (go-eldoc-setup)
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-,") 'pop-tag-mark)
    (add-to-list 'company-backends 'company-go))
  (add-hook 'go-mode-hook 'my-go-mode-hook))
